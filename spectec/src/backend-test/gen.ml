open Langs
open Utils
open Prune

open Util.Source
open Util
open Backend_interpreter
open Al.Al_util

(** Helpers **)
let (%>) f g v = f v |> g

let flatten_rec =
  List.concat_map (fun def ->
    match def.it with
    | Il.Ast.RecD defs -> defs
    | _ -> [ def ]
  )

let string_of_module m = match m with
| Al.Ast.CaseV ("MODULE", args) ->
  "(MODULE\n  " ^ (List.map Al.Print.string_of_value args |> String.concat "\n  ") ^ "\n)"
| _ -> failwith "Unreachable"

(** Mutation **)
let patch m =
  try
    Patch.patch_module m
  with e ->
    prerr_endline (Printexc.to_string e); m
  (* TODO *)

(** Injection **)
type invoke = string * Al.Ast.value list
type invoke_result = (Al.Ast.value list, exn) result
type assertion = invoke * invoke_result
type instant_result = (assertion list, exn) result

let mk_assertion funcinst =
  let name = strv_access "NAME" funcinst |> unwrap_textv in
  let addr = strv_access "ADDR" funcinst |> casev_nth_arg 0 in
  let arg_types =
    Ds.Store.access "FUNCS"
    |> unwrap_listv_to_list
    |> (fun l -> List.nth l (unwrap_numv_to_int addr))
    |> strv_access "TYPE"
    |> casev_get_args
    |> (fun l -> List.nth l 0)
    |> unwrap_listv_to_list
  in
  let args =
    List.map (function
      | Al.Ast.CaseV ("I32", []) as t -> caseV ("CONST", [t; numV (gen_bytes 4)])
      | Al.Ast.CaseV ("F32", []) as t -> caseV ("CONST", [t; Construct.(al_of_floatN layout32) (gen_bytes 4)])
      | Al.Ast.CaseV ("I64", []) as t -> caseV ("CONST", [t; numV (gen_bytes 8)])
      | Al.Ast.CaseV ("F64", []) as t -> caseV ("CONST", [t; Construct.(al_of_floatN layout64) (gen_bytes 8)])
      | Al.Ast.CaseV ("V128", []) as t -> caseV ("VCONST", [t; numV (gen_bytes 16)])
      | t -> (* Assumpnion: is ref *) caseV ("REF.NULL", [t])
    ) arg_types
  in
  let invoke = name, args in

  let store_bak = Ds.Store.get () |> copy_value in
  try
    let returns = Interpreter.invoke [ addr; listV_of_list args ] in
    invoke, Ok (unwrap_listv_to_list returns)
  with e ->
    if e = Exception.Exhaustion then
      store_bak |> Ds.Store.set;
    invoke, Error e

let print_assertion ((f, args), result) =
  Log.trace (match result with
  | Ok returns -> Printf.sprintf "(assert_return (invoke %s [%s]) [%s])"
    f
    (args |> List.map Al.Print.string_of_value |> String.concat " ")
    (returns |> List.map Al.Print.string_of_value |> String.concat " ")
  | Error Exception.Trap -> Printf.sprintf "(assert_trap (invoke %s [%s]))"
    f
    (args |> List.map Al.Print.string_of_value |> String.concat " ")
  | Error e -> Printf.sprintf "(invoke %s [%s]) failed due to %s"
    f
    (args |> List.map Al.Print.string_of_value |> String.concat " ")
    (Printexc.to_string e))

let get_instant_result m : instant_result =
  try
    let externvals = listV_of_list (
      []
      (* @ List.init 1 (fun i -> Al.Ast.CaseV ("FUNC", [numV_of_int i])) *)
      (* @ List.init 4 (fun i -> Al.Ast.CaseV ("GLOBAL", [numV_of_int i])) *)
      (* @ List.init 1 (fun i -> Al.Ast.CaseV ("TABLE", [numV_of_int i])) *)
      (* @ List.init 1 (fun i -> Al.Ast.CaseV ("MEM", [numV_of_int i])) *)
    ) in (*TODO *)
    let mm = Interpreter.instantiate [ m; externvals ] in
    let exported_funcs =
      mm
      |> strv_access "EXPORTS"
      |> unwrap_listv_to_list
      |> List.filter (fun inst -> inst |> strv_access "ADDR" |> casev_get_case = "FUNC")
    in
    Ok (List.map mk_assertion exported_funcs)
  with e -> Error e
let inject m =
  get_instant_result m

type module_ = Al.Ast.value
type test = module_ * instant_result

(** Output **)

let print_module module_ =
  Log.trace (string_of_module module_)
let print_result result =
  let print = Log.trace in
  (match result with
  | Ok assertions ->
    print "Instantiation success";
    List.iter print_assertion assertions
  | Error Exception.Trap -> print ("Instantiation trapped")
  | Error Exception.Exhaustion -> print ("Infinite loop in instantiation")
  | Error e -> print("Unexpected error during instantiation: " ^ Printexc.to_string e)
  );
  print "================"

let to_phrase x = Reference_interpreter.Source.(x @@ no_region)

let value_to_wast v =
  let open Reference_interpreter in
  let open Script in
  let open Value in

  let f32_pos_nan = F32.to_bits F32.pos_nan in
  let f32_neg_nan = F32.to_bits F32.neg_nan |> Int32.logand 0x0000_0000_ffff_ffffl in
  let f64_pos_nan = F64.to_bits F64.pos_nan in
  let f64_neg_nan = F64.to_bits F64.neg_nan in

  match v with
  | Al.Ast.CaseV ("REF.FUNC_ADDR", _) -> RefResult (RefTypePat FuncHT) |> to_phrase
  | _ ->
    match Construct.al_to_value v with
    | Num n -> NumResult (NumPat (n |> to_phrase)) |> to_phrase
    | Ref r -> RefResult (RefPat (r |> to_phrase)) |> to_phrase
    (* TODO: Check implementattion *)
    | Vec (V128 i) ->
      let i32 i = NumPat (to_phrase (I32 i)) in
      let i64 i = NumPat (to_phrase (I64 i)) in
      let f32 f =
        if f32_pos_nan = (F32.to_bits f) || f32_neg_nan = (F32.to_bits f) then
          NanPat (to_phrase (F32 CanonicalNan))
        else if Int32.logand (F32.to_bits f) f32_pos_nan = f32_pos_nan then
          NanPat (to_phrase (F32 ArithmeticNan))
        else
          NumPat (to_phrase (F32 f))
      in
      let f64 f =
        if f64_pos_nan = (F64.to_bits f) || f64_neg_nan = (F64.to_bits f) then
          NanPat (to_phrase (F64 CanonicalNan))
        else if Int64.logand (F64.to_bits f) f64_pos_nan = f64_pos_nan then
          NanPat (to_phrase (F64 ArithmeticNan))
        else
          NumPat (to_phrase (F64 f))
      in
      match choose [ "I8"; "I16"; "I32"; "I64"; "F32"; "F64" ] with
      | "I8" ->  to_phrase (VecResult (VecPat (V128 (V128.I8x16 (), List.map i32 (V128.I8x16.to_lanes i)))))
      | "I16" -> to_phrase (VecResult (VecPat (V128 (V128.I16x8 (), List.map i32 (V128.I16x8.to_lanes i)))))
      | "I32" -> to_phrase (VecResult (VecPat (V128 (V128.I32x4 (), List.map i32 (V128.I32x4.to_lanes i)))))
      | "I64" -> to_phrase (VecResult (VecPat (V128 (V128.I64x2 (), List.map i64 (V128.I64x2.to_lanes i)))))
      | "F32" -> to_phrase (VecResult (VecPat (V128 (V128.F32x4 (), List.map f32 (V128.F32x4.to_lanes i)))))
      | "F64" -> to_phrase (VecResult (VecPat (V128 (V128.F64x2 (), List.map f64 (V128.F64x2.to_lanes i)))))
      | _ -> failwith "hi"

let invoke_to_wast ((f, args), result) =
  let open Reference_interpreter.Script in
  let f' = Reference_interpreter.Utf8.decode f in
  let args' = List.map (Construct.al_to_value %> to_phrase) args in
  let action = Invoke (None, f', args') |> to_phrase in
  match result with
  | Ok returns -> Some (AssertReturn (action, List.map value_to_wast returns))
  | Error Exception.Exhaustion -> Some (AssertExhaustion (action, ""))
  | Error Exception.Trap -> Some (AssertTrap (action, ""))
  | Error e ->
    Printf.sprintf "Unexpected error in invoking %s: %s" f (Printexc.to_string e) |> prerr_endline;
    None

let to_wast seed m result =
  let open Reference_interpreter.Script in

  let global ty value =
    caseV ("GLOBAL", [ TupV ([ CaseV ("MUT", [ OptV None ]); nullary ty]);
      listV_of_list [CaseV ("CONST", [ nullary ty; value])]])
  in

  let export name var =
    caseV ("EXPORT", [ TextV name; CaseV ("GLOBAL", [ Construct.al_of_int32 var ])])
  in

  let m_spectest = ("MODULE", [
    empty_list; empty_list; empty_list;
    listV_of_list [
      global "I32" (Construct.al_of_int32 666l);
      global "I64" (Construct.al_of_int64 666L);
      global "F32" (0x4426a666l |> Z.of_int32_unsigned |> Construct.(al_of_floatN layout32));
      global "F64" (0x4084d4cccccccccdL |> Z.of_int64_unsigned |> Construct.(al_of_floatN layout64));
    ];
    empty_list; empty_list;
    ]@ (if !Flag.version = 3 then [empty_list] else []) @[
    empty_list; empty_list; OptV None;
    listV_of_list [
      export "global_i32" 0l;
      export "global_i64" 1l;
      export "global_f32" 2l;
      export "global_f64" 3l;
    ];
  ]) |> caseV |> Construct.al_to_module in

  let m_r = Construct.al_to_module m in

  let spectest = Textual (m_spectest, []) |> to_phrase in
  let def = Textual (m_r, []) |> to_phrase in
  let spectest_values = to_phrase "spectest_values" in
  let pre_script = [
    (Module (Some spectest_values, spectest) |> to_phrase);
    (Instance (Some spectest_values, Some spectest_values) |> to_phrase);
    (Register (Utf8.decode "spectest_values", Some spectest_values) |> to_phrase)
  ] in
  let script =
    pre_script
    @ [ Module (None, def) |> to_phrase ]
    @ match result with
      | Ok _ ->
        [ Instance (None, None) |> to_phrase ]
      | Error Exception.Trap ->
        [ Assertion (AssertUninstantiable (None, "") |> to_phrase) |> to_phrase ]
      | Error Exception.Exhaustion ->
        [ (* TODO: Exhaustion *) ]
      | Error e ->
        Printf.sprintf "Unexpected error in instantiating module: %s" (Printexc.to_string e) |> prerr_endline;
        []
  in

  let is_exhaustion = function
    | AssertExhaustion (_, _) -> true
    | _ -> false
  in

  let to_file name script =
    let dir = !Flag.out in
    if not (Sys.file_exists dir && Sys.is_directory dir) then
      Sys.mkdir dir 0o755;
    let file = Filename.concat dir (name ^ ".wast") in
    let oc = open_out file in
    Reference_interpreter.Print.script oc 80 `Textual script;
    close_out oc
  in

  match result with
  | Ok assertions ->
    let assertions = List.filter_map invoke_to_wast assertions in

    if List.exists is_exhaustion assertions then
      let assertions_returns = List.filter (is_exhaustion %> not) assertions in

      to_file (string_of_int seed) (script @ List.map (fun a -> Assertion (to_phrase a) |> to_phrase) assertions_returns);

      let _ = List.fold_left (fun (i, s) a ->
        if is_exhaustion a then
          let () = to_file (string_of_int seed ^ "-e" ^ string_of_int i) (script @ [Assertion (to_phrase a) |> to_phrase]) in
          (i + 1, s)
        else
          (i, s @ [Assertion (to_phrase a) |> to_phrase])
      ) (0, script) assertions in

      ()
    else
      to_file (string_of_int seed) (script @ List.map (fun a -> Assertion (to_phrase a) |> to_phrase) assertions)
  | Error Exception.Exhaustion ->
      to_file (string_of_int seed) pre_script;
      to_file (string_of_int seed ^ "-e") script
  | Error _ ->
      to_file (string_of_int seed) script
  (*
  try
    Reference_interpreter.Valid.check_module m_r;
    prerr_endline "Valid"
  with
    | e -> prerr_endline ("Invalid: " ^ Printexc.to_string e)
  *)

(* Generate tests *)

let gen_test el' il' al' =
  (* Register spec *)
  el := el';
  il := flatten_rec il';
  al := al';

  orig_il := !il;

  il_env := Il.Env.env_of_script il';

  (* Initialize *)
  Top_down.rts := List.map Top_down.get_rt (get_typing_rules ());
  Langs.estimate_const ();
  let st = Sys.time () in
  let times = ref [] in
  Printexc.register_printer (function
    | Bottom_up.UnifyFail (e1, e2) ->
      Some (Printf.sprintf "UnifyFail(%s, %s)" (Il.Print.string_of_exp e1) (Il.Print.string_of_exp e2))
    | _ -> None
  );

  List.init !Flag.n (fun i -> !Flag.seed + i)
  |> List.iter (fun seed -> try (
    (if seed mod 100 = 0 then Log.info else Log.debug) ("=== Generating " ^ string_of_int seed ^ ".wast... ===");

    (* Set random seed *)
    Random.init seed;

    (* Prune production grammar for swarm testing *)
    il := if !Flag.swarm then prune_il !orig_il else !orig_il;

    (* Generate test *)
    (* let module_ = Top_down.gen_module () *)
    let module_ = Bottom_up.gen_module [""] in

    (* Mutatiion *)
    let module_ = patch module_ in

    (* Initialize ds *)
    Ds.init !al;

    (* TODO *)
    Builtin.builtin () |> ignore;

    (* Print module *)
    print_module module_;

    (* Injection *)
    let result = inject module_ in

    (* Print result *)
    print_result result;

    (* Convert to Wast *)
    to_wast seed module_ result;

    (* Conform test *)
    Conform_test.conform_test seed;

    times := Sys.time () -. st :: !times;

    ) with | e -> print_endline @@ "FAIL: " ^ (string_of_int seed) ^ ".wast - " ^ Printexc.to_string e;
  );

  (* Print Coverage *)
  (* Ds.(Info.print (InfoMap.uncovered !info_map)) *)

  (* Print time *)
  let file = Filename.concat !Flag.out "time.txt" in
  let oc = open_out file in
  output_string oc (List.rev !times |> List.map string_of_float |> String.concat "\n");
  close_out oc

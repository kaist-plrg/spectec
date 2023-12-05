open Flag
open Util.Source
open Util.Record
open Backend_interpreter
open Al.Al_util

(* Specs *)
let el: El.Ast.script ref = ref []
let il: Il.Ast.script ref = ref []
let al: Al.Ast.script ref = ref []

type valtype =
| T of string
| RefT of valtype
| SubT of string * string
| TopT
| BotT
| SeqT
type restype = valtype list

(* Helpers *)
let choose l =
  let n = List.length l in
  assert (n > 0);
  let i = Random.int n in
  List.nth l i

let contains x = List.exists (fun x' -> x = x')

let hds xs = xs |> List.rev |> List.tl |> List.rev

let option_flatmap f opt = Option.bind opt f

let flatten_rec = List.concat_map (fun def ->
  match def.it with
  | Il.Ast.RecD defs -> defs
  | _ -> [ def ]
)

let find_syn name =
  let syn_opt = List.find_map (fun def -> match def.it with
  | Il.Ast.SynD (id, deftyp) when id.it = name -> Some deftyp
  | _ -> None ) !il in
  match syn_opt with
  | Some syn -> syn
  | None -> failwith (Printf.sprintf "The syntax named %s does not exist in the input spec" name)

let numV i = Al.Ast.NumV (i |> Int64.of_int)
let optV_none = Al.Ast.OptV None
let optV_some v = Al.Ast.OptV (Some v)

let string_of_atom = Il.Print.string_of_atom
let string_of_module m = match m with
| Al.Ast.CaseV ("MODULE", args) ->
  "(MODULE\n  " ^ (List.map Al.Print.string_of_value args |> String.concat "\n  ") ^ "\n)"
| _ -> failwith "Unreachable"

let push v s = s := v :: !s
let pop s = s := List.tl !s
let top s = List.hd !s

let correct_cvtop = Al.Ast.(function
| [ CaseV (t2, []); CaseV (cvtop, []); CaseV (t1, []); OptV (sx_opt) ] ->
  let x2 = String.sub t2 0 1 in
  let n2 = String.sub t2 1 2 in
  let x1 = String.sub t1 0 1 in
  let n1 = String.sub t1 1 2 in
  ( match (x2, n2, cvtop, x1, n1, sx_opt) with
  | "I", "32", "WRAP", "I", "64", None
  | "I", "64", "EXTEND", "I", "32", Some _
  | "I", _, "TRUNC", "F", _, Some _
  | "I", _, "TRUNC_SAT", "F", _, Some _
  | "F", "32", "DEMOTE", "F", "64", None
  | "F", "64", "PROMOTE", "F", "32", None
  | "F", _, "CONVERT", "I", _, Some _ -> true
  | _, _, "REINTERPRET", _, _, None -> x2 <> x1 && n2 = n1
  | _ -> false )
| _ -> false )

let valid case args const (rt1, rt2) = Al.Ast.(
  (* Bunch of hardcoded heuristics to check if the given Wasm instruction is valid *)
  match case with
  | "UNOP" | "BINOP" | "TESTOP" | "RELOP" -> ( match args, rt1 with
    | [ CaseV ("I32", []); CaseV ("_I", _) ], T "I32" :: _
    | [ CaseV ("I64", []); CaseV ("_I", _) ], T "I64" :: _
    | [ CaseV ("F32", []); CaseV ("_F", _) ], T "F32" :: _
    | [ CaseV ("F64", []); CaseV ("_F", _) ], T "F64" :: _ -> not const || rt2 = [ T "I32" ] || rt2 = [ T "I64" ]
    | _ -> false )
  | "EXTEND" -> ( match args, rt1 with
    | [ CaseV ("I32", []); _ ], [ T "I32" ]
    | [ CaseV ("I64", []); _ ], [ T "I64" ] -> true
    | _ -> false )
  | "CVTOP" -> correct_cvtop args && ( match args, rt1, rt2 with
    | [ CaseV (t2, []); _; CaseV (t1, []); _ ],  [ T t1' ], [ T t2' ] -> t1 = t1' && t2 = t2'
    | _ -> false )
  | "CONST" -> ( match args, rt2 with
    | [ CaseV ("I32", []); _ ], [ T "I32" ]
    | [ CaseV ("I64", []); _ ], [ T "I64" ]
    | [ CaseV ("F32", []); _ ], [ T "F32" ]
    | [ CaseV ("F64", []); _ ], [ T "F64" ] -> true
    | _ -> false )
  | "GLOBAL.GET" -> ( match args with
    (* builtin globals *)
    | [ NumV 0L ] -> rt2 = [ T "I32" ]
    | [ NumV 1L ] -> rt2 = [ T "I64" ]
    | [ NumV 2L ] -> rt2 = [ T "F32" ]
    | [ NumV 3L ] -> rt2 = [ T "F64" ]
    | _ -> true (* Over-approximate *) )
  | "CALL_REF" | "RETURN_CALL_REF" -> ( match args with
    | [ OptV (Some _) ] -> true
    | _ -> false )
  | _ -> true
)

(** Initialize **)

let rts = ref Record.empty

let rec string_of_vt = function
| T x -> x
| RefT t -> "REF " ^ string_of_vt t
| SubT (x, sub) -> x ^ "<:" ^ sub
| TopT -> "_"
| BotT -> "_"
| SeqT -> "_*"
let string_of_rt vts = List.map string_of_vt vts |> String.concat " "
let print_rts () =
  Record.iter (fun (k, v) ->
    let (rt1, rt2) = v in
    Printf.sprintf "%s : %s -> %s" k (string_of_rt rt1) (string_of_rt rt2) |> print_endline
  ) !rts

let estimate_rt () = Il.Ast.(
  let rec get_rt e = match e.it with
  | CaseE (atom, { it = TupE []; _}) -> [ T (string_of_atom atom) ]
  | ListE es -> List.concat_map get_rt es
  | VarE id -> [ SubT (id.it, Il.Print.string_of_typ e.note) ]
  | SubE ({ it = VarE id; _ }, { it = VarT id'; _ }, _)  -> [ SubT (id.it, id'.it) ]
  | IterE (_, (Il.Ast.List, _)) -> [ SeqT ]
  | CatE (e1, e2) -> get_rt e1 @ get_rt e2
  | CaseE (Atom "REF", { it = TupE [_; e']; _ }) -> get_rt e' |> List.map (fun t -> RefT t)
  | _ -> [ TopT ] in

  let expected e kind =
    Printf.sprintf "Expected %s to be %s" (Il.Print.string_of_exp e) kind |> failwith
  in

  List.iter (fun def -> match def.it with
    | RelD (id, _, _, rules) when id.it = "Instr_ok" || id.it = "Instrf_ok" ->
      List.iter (fun rule -> match rule.it with
        | RuleD (id, _, _, exp, _) -> ( match exp.it with
          | TupE [_c; _lhs; rhs] -> ( match rhs.it with
            | MixE (_, args) -> ( match args.it with
              | TupE [t1; t2] | TupE [t1; _; t2] ->
                let name = String.split_on_char '-' id.it |> List.hd |> String.uppercase_ascii in
                let rt1 = get_rt t1 in
                let rt2 = get_rt t2 in
                rts := Record.add name (rt1, rt2) !rts;
              | _ -> expected args "t1, t2 or t1, x*, t2" )
            | _ -> expected rhs "e1 -> e2" )
          | _ -> expected exp "C |- lhs : rhs" )
      ) rules
    | _ -> ()
  ) !il
)

let rt_stack = ref []
let update_rt rt1 rt2 =
  match !rt_stack with
  | [] -> failwith "rt_stack is empty"
  | (st, target) :: tl ->
    let st1 = List.fold_right (fun _ st -> List.tl st) rt1 st in
    let st2 = List.fold_left  (fun st t -> t :: st) st1 rt2 in
    rt_stack := (st2, target) :: tl

let rec matches vt1 vt2 = match vt1, vt2 with
| TopT, _ | _, TopT -> true
| _, BotT -> true
| T x1, T x2 -> x1 = x2
| RefT t1, RefT t2 -> matches t1 t2
| _ -> false

let rec matches_all vts1 vts2 = match vts1, vts2 with
| [], [] -> true
| hd1 :: tl1, hd2 :: tl2 -> matches_all tl1 tl2 && matches hd1 hd2
| _ -> false

let poppable rt =
  let rec aux l1 l2 = match l1, l2 with
  | [], _ -> true
  | hd1 :: tl1, hd2 :: tl2 -> matches hd1 hd2 && aux tl1 tl2
  | _ -> false in

  aux (List.rev rt) (top rt_stack |> fst)

let rec al_to_rt v = match v with
| Al.Ast.CaseV (x, []) -> T x
| Al.Ast.CaseV ("REF", [_; v']) -> al_to_rt v'
| _ -> BotT

let consts = ref []
let const_ctxs = ref []
let estimate_const () = Il.Ast.(
  List.iter (fun def -> match def.it with
    | RelD (id, _, _, rules) when id.it = "Instr_const" ->
      List.iter (fun rule -> match rule.it with
        | RuleD (id, _, _, _, _) -> push (String.uppercase_ascii id.it) consts
      ) rules
    | RelD (_, _, _, rules) ->
      List.iter (fun rule -> match rule.it with
        | RuleD (_, _, _, args, prems) when
          let rec is_const_checking prem = match prem.it with
          | RulePr (id, _, _) -> id.it = "Expr_ok_const"
          | IterPr (prem', _) -> is_const_checking prem'
          | _ -> false in
          List.exists is_const_checking prems -> ( match args.it with
            | TupE [_; e; _] | TupE [_; e] -> ( match e.it with
              | CaseE (atom, _) | MixE ([atom] :: _, _) ->
                push (atom |> string_of_atom |> String.uppercase_ascii) const_ctxs
              | _ -> () )
            | _ -> () )
        | _ -> () ) rules
    | _ -> ()
  ) !il
)

let print_consts () = List.iter (fun i -> print_endline i) !consts

(** Seed generation **)

let case_stack = ref []

let types_cache = ref []
let typeidx_cache = ref (numV 0)
let cache_if cond ref v = if cond then ref := v; v

let estimate_out_dim tid types = try (
  al_to_int tid
  |> List.nth types
  |> arg_of_case "TYPE" 0
  |> arg_of_tup 1 (* Wasm 2.0 *)
  |> al_to_list
  |> List.map (function
    (* | Al.Ast.CaseV (t, []) -> T t *)
    | _ -> TopT )
  |> Option.some
) with _ -> None

(* Generate specific syntax from input IL *)
let rec gen name =
  let syn = find_syn name in

  (* HARDCODE: Wasm expression *)
  if name = "expr" then gen_wasm_expr [] ( match top case_stack with
    | "FUNC" -> estimate_out_dim !typeidx_cache !types_cache
    | "ACTIVE" -> Some [T "I32"]
    | _ -> Some [TopT] ) else
  (* HARDCODE: name *)
  if name = "name" then (Al.Ast.TextV (choose ["a"; "b"; "c"] ^ choose ["1"; "2"; "3"])) else
  (* HARDCODE: memidx to be always 0 *)
  if name = "memidx" then numV 0 else
  (* HARDCODE: pack_size to be 8/16/32/64 *)
  if !case_stack > [] && contains (top case_stack)  [ "LOAD"; "STORE"; "EXTEND" ] && name = "n" then
    numV (choose [8; 16; 32; 64])
  else

  match syn.it with
  | AliasT typ -> gen_typ typ
  (* TupV *)
  | NotationT ([[]; []; []], typs)
  | NotationT ([[LBrack]; [Dot2]; [RBrack]], typs) ->
    let pair = gen_typs typs in
    let fst = List.hd pair in
    let snd = List.hd (List.tl pair) in
    Al.Ast.TupV [ fst; snd ]
  (* ArrowV *)
  | NotationT ([[]; [Arrow]; []], typs)
  | NotationT ([[]; [Star; Arrow]; [Star]], typs) ->
    let pair = gen_typs typs in
    let fst = List.hd pair in
    let snd = List.hd (List.tl pair) in
    Al.Ast.ArrowV (fst, snd)
  (* CaseV *)
  | NotationT ((atom :: _) :: _, typs)
  | NotationT ([[]; [atom]], typs) (* Hack for I8 *) ->
    let case = string_of_atom atom in
    push case case_stack;
    let args = gen_typs typs in
    pop case_stack;
    Al.Ast.CaseV (case, args)
  (* StrV *)
  | StructT typfields ->
    let rec_ = List.fold_right (fun typefield ->
      let (atom, (_, typ, _), _) = typefield in
      let key = string_of_atom atom in
      let value = gen_typ typ in
      Record.add key value
    ) typfields Record.empty in
    Al.Ast.StrV rec_
  (* CaseV *)
  (* HARDCODE: Wasm instruction *)
  | VariantT typcases when name = "instr" ->
    (* HARDCODE: checks if currently in a context that requires const instrution *)
    let const_required = (contains (top case_stack) !const_ctxs) in
    let typcases' = if not const_required then typcases else
      let is_const (atom, _, _) = contains (string_of_atom atom) !consts in
      List.filter is_const typcases
    in
    let rec try_instr life =
      let typcase = choose typcases' in
      let (atom, (_, typs, _), _) = typcase in
      let case = string_of_atom atom in
      let (t1, t2) = !rts |> Record.find case in
      let rt1, rt2 = fix_rts t1 t2 in
      if not (poppable rt1) && life > 0 then
        try_instr (life - 1)
      else
        let rec try_args life' =
          if life' = 0 && life > 0 then try_instr (life - 1) else (
          push case case_stack;
          let args = match case with
          | "BLOCK"
          | "LOOP" -> [ gen "blocktype"; gen_wasm_expr rt1 (Some rt2) ]
          | "IF" -> [ gen "blocktype"; gen_wasm_expr (hds rt1) (Some rt2); gen_wasm_expr (hds rt1) (Some rt2) ]
          | _ -> gen_typs typs in
          pop case_stack;
          if not (valid case args const_required (rt1, rt2)) && life' > 0 then
            try_args (life' - 1)
          else (
            update_rt rt1 rt2;
            Al.Ast.CaseV (case, args) ) )
        in try_args 10
    in try_instr 100
  | VariantT typcases ->
    (* HARDCODE: Remove V128 *)
    let typcases = List.filter (fun (atom, _, _) -> atom <> Il.Ast.Atom "V128") typcases in
    let typcases = List.filter (fun (atom, _, _) -> atom <> Il.Ast.Atom "BOT") typcases in
    let typcase = choose typcases in
    let (atom, (_, typs, _), _) = typcase in
    let case = string_of_atom atom in
    push case case_stack;
    let args = gen_typs typs in
    pop case_stack;
    Al.Ast.CaseV (case, args)
  | _ -> failwith ("TODO: gen of " ^ Il.Print.string_of_deftyp syn ^ Il.Print.structured_string_of_deftyp syn)

and gen_wasm_expr rt rt_opt =
  let rec try_expr life =
    if (life = 0) then
      prerr_endline "life is 0..";
    push (rt, rt_opt) rt_stack;
    let n = Random.int 5 + 1 (* 1, 2, 3, 4, 5 *) in
    let ret = List.init n (fun _ -> gen "instr") |> listV in
    let (actual, expected) = List.hd !rt_stack in
    pop rt_stack;
    match actual, expected with
    | _, None -> ret
    | r, Some r' when matches_all r r' -> ret
    | _ when life > 0 -> try_expr (life - 1) (* TODO: This is THE source of inefficiency *)
    | _ -> ret
  in
  try_expr 1000

and gen_typ typ = match typ.it with
  | VarT id -> gen id.it |> cache_if (id.it = "typeidx") typeidx_cache
  | NumT NatT ->
    let i = Random.int 2 (* 0, 1 *) in
    numV i
  | IterT (typ', List) ->
    let is_types = match typ'.it with VarT id -> id.it = "type" | _ -> false in
    let n = Random.int 3 + 1 (* 1, 2, 3 *) + if is_types then 1 else 0 in
    let l = List.init n (fun _ -> gen_typ typ') |> cache_if is_types types_cache in
    listV l
  | TupT typs -> TupV (List.map gen_typ typs)
  | IterT (typ', Opt) ->
    if Random.bool() then
      optV_none
    else
      optV_some (gen_typ typ')
  | _ -> failwith ("TODO: unhandled type for gen_typ: " ^ Il.Print.structured_string_of_typ typ)

and gen_typs typs = match typs.it with
  | TupT typs' -> List.map gen_typ typs'
  | _ -> [ gen_typ typs ]

and fix_rts rt1 rt2 : (restype * restype) =
  let cache = ref Record.empty in
  let rec fix_rt rt = List.concat_map (fun vt -> match vt with
    | RefT t -> fix_rt [t] |> List.map (fun t' -> RefT t')
    | SubT (x, _) when Record.keys !cache |> contains x -> [ Record.find x !cache ]
    | SubT (x, sub) -> let vt = gen sub |> al_to_rt in cache := Record.add x vt !cache; [ vt ]
    | SeqT -> List.init (Random.int 3) (fun _ -> BotT)
    | t -> [ t ]
  ) rt in
  (fix_rt rt1, fix_rt rt2)

(** Mutation **)
let mutate modules = modules (* TODO *)

(** Injection **)
type invoke = string * Al.Ast.value list
type exn' = exn * string list
type invoke_result = (Al.Ast.value list, exn') result
type assertion = invoke * invoke_result
type instant_result = (assertion list, exn') result

let mk_assertion funcinst =
  let name = field_of_str "NAME" funcinst |> al_to_string in
  let addr = field_of_str "VALUE" funcinst |> arg_of_case "FUNC" 0 in
  let n = try
    !Ds.store
    |> Record.find "FUNC"
    |> al_to_list
    |> (fun l -> List.nth l (al_to_int addr))
    |> field_of_str "TYPE"
    |> arg_of_tup 0 (* Wasm 2.0 *)
    |> al_to_list
    |> List.length
  with _ -> 3 in
  let args = List.init n (fun i -> Al.Ast.CaseV ("CONST", [ singleton "I32"; numV (i+1) ])) in (*TODO *)
  let invoke = (name, args) in

  try
    let returns = Interpreter.call_invoke [ addr; args |> listV ] in
    invoke, Ok (al_to_list returns)
  with
    | e -> invoke, Error (e, !Interpreter.algo_name_stack)

let print_assertion ((f, args), result) =
  print_endline (match result with
  | Ok returns -> Printf.sprintf "(assert_return (invoke %s [%s]) [%s])"
    f
    (args |> List.map Al.Print.string_of_value |> String.concat " ")
    (returns |> List.map Al.Print.string_of_value |> String.concat " ")
  | Error (Exception.Trap, _) -> Printf.sprintf "(assert_trap (invoke %s [%s]))"
    f
    (args |> List.map Al.Print.string_of_value |> String.concat " ")
  | Error (e, stack) -> Printf.sprintf "Invocation of %s failed due to %s during %s"
    f
    (Printexc.to_string e)
    (String.concat "," stack) )

let get_instant_result m : instant_result = try
  let externvals = listV (
    List.init 1 (fun i -> Al.Ast.CaseV ("FUNC", [numV i]))
    @ List.init 4 (fun i -> Al.Ast.CaseV ("GLOBAL", [numV i]))
    @ List.init 1 (fun i -> Al.Ast.CaseV ("TABLE", [numV i]))
    @ List.init 1 (fun i -> Al.Ast.CaseV ("MEM", [numV i]))
  ) in (*TODO *)
  let mm = Interpreter.call_instantiate [ m; externvals ] in
  let exported_funcs =
    mm
    |> field_of_str "EXPORT"
    |> al_to_list
    |> List.filter (fun inst -> inst |> field_of_str "VALUE" |> is_case "FUNC")
  in
  Ok (List.map mk_assertion exported_funcs)
with
  | e -> Error (e, !Interpreter.algo_name_stack)
let inject m = (m, get_instant_result m)

type module_ = Al.Ast.value
type test = module_ * instant_result

(** Output **)

let print_test (m, result) =
  print_endline (string_of_module m);
  ( match result with
  | Ok assertions ->
    print_endline "Instantiation success";
    List.iter print_assertion assertions
  | Error (Exception.Trap, stack) ->
    print_endline ("Instantiation trapped during " ^ (String.concat "," stack))
  | Error (e, stack) ->
    print_endline ("Instantiation failed: " ^ Printexc.to_string e ^ " during " ^ (String.concat "," stack)) );
  print_endline "================"

let to_wast i (m, result) =
  "Writing " ^ (string_of_int i) ^ "th module..." |> print_endline;
  let file = Filename.concat !Flag.out (string_of_int i ^ ".wast") in
  let oc = open_out file in
  let ref_module = Construct.al_to_module m in

  Reference_interpreter.Print.module_ oc 80 ref_module;

  close_out oc;
  ignore result

(* Generate tests *)

let gen_test el' il' al' =
  (* Register spec *)
  el := el';
  il := flatten_rec il';
  al := al';

  (* Initialize *)
  Random.init !seed;
  Ds.init !al;
  estimate_rt ();
  estimate_const ();
  Tester.init_tester ();

  (* Generate tests *)
  let seeds = List.init !test_num (fun _i ->
    prerr_endline ("Generatring " ^ string_of_int _i ^ "th module...");
    gen "module"
  ) in

  (* Mutatiion *)
  let modules = mutate seeds in

  (* Injection *)
  let tests = List.map inject modules in

  (* Print result *)
  List.iter print_test tests;

  (* Convert to Wast *)
  List.iteri to_wast tests

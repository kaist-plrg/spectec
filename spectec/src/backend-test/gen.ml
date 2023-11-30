open Flag
open Util.Source
open Util.Record

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

let listV l = Al.Ast.ListV (l |> Array.of_list |> ref)
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

let valid case args const (_rt1, rt2) = Al.Ast.(
  (* Bunch of hardcoded heuristics to check if the given Wasm instruction is valid *)
  match case with
  | "UNOP" | "BINOP" | "TESTOP" | "RELOP" -> ( match args, rt2 with
    | [ CaseV ("I32", []); CaseV ("_I", _) ], [ T "I32" ]
    | [ CaseV ("I64", []); CaseV ("_I", _) ], [ T "I64" ] -> true
    | [ CaseV ("F32", []); CaseV ("_F", _) ], [ T "F32" ]
    | [ CaseV ("F64", []); CaseV ("_F", _) ], [ T "F64" ] -> not const
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
    | _ -> true )
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

(* Generate specific syntax from input IL *)
let rec gen name =
  let syn = find_syn name in

  (* HARDCODE: Wasm expression *)
  if name = "expr" then gen_wasm_expr [] ( match top case_stack with
    | "FUNC" -> None
    | "ACTIVE" -> Some [T "I32"]
    | _ -> Some [TopT] ) else

  (* HARDCODE: name *)
  if name = "name" then (Al.Ast.TextV (choose ["a"; "b"; "c"] ^ choose ["1"; "2"; "3"])) else

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
  | NotationT ([[]; [Arrow]; []], typs) ->
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
  try_expr 100

and gen_typ typ = match typ.it with
  | VarT id -> gen id.it
  | NumT NatT ->
    let i = Random.int 2 in (* 0, 1 *)
    numV i
  | IterT (typ', List) ->
    let n = Random.int 3 + 1 in (* 1, 2, 3 *)
    let n = match typ'.it with VarT id when id.it = "type" -> n + 1 | _ -> n in
    let l = List.init n (fun _ -> gen_typ typ') in
    listV l
  | TupT typs -> List.map gen_typ typs |> listV
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

(* Generate tests *)

let gen_test el' il' al' =
  (* Register spec *)
  el := el';
  il := flatten_rec il';
  al := al';

  (* Initialize *)
  Random.init !seed;
  Backend_interpreter.Ds.init !al;
  estimate_rt ();
  (* print_rts(); *)
  estimate_const ();

  (* Generate tests *)
  let seeds = List.init !test_num (fun _i ->
    prerr_endline ("Generatring " ^ string_of_int _i ^ "th module...");
    gen "module"
  ) in

  (* Mutatiion *)
  let tests = mutate seeds in

  (* Result *)
  Backend_interpreter.Tester.init_tester ();

  tests |> List.iter (fun m ->
    print_endline "================";
    print_endline (string_of_module m);
    try
      let externvals = listV (
        List.init 1 (fun i -> Al.Ast.CaseV ("FUNC", [numV i]))
        @ List.init 4 (fun i -> Al.Ast.CaseV ("GLOBAL", [numV i]))
        @ List.init 1 (fun i -> Al.Ast.CaseV ("TABLE", [numV i]))
        @ List.init 1 (fun i -> Al.Ast.CaseV ("MEM", [numV i]))
      ) in (*TODO *)
      Backend_interpreter.Interpreter.call_instantiate [ m; externvals ] |> ignore;
      print_endline "Instantiation success"
    with e ->
      print_endline (
        "Instantiation fail due to "
        ^ Printexc.to_string e
        ^ " during "
        ^ String.concat ", " !Backend_interpreter.Interpreter.algo_name_stack
      )
  )

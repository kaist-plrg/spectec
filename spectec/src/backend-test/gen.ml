open Flag
open Utils
open Valid

open Util.Source
open Util.Record
open Backend_interpreter
open Al.Al_util

(* Specs *)
let el: El.Ast.script ref = ref []
let il: Il.Ast.script ref = ref []
let al: Al.Ast.script ref = ref []

(* Helpers *)

let hds xs = xs |> List.rev |> List.tl |> List.rev

let option_flatmap f opt = Option.bind opt f

let (-->) p q = (not p) || q
let (%>) f g v = f v |> g

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

let string_of_atom = Il.Print.string_of_atom
let string_of_module m = match m with
| Al.Ast.CaseV ("MODULE", args) ->
  "(MODULE\n  " ^ (List.map Al.Print.string_of_value args |> String.concat "\n  ") ^ "\n)"
| _ -> failwith "Unreachable"

let flatten_args e = match e.it with
| Il.Ast.TupE es -> es
| _ -> [ e ]

let replace ixs = List.mapi (fun i x -> match List.assoc_opt i ixs with Some x' -> x' | None -> x)

let push v s = s := v :: !s
let pop s = s := List.tl !s
let top s = List.hd !s
let string_of_stack s = String.concat "," !s


(** Initialize **)

let rts = ref Record.empty

let rec string_of_vt = function
| T v -> Al.Print.string_of_value v
| SubT (x, sub) -> x ^ "<:" ^ sub
| TopT | BotT -> "_"
| SeqT t -> (string_of_vt t) ^ "*"
let string_of_rt vts = List.map string_of_vt vts |> String.concat " "
let print_rts () =
  Record.iter (fun (k, v) ->
    let (rt1, rt2, _) = v in
    Printf.sprintf "%s : %s -> %s" k (string_of_rt rt1) (string_of_rt rt2) |> print_endline
  ) !rts

let estimate_rt () = Il.Ast.(
  let rec get_rt e = match e.it with
  | CaseE (atom, { it = TupE []; _}) -> [ T (singleton (string_of_atom atom)) ]
  | ListE es -> List.concat_map get_rt es
  | VarE id -> [ SubT (id.it, Il.Print.string_of_typ e.note) ]
  | SubE ({ it = VarE id; _ }, { it = VarT id'; _ }, _)  -> [ SubT (id.it, id'.it) ]
  | IterE (e', (Il.Ast.List, _)) -> [ SeqT (get_rt e' |> List.hd) ]
  | CatE (e1, e2) -> get_rt e1 @ get_rt e2
  | _ -> [ TopT ] in

  let get_entangles case ts = match case.it with
  | CaseE (_, args) -> List.mapi (fun i arg ->
    let arg_t = get_rt arg in
    match arg_t with
    | [ SubT (x, _) as t ] -> if contains t ts then Some (i, x) else None
    | _ -> None
  ) (flatten_args args) |> List.filter_map (fun x -> x)
  | _ -> [] in

  let expected e kind =
    Printf.sprintf "Expected %s to be %s" (Il.Print.string_of_exp e) kind |> failwith
  in

  List.iter (fun def -> match def.it with
    | RelD (id, _, _, rules) when id.it = "Instr_ok" || id.it = "Instrf_ok" ->
      List.iter (fun rule -> match rule.it with
        | RuleD (id, _, _, exp, _) -> ( match exp.it with
          | TupE [_c; lhs; rhs] -> ( match rhs.it with
            | MixE (_, args) -> ( match args.it with
              | TupE [t1; t2] | TupE [t1; _; t2] ->
                let name = String.split_on_char '-' id.it |> List.hd |> String.uppercase_ascii in
                let rt1 = get_rt t1 in
                let rt2 = get_rt t2 in
                let entangles = get_entangles lhs (rt1 @ rt2) in
                rts := Record.add name (rt1, rt2, entangles) !rts;
              | _ -> expected args "t1, t2 or t1, x*, t2" )
            | _ -> expected rhs "e1 -> e2" )
          | _ -> expected exp "C |- lhs : rhs" )
      ) rules
    | _ -> ()
  ) !il
)

let expr_info_stack = ref []
let updated_stack rt1 rt2 (st, target, label, n) =
  let st1 = List.fold_right (fun _ st -> List.tl st) rt1 st in
  let st2 = List.fold_left  (fun st t -> t :: st) st1 rt2 in
  (st2, target, label, n - 1)
let update_rt rt1 rt2 =
  match !expr_info_stack with
  | [] -> failwith "expr_info_stack is empty"
  | hd :: tl -> expr_info_stack := (updated_stack rt1 rt2 hd) :: tl
let nullify_target () =
  match !expr_info_stack with
  | [] -> failwith "expr_info_stack is empty"
  | (st, _, label, n) :: tl -> expr_info_stack := (st, None, label, n) :: tl

let matches vt1 vt2 = match vt1, vt2 with
| BotT, _ | _, BotT -> false
| TopT, _ | _, TopT -> true
| T x1, T x2 -> x1 = x2
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

  aux (List.rev rt) (top expr_info_stack |> (fun (x, _, _, _) -> x))

let edit_dist ts1 ts2_opt = match ts2_opt with
  | None -> 0
  | Some ts2 ->
    let rec common_len xs ys = match xs, ys with
    | x :: xs', y :: ys' when matches x y -> 1 + common_len xs' ys'
    | _ -> 0 in
    let l1 = List.length ts1 in
    let l2 = List.length ts2 in
    let l3 = common_len (List.rev ts1) (List.rev ts2) in
    (l1-l3) + (l2-l3)

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

type context = {
  i: int;               (* Denote generating i-th value for IterT *)
  parent_case: string;  (* Denote case of most recent parent CaseV *)
  parent_name: string;  (* Denote name of most recent production *)
  depth_limit: int;     (* HARDCODE: Limit on block / loop / if depth *)
  is_func: bool;        (* HARDCODE: currently making function *)
}
let default_context = {
  i = 0;
  parent_case = "";
  parent_name = "";
  depth_limit = 3;
  is_func = false;
}

let types_cache = ref []
let type_cache = ref (numV 0)
let locals_cache = ref []
let tids_cache = ref []
let globals_cache = ref []
let tables_cache = ref []
let elems_cache = ref []
let refs_cache = ref []

let init_cache () =
  types_cache := [];
  type_cache := (numV 0);
  locals_cache := [];
  tids_cache := [];
  globals_cache := [];
  tables_cache := [];
  elems_cache := [];
  refs_cache := [];
  ()

let do_cache ref v = ref := v; v
let append_cache ref v = ref := v :: !ref; v
let cache_if cond ref v = if cond then ref := v; v
let append_cache_if cond ref v = if cond then ref := v :: !ref; v

let get_type types tid =
  let arrow = List.nth types tid |> arg_of_case "TYPE" 0 in
  let f i =
    arrow
    |> arg_of_tup i (* Wasm 2.0 *)
    |> al_to_list
    |> List.map (fun v -> T v) in
  (f 0, f 1)
let estimate_out t types = Al.Ast.(
  match t with
  | NumV i64 ->
    Int64.to_int i64
    |> get_type types
    |> snd
  | TupV [ CaseV ("MUT", _); v ] | v ->
    [ T v ]
)

let is_func_table = function
| Al.Ast.CaseV ("TABLE", [ TupV [ _; CaseV ("FUNCREF", []) ]]) -> true
| _ -> false

exception OutOfLife

(* Generate specific syntax from input IL *)
let rec gen c name =
  let c' = { c with parent_name = name } in
  let c' = if name = "func" || name = "start" then { c' with is_func = true } else c' in

  (* HARDCODE: Wasm expression *)
  if name = "expr" then
    let out = ( match c.parent_case with
    | "FUNC" | "GLOBAL" | "ELEM" -> estimate_out !type_cache !types_cache
    | "ACTIVE" -> [ T (singleton "I32") ]
    | _ -> [ TopT ] ) in
    gen_wasm_expr c' [] out out else
  (* HARDCODE: name *)
  if name = "name" then (Al.Ast.TextV (choose ["a"; "b"; "c"] ^ choose ["1"; "2"; "3"])) else
  (* HARDCODE: memidx to be always 0 *)
  if name = "memidx" then numV 0 else
  (* HARCODE: typeidx of function is already cached *)
  if name = "typeidx" && c.parent_case = "FUNC" then List.nth !tids_cache c.i |> numV |> do_cache type_cache else
  (* HARDCODE: pack_size to be 8/16/32 *)
  if contains c.parent_case [ "LOAD"; "STORE"; "EXTEND" ] && name = "n" then
    numV (choose [8; 16; 32])
  else

  let post_process =
  cache_if (
    name = "globaltype"
    || name = "reftype" && c.parent_case = "ELEM" ) type_cache
  %> append_cache_if (
    name = "funcidx" && not c.is_func) refs_cache in


  let syn = find_syn name in
  ( match syn.it with
  | AliasT typ -> gen_typ c' typ
  (* TupV *)
  | NotationT ([[]; []; []], typs) ->
    let pair = gen_typs c' typs in
    Al.Ast.TupV pair
  | NotationT ([[LBrack]; [Dot2]; [RBrack]], typs) ->
    (* limits *)
    let pair = gen_typs c' typs in
    let fst = List.hd pair in
    let snd = List.hd (List.tl pair) in
    let new_snd = add_num fst snd in
    Al.Ast.TupV [ fst; new_snd ]
  (* ArrowV *)
  | NotationT ([[]; [Arrow]; []], typs)
  | NotationT ([[]; [Star; Arrow]; [Star]], typs) ->
    let pair = gen_typs c' typs in
    let fst = List.hd pair in
    let snd = List.hd (List.tl pair) in
    Al.Ast.ArrowV (fst, snd)
  (* CaseV *)
  | NotationT ((atom :: _) :: _, typs)
  | NotationT ([[]; [atom]], typs) (* Hack for I8 *) ->
    let case = string_of_atom atom in
    let c'' = { c' with parent_case = case } in
    let args = gen_typs c'' typs in
    let args' = List.map (gen_wasm_funcs c'') args in (* Regenerated deferred wasm funcs *)
    Al.Ast.CaseV (case, args')
  (* StrV *)
  | StructT typfields ->
    let rec_ = List.fold_right (fun typefield ->
      let (atom, (_, typ, _), _) = typefield in
      let key = string_of_atom atom in
      let value = gen_typ c' typ in
      Record.add key value
    ) typfields Record.empty in
    Al.Ast.StrV rec_
  (* CaseV *)
  (* HARDCODE: Wasm instruction *)
  | VariantT typcases when name = "instr" ->
    (* HARDCODE: checks if currently in a context that requires const instrution *)
    let const_required = (contains c.parent_case !const_ctxs) in
    let const_filter (atom, _, _) =
      const_required --> contains (string_of_atom atom) !consts in
    let block_filter (atom, _, _) =
      (c.depth_limit <= 0) --> not (contains (string_of_atom atom) [ "BLOCK"; "LOOP"; "IF" ]) in
    let typcases' = typcases
      |> List.filter const_filter
      |> List.filter block_filter
    in
    let rec try_instr life =
      if life = 0 then raise OutOfLife;
      let typcase = choose typcases' in
      let (atom, (_, typs, _), _) = typcase in
      let case = string_of_atom atom in
      let (t1, t2, entangles) = !rts |> Record.find case in
      let (rt1, rt2, induced_args) = fix_rts case const_required t1 t2 entangles in
      let valid_rts = (
        poppable rt1 && (
          let (cur_stack, target_stack_opt, _, n) = updated_stack rt1 rt2 (List.hd !expr_info_stack) in
          let d = edit_dist cur_stack target_stack_opt in
          (* Check if Random.float <= n / (n+d), but in smarter way *)
          d = 0 || (Random.int (n + d) < n) )
      ) in
      if not valid_rts then
        try_instr (life - 1)
      else
        let rec try_args life' =
          if life' = 0 then try_instr (life - 1) else (
          let c'' = { c' with parent_case = case; depth_limit = c'.depth_limit - 1 } in
          let args = ( match case with
          | "BLOCK" -> [ gen c'' "blocktype"; gen_wasm_expr c'' rt1 rt2 rt2 ]
          | "LOOP" -> [ gen c'' "blocktype"; gen_wasm_expr c'' rt1 rt2 rt1 ]
          | "IF" -> [ gen c'' "blocktype"; gen_wasm_expr c'' (hds rt1) rt2 rt2; gen_wasm_expr c'' (hds rt1) rt2 rt2 ]
          | _ -> gen_typs c'' typs ) |> replace induced_args in
          match validate_instr case args const_required (rt1, rt2) with
          | None -> try_args (life' - 1)
          | Some args' ->
            update_rt rt1 rt2;
            if contains name ["RETURN"; "BR"; "BR_TABLE"; "UNREACHABLE"] then (*TODO: Perhaps automate this? *)
              nullify_target ();
            Al.Ast.CaseV (case, args') )
        in try_args 100
    in try_instr 100
  | VariantT typcases ->
    (* HARDCODE: Remove V128 *)
    let typcases = List.filter (fun (atom, _, _) -> atom <> Il.Ast.Atom "V128") typcases in
    let typcases = List.filter (fun (atom, _, _) -> atom <> Il.Ast.Atom "BOT") typcases in
    let typcase = choose typcases in
    let (atom, (_, typs, _), _) = typcase in
    let case = string_of_atom atom in
    let args = gen_typs { c' with parent_case = case } typs in
    Al.Ast.CaseV (case, args)
  | _ -> failwith ("TODO: gen of " ^ Il.Print.string_of_deftyp syn ^ Il.Print.structured_string_of_deftyp syn)
  ) |> post_process

and gen_wasm_expr c rt1 rt2 label =
  let max_life = 100 in
  let rec try_expr life =
    if (life = 0) then
      List.map (fun _ -> singleton "DROP") rt1 @ List.map default rt2 |> listV
    else
      let n = Random.int 5 + 1 (* 1, 2, 3, 4, 5 *) in
      push (List.rev rt1, Some (List.rev rt2), label, n) expr_info_stack; (* TODO: make input optional? *)
      try
        let ret = List.init n (fun i -> gen { c with i = i } "instr") |> listV in
        pop expr_info_stack;
        ret
      with
        OutOfLife -> pop expr_info_stack; try_expr (life - 1)
  in
  try_expr max_life

and gen_wasm_funcs c = function
  | CaseV ("DEFERRED_FUNCS", []) ->
    List.init (List.length !tids_cache) (fun i -> gen { c with i = i } "func") |> listV
  | v -> v

and gen_typ c typ = match typ.it with
  (* HARDCODE: imported builtins *)
  | IterT ({ it = VarT id; _ }, List) when id.it = "import" ->
    let import name kind t = Al.Ast.CaseV ("IMPORT", [ TextV "spectest"; TextV name; case_v kind t ]) in
    let const = empty "MUT" in
    listV [
      import "print" "FUNC" zero;
      import "global_i32" "GLOBAL" (TupV [const; singleton "I32"]);
      import "global_i64" "GLOBAL" (TupV [const; singleton "I64"]);
      import "global_f32" "GLOBAL" (TupV [const; singleton "F32"]);
      import "global_f64" "GLOBAL" (TupV [const; singleton "F64"]);
      import "table" "TABLE" (TupV [ TupV [ NumV 10L; NumV 20L ]; singleton "FUNCREF" ]);
      (* import "memory" "MEM" (CaseV ("I8", [ TupV [ NumV 1L; NumV 2L ] ])); *)
    ]
  (* General types *)
  | VarT id -> gen c id.it
  | NumT NatT ->
    let i = Random.int 3 (* 0, 1, 2 *) in
    numV i
  | IterT (typ', List) ->
    let name = match typ'.it with | VarT id -> id.it | _ -> "" in
    let n = match name with
    | "table" | "data" | "elem" | "type" | "func" | "global" | "mem" -> Random.int 3 + 3 (* 3, 4, 5 *)
    | _ -> Random.int 3 (* 0, 1, 2 *) in
    if name = "func" then (* Hardcode: Defer generating functions *)
      let tids = List.init n (fun _ -> Random.int (List.length !types_cache)) in
      tids_cache := tids;
      singleton "DEFERRED_FUNCS" else
    let l = List.init n (fun i -> gen_typ { c with i = i } typ') in
    if name = "type" then types_cache := l;
    if name = "local" then locals_cache := l;
    if name = "table" then tables_cache := l;
    if name = "global" then globals_cache := l;
    if name = "elem" then elems_cache := l;
    listV l
  | TupT typs -> TupV (List.map (gen_typ c) typs)
  | IterT (typ', Opt) ->
    if Random.bool() then
      optV_none
    else
      optV_some ((gen_typ c) typ')
  | _ -> failwith ("TODO: unhandled type for gen_typ: " ^ Il.Print.structured_string_of_typ typ)

and gen_typs c typs = match typs.it with
  | TupT typs' -> List.map (gen_typ c) typs'
  | _ -> [ gen_typ c typs ]

and fix_rts case const_required rt1 rt2 entangles = Al.Ast.(
  let bot = [ BotT ], [], [] in

  if contains case [ "BLOCK"; "LOOP"; "IF" ] then
    let i32_opt = if case = "IF" then [ T (singleton "I32") ] else [] in
    let bt = gen default_context "blocktype" in
    let pair = [ 0, bt ] in
    match bt with
    | CaseV ("_RESULT", [ OptV None ]) -> i32_opt, [], pair
    | CaseV ("_RESULT", [ OptV (Some t) ]) -> i32_opt, [ T t ], pair
    | CaseV ("_IDX", [ tid ]) ->
      let arrow = get_type !types_cache (al_to_int tid) in
      fst arrow @ i32_opt, snd arrow, pair
    | _ -> failwith "Unreachable (Are you using Wasm 1 or Wasm 3?)"

  else if case = "REF.FUNC" then
    if List.length !refs_cache = 0 then bot
    else rt1, rt2, [ 0, choose !refs_cache ]

  else if contains case [ "LOCAL.GET"; "LOCAL.SET"; "LOCAL.TEE" ] then (*TODO: Perhaps automate this? *)
    let params = get_type !types_cache (!type_cache |> al_to_int) |> fst in
    let locals = !locals_cache |> List.map (fun l -> T (arg_of_case "LOCAL" 0 l)) in
    let ls = params @ locals in
    if ls = [] then bot else
    let lid = Random.int (List.length ls) in
    let lt = List.nth ls lid in
    let subst = function SubT _ -> lt | t -> t in
    (List.map subst rt1, List.map subst rt2, [ 0, numV lid ])

  else if contains case [ "GLOBAL.GET"; "GLOBAL.SET" ] then (*TODO: Perhaps automate this? *)
    let no_mut = case_v "MUT" (OptV None) in
    let gs = List.map (function
    | CaseV ("GLOBAL", [ TupV [ m; t ]; _ ]) -> (m <> no_mut, t)
    | _ -> failwith "Unreachable: Global") !globals_cache in
    let g x = (false, singleton x) in
    let gs_builtins = [ g "I32"; g "I64"; g "F32"; g "F64" ] in
    let gs' = if const_required then gs_builtins else gs_builtins @ gs in
    let gids = find_index_all (fun (is_mut, _) -> (case = "GLOBAL.SET") --> is_mut) gs' in
    if gids = [] then bot else
    let gid = choose gids in
    let t = List.nth gs' gid |> snd in
    let subst = function SubT _ -> T t | t' -> t' in
    (List.map subst rt1, List.map subst rt2, [ 0, numV gid ])

  else if contains case [ "TABLE.GET"; "TABLE.SET"; "TABLE.GROW"; "TABLE.FILL" ] then
    let (tid, table) = choosei !tables_cache in
    let rt = match table with
    | CaseV ("TABLE", [ TupV [ _; rt ] ] ) -> rt
    | _ -> failwith "Unreachable: Table" in
    let subst = function SubT _ -> T rt | t' -> t' in
    (List.map subst rt1, List.map subst rt2, [ 0, numV tid ])

  else if case = "TABLE.COPY" then
    let get_rt = function
    | CaseV ("TABLE", [ TupV [ _; rt ] ] ) -> rt
    | _ -> failwith "Unreachable: Table" in
    let groups = groupi_by get_rt !tables_cache in
    let (_, tids) = choose groups in
    (rt1, rt2, [ 0, numV (choose tids); 1, numV (choose tids) ])

  else if case = "TABLE.INIT" then
    let get_rt = function
    | CaseV ("TABLE", [ TupV [ _; rt ] ] )
    | CaseV ("ELEM", rt :: _) -> rt
    | _ -> failwith "Unreachable: Table / Elem" in
    let tgroups = groupi_by get_rt !tables_cache in
    let egroups = groupi_by get_rt !elems_cache in
    let tegroups = List.fold_left (fun acc (et, eids) ->
      let f (tt, tids) = if (et = tt) then Some tids else None in
      match List.find_map f tgroups with
      | Some tids -> (tids, eids) :: acc
      | None -> acc
    ) [] egroups in
    if tegroups = [] then bot else
    let (tids, eids) = choose tegroups in
    (rt1, rt2, [ 0, numV (choose tids); 1, numV (choose eids) ])

  else if case = "RETURN" then
    (* TODO: Signal arbitrary size better *)
    (List.init (Random.int 3) (fun _ -> TopT) @ estimate_out !type_cache !types_cache),
    List.init 3 (fun _ -> TopT),
    []

  else if case = "BR" then
    let lid = Random.int (List.length !expr_info_stack) in
    let (_, _, t, _) = List.nth !expr_info_stack lid in
    (List.init (Random.int 3) (fun _ -> TopT) @ t),
    List.init 3 (fun _ -> TopT),
    [ 0, numV lid ]
  else if case = "BR_IF" then
    let lid = Random.int (List.length !expr_info_stack) in
    let (_, _, t, _) = List.nth !expr_info_stack lid in
    t @ [ T (singleton "I32") ],
    t,
    [ 0, numV lid ]
  else if case = "BR_TABLE" then
    let lid_groups = groupi_by (fun (_, _, t, _) -> t) !expr_info_stack in
    let (t, is) = choose lid_groups in
    let lids = List.init (Random.int 3) (fun _ -> choose is) in
    let lid = choose is in
    (List.init (Random.int 3) (fun _ -> TopT) @ t @ [ T (singleton "I32") ]),
    List.init 3 (fun _ -> TopT),
    [ 0, List.map numV lids |> listV; 1, numV lid ]

  else if case = "CALL" then
    let fid, tid = choosei !tids_cache in
    let arrow = get_type !types_cache tid in
    fst arrow, snd arrow, [ 0, numV (fid + 1) (* due to imported function (move to patch?) *) ]

  else if case = "CALL_INDIRECT" then
    let tables = find_index_all is_func_table !tables_cache in
    if tables = [] then bot else
    let table = choose tables in
    let tid = Random.int (List.length !types_cache) in
    let arrow = get_type !types_cache tid in
    fst arrow @ [ T (singleton "I32") ], snd arrow, [ 0, numV table; 1, numV (tid + 1) (* move to patch? *) ]

  else
    let cache = ref Record.empty in
    let len_cache = ref Record.empty in
    let update r x v = r := Record.add x v !r; v in
    let get x sub = try Record.find x !cache with | _ -> gen default_context sub |> update cache x in
    let get_len x = try Record.find x !len_cache with | _ -> Random.int 3 |> update len_cache x in

    let rec fix_rt rt = List.concat_map (fun vt -> match vt with
      | SubT (x, sub) -> [ T (get x sub) ]
      | SeqT (SubT (x, sub)) -> List.init (get_len x) (fun i -> (List.hd (fix_rt [ SubT (x ^ string_of_int i, sub) ])))
      | SeqT t -> List.init (Random.int 3) (fun _ -> (List.hd (fix_rt [ t ])))
      | t -> [ t ]
    ) rt in
    let rt1' = fix_rt rt1 in
    let rt2' = fix_rt rt2 in
    let induced_args = List.map (fun (i, x) -> (i, Record.find x !cache)) entangles in
    (rt1', rt2', induced_args)
)

(** Mutation **)
let mutate modules =
  let aux i m = try
    prerr_endline ("Patching " ^ string_of_int i ^ "th module..");
    Patch.patch_module m
  with e -> prerr_endline (Printexc.to_string e); m in
  List.mapi aux modules
  (* TODO *)

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
    (* @ List.init 1 (fun i -> Al.Ast.CaseV ("MEM", [numV i])) *)
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
  ignore result;
  "Writing " ^ (string_of_int i) ^ "th module..." |> prerr_endline;
  let file = Filename.concat !Flag.out (string_of_int i ^ ".wast") in
  let ref_module = Construct.al_to_module m in

  let oc = open_out file in
  Reference_interpreter.Print.module_ oc 80 ref_module;
  close_out oc;

  try
    Reference_interpreter.Valid.check_module ref_module;
    prerr_endline "Valid"
  with
    | e -> prerr_endline ("Invalid: " ^ Printexc.to_string e)

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
  let seeds = List.init !test_num (fun i ->
    prerr_endline ("Generatring " ^ string_of_int i ^ "th module...");
    init_cache ();
    gen default_context "module"
  ) in

  (* Mutatiion *)
  let modules = mutate seeds in

  (* Injection *)
  let tests = List.map inject modules in

  (* Print result *)
  List.iter print_test tests;

  (* Convert to Wast *)
  List.iteri to_wast tests

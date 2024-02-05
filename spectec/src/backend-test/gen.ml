open Flag
open Utils
open Valid

open Util.Source
open Util
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

let flatten_rec =
  List.concat_map (fun def ->
    match def.it with
    | Il.Ast.RecD defs -> defs
    | _ -> [ def ]
  )

let find_syn name =
  match
    List.find_map (fun def ->
      match def.it with
      | Il.Ast.SynD (id, deftyp) when id.it = name -> Some deftyp
      | _ -> None
    ) !il
  with
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

let print_rts () =
  Record.iter (fun k v ->
    let (rt1, rt2, _) = v in
    Printf.sprintf "%s : %s -> %s" k (string_of_rt rt1) (string_of_rt rt2) |> print_endline
  ) !rts


let get_typing_rules =
  List.concat_map (fun def ->
    match def.it with
    | Il.Ast.RelD (id, _, _, rules) when id.it = "Instr_ok" || id.it = "Instrf_ok" -> rules
    | _ -> []
  )

let get_rt rule =
  let open Il.Ast in
  let rec estimate_rt e =
    match e.it with
    | CaseE (atom, { it = TupE []; _}) -> [ T (nullary (string_of_atom atom)) ]
    | ListE es -> List.concat_map estimate_rt es
    | VarE id -> [ SubT (id.it, Il.Print.string_of_typ e.note) ]
    | SubE ({ it = VarE id; _ }, { it = VarT id'; _ }, _)  -> [ SubT (id.it, id'.it) ]
    | SubE (
      { it = CallE (name, _); _},
      { it = VarT id; _ },
      _
    ) when name.it = "unpacked" -> [ SubT (id.it, id.it) ]
    | IterE (e', (List, _)) -> [ SeqT (List.hd (estimate_rt e')) ]
    | CatE (e1, e2) -> estimate_rt e1 @ estimate_rt e2
    | _ -> [ TopT ]
  in

  let get_entangles case ts =
    match case.it with
    | CaseE (_, args) ->
      List.mapi (fun i arg ->
        match estimate_rt arg with
        | [ SubT (x, _) as t ] -> if List.mem t ts then Some (i, x) else None
        | _ -> None
      ) (flatten_args args)
      |> List.filter_map (fun x -> x)
    | _ -> []
  in

  let expected e kind =
    Printf.sprintf "Expected %s to be %s" (Il.Print.string_of_exp e) kind |> failwith
  in

  let RuleD (id, _, _, exp, _) = rule.it in
  match exp.it with
  | TupE [_c; lhs; rhs] ->
    (match rhs.it with
    | MixE (_, args) ->
      (match args.it with
      | TupE [t1; t2] | TupE [t1; _; t2] ->
        let name = id.it |> String.split_on_char '-' |> List.hd |> String.uppercase_ascii in
        let rt1 = estimate_rt t1 in
        let rt2 = estimate_rt t2 in
        let entangles = get_entangles lhs (rt1 @ rt2) in
        name, ref (rt1, rt2, entangles)
      | _ -> expected args "t1, t2 or t1, x*, t2"
      )
    | _ -> expected rhs "e1 -> e2"
    )
  | _ -> expected exp "C |- lhs : rhs"

let expr_info_stack = ref []
let updated_stack rt1 rt2 (st, target, label, n) =
  let st1 = List.fold_right (fun _ st -> List.tl st) rt1 st in
  let st2 = List.fold_left  (fun st t -> t :: st) st1 rt2 in
  st2, target, label, n - 1
let update_rt rt1 rt2 =
  match !expr_info_stack with
  | [] -> failwith "expr_info_stack is empty"
  | hd :: tl -> expr_info_stack := updated_stack rt1 rt2 hd :: tl
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
  let rec aux l1 l2 =
    match l1, l2 with
    | [], _ -> true
    | hd1 :: tl1, hd2 :: tl2 -> matches hd1 hd2 && aux tl1 tl2
    | _ -> false
  in
  expr_info_stack
  |> top
  |> (fun (x, _, _, _) -> x)
  |> aux (List.rev rt)

let edit_dist ts1 ts2_opt =
  match ts2_opt with
  | None -> 0
  | Some ts2 ->
    let rec common_len xs ys =
      match xs, ys with
      | x :: xs', y :: ys' when matches x y -> 1 + common_len xs' ys'
      | _ -> 0
    in
    let l1 = List.length ts1 in
    let l2 = List.length ts2 in
    let l3 = common_len (List.rev ts1) (List.rev ts2) in
    (l1-l3) + (l2-l3)

let consts = ref []
let const_ctxs = ref []
let estimate_const () =
  let open Il.Ast in
  List.iter (fun def ->
    match def.it with
    | RelD (id, _, _, rules) when id.it = "Instr_const" ->
      List.iter (fun rule -> match rule.it with
        | RuleD (id, _, _, _, _) -> push (String.uppercase_ascii id.it) consts
      ) rules
    | RelD (_, _, _, rules) ->
      List.iter (fun rule ->
        let rec is_const_checking prem =
          match prem.it with
          | RulePr (id, _, _) -> id.it = "Expr_ok_const"
          | IterPr (prem', _) -> is_const_checking prem'
          | _ -> false
        in
        match rule.it with
        | RuleD (_, _, _, args, prems) when List.exists is_const_checking prems ->
          (match args.it with
          | TupE [_; e; _] | TupE [_; e] ->
            (match e.it with
            | CaseE (Atom atomid, _) | MixE ([Atom atomid] :: _, _) ->
              push atomid const_ctxs
            | _ -> ()
            )
          | _ -> ()
          )
        | _ -> ()
      ) rules
    | _ -> ()
  ) !il

let print_consts () = List.iter print_endline !consts

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
let type_cache = ref (numV 0L)
let locals_cache = ref []
let tids_cache = ref []
let globals_cache = ref []
let tables_cache = ref []
let elems_cache = ref []
let refs_cache = ref []

let init_cache () =
  types_cache := [];
  type_cache := (numV 0L);
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
  let arrow = List.nth types tid |> casev_nth_arg 0 in
  let f i =
    i
    |> List.nth (unwrap_tupv arrow)
    |> unwrap_listv_to_list
    |> List.map (fun v -> T v) in
  f 0, f 1

(* get output type of t *)
let estimate_out t types =
  let open Al.Ast in
  match t with
  | NumV i64 ->
    i64
    |> Int64.to_int
    |> get_type types
    |> snd
  | TupV [ CaseV ("MUT", _); v ] | v -> [ T v ]

let is_func_table = function
  | Al.Ast.CaseV ("TABLE", [ TupV [ _; CaseV ("FUNCREF", []) ]]) -> true
  | _ -> false

exception OutOfLife

let gen_vector () =
  let gen_byte _ = string_of_int (Random.int 256) in
  gen_byte |> List.init 16 |> List.fold_left (^) "" |> vecV

(* Generate specific syntax from input IL *)
let rec gen c name =
  let c' = { c with parent_name = name } in
  let c' = if name = "func" || name = "start" then { c' with is_func = true } else c' in

  (* HARDCODE: Wasm expression *)
  match name with
  | "expr" ->
    let out =
      match c.parent_case with
      | "FUNC" | "GLOBAL" | "ELEM" -> estimate_out !type_cache !types_cache
      | "ACTIVE" -> [ T (nullary "I32") ]
      | _ -> [ TopT ]
    in
    gen_wasm_expr c' [] out out
  (* HARDCODE: name *)
  | "name" -> Al.Ast.TextV (choose ["a"; "b"; "c"] ^ choose ["1"; "2"; "3"])
  (* HARDCODE: memidx to be always 0 *)
  | "memidx" -> numV 0L
  (* HARCODE: typeidx of function is already cached *)
  | "typeidx" when c.parent_case = "FUNC" ->
    List.nth !tids_cache c.i |> numV_of_int |> do_cache type_cache
  (* HARDCODE: c_vectype to be string *)
  | "c_vectype" ->
    gen_vector ()
  (* HARDCODE: pack_size to be 8/16/32 *)
  | "n" when List.mem c.parent_case
    [ "LOAD"; "STORE"; "EXTEND"; "VLOAD_LANE"; "VSTORE_LANE" ] ->
      numV_of_int (choose [8; 16; 32])
  | _ ->
    let syn = find_syn name in
    let result =
      match syn.it with
      | AliasT typ -> gen_typ c' typ
      (* TupV *)
      | NotationT ([[]; []; []], typs) -> Al.Ast.TupV (gen_typs c' typs)
      (* limits *)
      | NotationT ([[LBrack]; [Dot2]; [RBrack]], typs) ->
        let pair = gen_typs c' typs in
        let fst = List.hd pair in
        let snd = List.hd (List.tl pair) in
        let new_snd = map2 unwrap_numv numV Int64.add fst snd in
        Al.Ast.TupV [ fst; new_snd ]
      (* Shape *)
      | NotationT ([[]; [Atom "X"]; []], typs) when name = "shape" ->
        let shape = Al.Ast.TupV (gen_typs c' typs) in
        validate_shape shape
      (* packshape *)
      | NotationT ([[]; [Atom "X"]; []], _) when name = "packshape" ->
        (* TODO: Hardcode packshape *)
        let packshape = choose [ [32L; 2L]; [16L; 4L]; [8L; 8L] ] in
        tupV (List.map numV packshape)
      (* CaseV *)
      | NotationT ((Atom atomid :: _) :: _, typs)
      | NotationT ([[]; [Atom atomid]], typs) (* Hack for I8 *) ->
        let c'' = { c' with parent_case = atomid } in
        let args =
          typs
          |> gen_typs c''
          (* Regenerated deferred wasm funcs *)
          |> List.map (gen_wasm_funcs c'')
        in
        Al.Ast.CaseV (atomid, args)
      (* InfixE *)
      | NotationT ([[]; [Arrow]; []], typs)
      | NotationT ([[]; [Star; Arrow]; [Star]], typs) ->
        let pair = gen_typs c' typs in
        Al.Ast.TupV pair
      (* StrV *)
      | StructT typfields ->
        let rec_ =
          List.fold_right (fun typefield ->
            let atom, (_, typ, _), _ = typefield in
            Record.add (string_of_atom atom) (gen_typ c' typ)
          ) typfields Record.empty
        in
        Al.Ast.StrV rec_
      (* CaseV *)
      (* HARDCODE: Wasm instruction *)
      | VariantT typcases when name = "instr" ->
        (* HARDCODE: checks if currently in a context that requires const instrution *)
        let const_required = List.mem c.parent_case !const_ctxs in
        let const_filter (atom, _, _) = const_required --> List.mem (string_of_atom atom) !consts in
        let block_filter (atom, _, _) =
          (c.depth_limit <= 0) --> not (List.mem (string_of_atom atom) [ "BLOCK"; "LOOP"; "IF" ])
        in
        let typcases' = typcases |> List.filter const_filter |> List.filter block_filter in
        let rec try_instr life =
          if life = 0 then raise OutOfLife;
          let atom, (_, typs, _), _ = choose typcases' in
          let case = string_of_atom atom in
          let t1, t2, entangles = Record.find case !rts in
          let rt1, rt2, induced_args = fix_rts case const_required t1 t2 entangles in
          let valid_rts =
            poppable rt1 && (
              let cur_stack, target_stack_opt, _, n = updated_stack rt1 rt2 (List.hd !expr_info_stack) in
              let d = edit_dist cur_stack target_stack_opt in
              (* Check if Random.float <= n / (n+d), but in smarter way *)
              d = 0 || Random.int (n + d) < n
            )
          in
          if not valid_rts then try_instr (life - 1)
          else
            let rec try_args life' =
              if life' = 0 then try_instr (life - 1)
              else (
                let c'' = { c' with parent_case = case; depth_limit = c'.depth_limit - 1 } in
                let args =
                  (match case with
                  | "BLOCK" -> [ gen c'' "blocktype"; gen_wasm_expr c'' rt1 rt2 rt2 ]
                  | "LOOP" -> [ gen c'' "blocktype"; gen_wasm_expr c'' rt1 rt2 rt1 ]
                  | "IF" -> [
                    gen c'' "blocktype";
                    gen_wasm_expr c'' (hds rt1) rt2 rt2;
                    gen_wasm_expr c'' (hds rt1) rt2 rt2
                  ]
                  | _ -> gen_typs c'' typs
                  ) |> replace induced_args
                  in
                match validate_instr case args const_required (rt1, rt2) with
                | None -> try_args (life' - 1)
                | Some args' ->
                  update_rt rt1 rt2;
                  (*TODO: Perhaps automate this? *)
                  if List.mem name ["RETURN"; "BR"; "BR_TABLE"; "UNREACHABLE"] then
                    nullify_target ();
                  Al.Ast.CaseV (case, args')
              )
            in
            try_args 100
        in
        try_instr 100
      | VariantT typcases ->
        let typcases = List.filter (fun (atom, _, _) -> atom <> Il.Ast.Atom "BOT") typcases in
        (* let typcases = List.filter (fun (atom, _, _) -> atom <> Il.Ast.Atom "EXTERNREF") typcases in *)
        let typcase = choose typcases in
        let atom, (_, typs, _), _ = typcase in
        let case = string_of_atom atom in
        let args = gen_typs { c' with parent_case = case } typs in
        Al.Ast.CaseV (case, args)
      | _ ->
        failwith (
          Printf.sprintf "TODO: gen of %s: %s"
            (Il.Print.string_of_deftyp syn) (Il.Print.structured_string_of_deftyp syn)
        )
    in

    result
    |> cache_if (
      name = "globaltype" || name = "reftype" && c.parent_case = "ELEM"
    ) type_cache
    |> append_cache_if (name = "funcidx" && not c.is_func) refs_cache

and gen_wasm_expr c rt1 rt2 label =
  let max_life = 100 in
  let rec try_expr life =
    if life = 0 then
      let l = List.map (fun _ -> nullary "DROP") rt1 @ List.map default rt2 in
      listV_of_list l
    else
      let n = Random.int 5 + 1 (* 1, 2, 3, 4, 5 *) in
      (* TODO: make input optional? *)
      push (List.rev rt1, Some (List.rev rt2), label, n) expr_info_stack;
      try
        let l = List.init n (fun i -> gen { c with i = i } "instr") in
        pop expr_info_stack;
        listV_of_list l
      with
        OutOfLife -> pop expr_info_stack; try_expr (life - 1)
  in
  try_expr max_life

and gen_wasm_funcs c = function
  | CaseV ("DEFERRED_FUNCS", []) ->
    let l = List.init (List.length !tids_cache) (fun i -> gen { c with i = i } "func") in
    listV_of_list l
  | v -> v

and gen_typ c typ =
  match typ.it with
  (* HARDCODE: imported builtins *)
  | IterT ({ it = VarT id; _ }, List) when id.it = "import" ->
    let import name kind t =
      Al.Ast.CaseV ("IMPORT", [ TextV "spectest"; TextV name; caseV (kind, [t])])
    in
    let const = some "MUT" in
    listV_of_list [
      (* import "print" "FUNC" zero; *)
      import "global_i32" "GLOBAL" (TupV [const; nullary "I32"]);
      import "global_i64" "GLOBAL" (TupV [const; nullary "I64"]);
      import "global_f32" "GLOBAL" (TupV [const; nullary "F32"]);
      import "global_f64" "GLOBAL" (TupV [const; nullary "F64"]);
      import "table" "TABLE" (TupV [ TupV [ NumV 10L; NumV 20L ]; nullary "FUNCREF" ]);
      (* import "memory" "MEM" (CaseV ("I8", [ TupV [ NumV 1L; NumV 2L ] ])); *)
    ]
  (* HARDCODE: export functios *)
  | IterT ({ it = VarT id; _ }, List) when id.it = "export" ->
    let l =
      List.init (List.length !tids_cache) (fun i ->
        let funcidx = numV_of_int i in
        refs_cache := funcidx :: !refs_cache;
        caseV ("EXPORT", [TextV ("f" ^ string_of_int i); caseV ("FUNC", [funcidx])])
      )
    in
    listV_of_list l
  (* General types *)
  | VarT id -> gen c id.it
  | NumT NatT when c.parent_case = "SPLAT" -> numV (choose [8L; 16L; 32L])
  | NumT NatT when c.parent_case = "ZERO" -> numV (choose [32L; 64L])
  | NumT NatT -> numV_of_int (Random.int 3) (* 0, 1, 2 *)
  | IterT (typ', List) ->
    let name = match typ'.it with VarT id -> id.it | _ -> "" in
    let n =
      match name with
      | "table" | "data" | "elem" | "type" | "func" | "global" | "mem" -> Random.int 3 + 3 (* 3, 4, 5 *)
      | _ -> Random.int 3 (* 0, 1, 2 *)
    in
    (* Hardcode: Defer generating functions *)
    if name = "func" then (
      tids_cache := List.init n (fun _ -> Random.int (List.length !types_cache));
      nullary "DEFERRED_FUNCS"
    )
    else
      let l = List.init n (fun i -> gen_typ { c with i = i } typ') in
      if name = "type" then types_cache := l;
      if name = "local" then locals_cache := l;
      if name = "table" then tables_cache := l;
      if name = "global" then globals_cache := l;
      if name = "elem" then elems_cache := l;
      listV_of_list l
  | TupT typs -> TupV (List.map (gen_typ c) typs)
  | IterT (typ', Opt) ->
    if Random.bool() then optV None
    else optV (Some (gen_typ c typ'))
  | _ -> failwith ("TODO: unhandled type for gen_typ: " ^ Il.Print.structured_string_of_typ typ)

and gen_typs c typs =
  match typs.it with
  | TupT typs' -> List.map (gen_typ c) typs'
  | _ -> [ gen_typ c typs ]

and fix_rts case const_required rt1 rt2 entangles =
  let open Al.Ast in
  let bot = [ BotT ], [], [] in

  if List.mem case [ "BLOCK"; "LOOP"; "IF" ] then
    let i32_opt = if case = "IF" then [ T (nullary "I32") ] else [] in
    let bt = gen default_context "blocktype" in
    let pair = [ 0, bt ] in
    match bt with
    | CaseV ("_RESULT", [ OptV None ]) -> i32_opt, [], pair
    | CaseV ("_RESULT", [ OptV (Some t) ]) -> i32_opt, [ T t ], pair
    | CaseV ("_IDX", [ tid ]) ->
      let rt1', rt2' = get_type !types_cache (unwrap_numv_to_int tid) in
      rt1' @ i32_opt, rt2', pair
    | _ -> failwith "Unreachable (Are you using Wasm 1 or Wasm 3?)"

  else if case = "REF.FUNC" then
    if List.length !refs_cache = 0 then bot
    else rt1, rt2, [ 0, choose !refs_cache ]

  (*TODO: Perhaps automate this? *)
  else if List.mem case [ "LOCAL.GET"; "LOCAL.SET"; "LOCAL.TEE" ] then
    let params = !type_cache |> unwrap_numv_to_int |> get_type !types_cache |> fst in
    let locals = List.map (fun l -> T (casev_nth_arg 0 l)) !locals_cache in
    let ls = params @ locals in
    if ls = [] then bot
    else
      let lid = Random.int (List.length ls) in
      let subst = function SubT _ -> List.nth ls lid | t -> t in
      List.map subst rt1, List.map subst rt2, [ 0, numV_of_int lid ]

  else if List.mem case [ "GLOBAL.GET"; "GLOBAL.SET" ] then (*TODO: Perhaps automate this? *)
    let no_mut = none "MUT" in
    let gs =
      List.map (function
        | CaseV ("GLOBAL", [ TupV [ m; t ]; _ ]) -> m <> no_mut, t
        | _ -> failwith "Unreachable: Global"
      ) !globals_cache
    in
    let g x = false, nullary x in
    let gs_builtins = [ g "I32"; g "I64"; g "F32"; g "F64" ] in
    let gs' = if const_required then gs_builtins else gs_builtins @ gs in
    let gids = find_index_all (fun (is_mut, _) -> (case = "GLOBAL.SET") --> is_mut) gs' in
    if gids = [] then bot
    else
      let gid = choose gids in
      let subst = function SubT _ -> T (snd (List.nth gs' gid)) | t -> t in
      List.map subst rt1, List.map subst rt2, [ 0, numV_of_int gid ]

  else if List.mem case [ "TABLE.GET"; "TABLE.SET"; "TABLE.GROW"; "TABLE.FILL" ] then
    let tid, table = choosei !tables_cache in
    let rt =
      match table with
      | CaseV ("TABLE", [ TupV [ _; rt ] ] ) -> rt
      | _ -> failwith "Unreachable: Table"
    in
    let subst = function SubT _ -> T rt | t -> t in
    List.map subst rt1, List.map subst rt2, [ 0, numV_of_int tid ]

  else if case = "TABLE.COPY" then
    let get_rt = function
      | CaseV ("TABLE", [ TupV [ _; rt ] ] ) -> rt
      | _ -> failwith "Unreachable: Table"
    in
    let groups = groupi_by get_rt !tables_cache in
    let _, tids = choose groups in
    rt1, rt2, [ 0, numV_of_int (choose tids); 1, numV_of_int (choose tids) ]

  else if case = "TABLE.INIT" then
    let get_rt = function
      | CaseV ("TABLE", [ TupV [ _; rt ] ] )
      | CaseV ("ELEM", rt :: _) -> rt
      | _ -> failwith "Unreachable: Table / Elem"
    in
    let tgroups = groupi_by get_rt !tables_cache in
    let egroups = groupi_by get_rt !elems_cache in
    let tegroups = List.fold_left (fun acc (et, eids) ->
      let f (tt, tids) = if (et = tt) then Some tids else None in
      match List.find_map f tgroups with
      | Some tids -> (tids, eids) :: acc
      | None -> acc
    ) [] egroups in
    if tegroups = [] then bot
    else
      let tids, eids = choose tegroups in
      rt1, rt2, [ 0, numV_of_int (choose tids); 1, numV_of_int (choose eids) ]

  else if case = "RETURN" then
    (* TODO: Signal arbitrary size better *)
    (
      List.init (Random.int 3) (fun _ -> TopT) @ estimate_out !type_cache !types_cache,
      List.init 3 (fun _ -> TopT),
      []
    )

  else if case = "BR" then
    let lid = Random.int (List.length !expr_info_stack) in
    let _, _, t, _ = List.nth !expr_info_stack lid in
    (
      List.init (Random.int 3) (fun _ -> TopT) @ t,
      List.init 3 (fun _ -> TopT),
      [ 0, numV_of_int lid ]
    )

  else if case = "BR_IF" then
    let lid = Random.int (List.length !expr_info_stack) in
    let _, _, t, _ = List.nth !expr_info_stack lid in
    t @ [ T (nullary "I32") ], t, [ 0, numV_of_int lid ]

  else if case = "BR_TABLE" then
    let lid_groups = groupi_by (fun (_, _, t, _) -> t) !expr_info_stack in
    let t, is = choose lid_groups in
    let lids = List.init (Random.int 3) (fun _ -> choose is) in
    let lid = choose is in
    (
      List.init (Random.int 3) (fun _ -> TopT) @ t @ [ T (nullary "I32") ],
      List.init 3 (fun _ -> TopT),
      [ 0, listV_of_list (List.map numV_of_int lids); 1, numV_of_int lid ]
    )

  else if case = "CALL" then
    let fid, tid = choosei !tids_cache in
    let rt1', rt2' = get_type !types_cache tid in
    rt1', rt2', [ 0, numV_of_int fid ]

  else if case = "CALL_INDIRECT" then
    let tables = find_index_all is_func_table !tables_cache in
    if tables = [] then bot else
    let table = choose tables in
    let tid = Random.int (List.length !types_cache) in
    let rt1', rt2' = get_type !types_cache tid in
    rt1' @ [ T (nullary "I32") ], rt2', [ 0, numV_of_int table; 1, numV_of_int (tid + 1) (* move to patch? *) ]

  else
    let cache = ref Record.empty in
    let len_cache = ref Record.empty in
    let update r x v = r := Record.add x v !r; v in
    let get x sub = try Record.find x !cache with _ -> update cache x (gen default_context sub) in
    let get_len x = try Record.find x !len_cache with _ -> update len_cache x (Random.int 3) in

    let rec fix_rt rt =
      List.concat_map (fun vt -> match vt with
        | SubT (x, sub) -> [ T (get x sub) ]
        | SeqT (SubT (x, sub)) -> List.init (get_len x) (fun i -> (List.hd (fix_rt [ SubT (x ^ string_of_int i, sub) ])))
        | SeqT t -> List.init (Random.int 3) (fun _ -> (List.hd (fix_rt [ t ])))
        | t -> [ t ]
      ) rt
    in
    let rt1' = fix_rt rt1 in
    let rt2' = fix_rt rt2 in
    let induced_args = List.map (fun (i, x) -> i, Record.find x !cache) entangles in
    rt1', rt2', induced_args

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
  let addr = strv_access "VALUE" funcinst |> casev_nth_arg 0 in
  let arg_types = try
    Ds.get_store ()
    |> Record.find "FUNC"
    |> unwrap_listv_to_list
    |> (fun l -> List.nth l (unwrap_numv_to_int addr))
    |> strv_access "TYPE"
    |> unwrap_tupv
    |> (fun l -> List.nth l 0)
    |> unwrap_listv_to_list
  with _ -> []
  in
  let args =
    List.map (function
      | Al.Ast.CaseV ("I32", [])
      | Al.Ast.CaseV ("I64", [])
      | Al.Ast.CaseV ("F32", [])
      | Al.Ast.CaseV ("F64", []) as t -> caseV ("CONST", [t; numV_of_int (Random.full_int 0x7fffffff)])
      | Al.Ast.CaseV ("V128", []) as t -> caseV ("VVCONST", [t; gen_vector ()])
      | t -> (* Assumpnion: is ref *) caseV ("REF.NULL", [t])
    ) arg_types
  in
  let invoke = name, args in

  try
    let returns = Interpreter.invoke [ addr; listV_of_list args ] in
    invoke, Ok (unwrap_listv_to_list returns)
  with Exception.Trap as e -> invoke, Error e

let print_assertion ((f, args), result) =
  print_endline (match result with
  | Ok returns -> Printf.sprintf "(assert_return (invoke %s [%s]) [%s])"
    f
    (args |> List.map Al.Print.string_of_value |> String.concat " ")
    (returns |> List.map Al.Print.string_of_value |> String.concat " ")
  | Error Exception.Trap -> Printf.sprintf "(assert_trap (invoke %s [%s]))"
    f
    (args |> List.map Al.Print.string_of_value |> String.concat " ")
  | Error e -> Printf.sprintf "Invocation of %s failed due to %s"
    f
    (Printexc.to_string e))

let get_instant_result m : instant_result =
    let externvals = listV_of_list (
      (* List.init 1 (fun i -> Al.Ast.CaseV ("FUNC", [numV_of_int i])) *)
      List.init 4 (fun i -> Al.Ast.CaseV ("GLOBAL", [numV_of_int i]))
      (* @ List.init 1 (fun i -> Al.Ast.CaseV ("TABLE", [numV_of_int i])) *)
      (* @ List.init 1 (fun i -> Al.Ast.CaseV ("MEM", [numV_of_int i])) *)
    ) in (*TODO *)
    let mm = Interpreter.instantiate [ m; externvals ] in
    let exported_funcs =
      mm
      |> strv_access "EXPORT"
      |> unwrap_listv_to_list
      |> List.filter (fun inst -> inst |> strv_access "VALUE" |> casev_of_case = "FUNC")
    in
    Ok (List.map mk_assertion exported_funcs)
let inject m =
  get_instant_result m

type module_ = Al.Ast.value
type test = module_ * instant_result

(** Output **)

let print_test (m, result) =
  print_endline (string_of_module m);
  (match result with
  | Ok assertions ->
    print_endline "Instantiation success";
    List.iter print_assertion assertions
  | Error Exception.Trap -> print_endline ("Instantiation failed")
  | Error Exception.Exhaustion -> print_endline ("Infinite loop in instantiation")
  | Error e ->
    Printf.printf "Unexpected error during instantiation: %s" (Printexc.to_string e)
  );
  print_endline "================"

let to_phrase x = Reference_interpreter2.Source.(x @@ no_region)

let value_to_wast v =
  let open Reference_interpreter2 in
  let open Script in
  let open Values in

  let f32_pos_nan = F32.to_bits F32.pos_nan in
  let f32_neg_nan = F32.to_bits F32.neg_nan |> Int32.logand 0x0000_0000_ffff_ffffl in
  let f64_pos_nan = F64.to_bits F64.pos_nan in
  let f64_neg_nan = F64.to_bits F64.neg_nan in

  match v with
  | Al.Ast.CaseV ("REF.FUNC_ADDR", _) -> RefResult (RefTypePat FuncRefType) |> to_phrase
  | _ ->
    match Construct2.al_to_value v with
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
  let open Reference_interpreter2.Script in
  let f' = Reference_interpreter.Utf8.decode f in
  let args' = List.map (Construct2.al_to_value %> to_phrase) args in
  let action = Invoke (None, f', args') |> to_phrase in
  (match result with
  | Ok returns -> Some (AssertReturn (action, List.map value_to_wast returns))
  | Error Exception.Exhaustion -> Some (AssertExhaustion (action, ""))
  | Error Exception.Trap -> Some (AssertTrap (action, ""))
  | _ ->
    Printf.sprintf "Unexpected error in invoking %s" f |> prerr_endline;
    None

  ) |> Option.map (fun a -> Assertion (to_phrase a) |> to_phrase)

let to_wast m result =
  let open Reference_interpreter2.Script in
  let m_r = Construct2.al_to_module m in

  let def = Textual m_r |> to_phrase in
  let script =
    match result with
    | Ok assertions ->
      (Module (None, def) |> to_phrase)
      :: List.filter_map invoke_to_wast assertions
    | Error Exception.Trap ->
      [ Assertion (AssertUninstantiable (def, "") |> to_phrase) |> to_phrase ]
    | Error Exception.Exhaustion -> []
    | _ ->
      Printf.sprintf "Unexpected error in instantiating module" |> prerr_endline;
      []
  in

  let file = Filename.concat !Flag.out (string_of_int !seed ^ ".wast") in
  let oc = open_out file in
  (* Print.module_ oc 80 m_r; *)
  Reference_interpreter2.Print.script oc 80 `Textual script;
  close_out oc
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

  (* Initialize *)
  Random.init !seed;
  Ds.init !al;
  rts := List.map get_rt (get_typing_rules !il);
  estimate_const ();

  (* Generate tests *)
  init_cache ();
  let module_ = gen default_context "module" in

  (* Mutatiion *)
  let module_ = patch module_ in

  (* TODO *)
  Builtin.builtin () |> ignore;

  (* Injection *)
  let result = inject module_ in

  (* Convert to Wast *)
  to_wast module_ result


  (* Print Coverage *)
  (* Ds.(Info.print (InfoMap.uncovered !info_map)) *)

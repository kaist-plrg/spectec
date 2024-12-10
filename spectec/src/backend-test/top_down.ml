open Langs
open Utils
open Valid

open Util.Source
open Util
open Al.Al_util

(* Helpers *)
let hds xs = xs |> List.rev |> List.tl |> List.rev

let option_flatmap f opt = Option.bind opt f

let (-->) p q = (not p) || q

let spf = Printf.sprintf

let version = Flag.version

(** Helpers to handle type-family-based generation **)
  let has_name name def =
    match def.it with
    | Il.Ast.TypD (id, _params, insts) when id.it = name -> Some insts
    | _ -> None
  let type_of_exp e =
    match e.it with
    | Il.Ast.SubE (_, t, _) -> t
    | _ -> e.note
  let type_of_arg a =
    match a.it with
    | Il.Ast.ExpA e -> type_of_exp e
    | Il.Ast.TypA t -> t
    | Il.Ast.DefA _ -> failwith "TODO"
    | Il.Ast.GramA _ -> failwith "TODO"
  let typ_of_bind bind =
    match bind.it with
    | Il.Ast.ExpB (_, t) -> t
    | Il.Ast.TypB _
    | Il.Ast.DefB _
    | Il.Ast.GramB _ -> failwith "typ_of_bind"

  let do_binds alist vlist =
    let rec do_bind e v =
      match e.it, v with
      | Il.Ast.VarE id, _ -> [ id.it, v ]
      | Il.Ast.SubE (e, _, _), _ -> do_bind e v
      | Il.Ast.CaseE ([[];[]], { it = TupE [e]; _}), _ -> do_bind e v
      | Il.Ast.CaseE (_, { it = TupE es; _}),
        (Al.Ast.CaseV (_, vs) | Al.Ast.TupV vs) ->
          List.map2 do_bind es vs |> List.concat
      | _ -> failwith (spf "TODO: do_bind %s %s" (Il.Print.string_of_exp e) (Al.Print.string_of_value v))
    in

    let do_bind_arg a kv =
      match a.it with
      | Il.Ast.ExpA e -> do_bind e (snd kv)
      | Il.Ast.TypA _ -> failwith "do_bind: arg is TypA"
      | Il.Ast.DefA _ -> failwith "do_bind: arg is DefA"
      | Il.Ast.GramA _ -> failwith "do_bind: arg is GramA"
    in

    List.map2 do_bind_arg alist vlist |> List.concat

  exception DispatchFail of string

  let rec has_deftyp v dt =
    (* print_endline (spf "has_deftype %s %s ?" (Al.Print.string_of_value v) (Il.Print.string_of_deftyp `H dt)); *)
    match v, dt.it with
    | Al.Ast.CaseV (name, []), Il.Ast.VariantT typcases ->
      List.exists (fun (mixop, _, _) -> name = Il.Print.string_of_mixop mixop) typcases
    | _, Il.Ast.AliasT t -> has_type v t
    | _, Il.Ast.VariantT [ [[]; []], ([ bind ], _, _), _ ] -> has_type v (typ_of_bind bind)
    (* HARDCODE: N x M *)
    | Al.Ast.CaseV ("X", vs), Il.Ast.VariantT [ typcase ] ->
      let (_mixop, (binds, _, _), _) = typcase in
      (* TODO: assert mixop = `%X%` *)
      List.for_all2 has_type vs (List.map typ_of_bind binds)
    | _ -> false
  and has_type v t =
    (* print_endline (spf "has_type %s %s ?" (Al.Print.string_of_value v) (Il.Print.string_of_typ t)); *)
    match v, t.it with
    | Al.Ast.NumV _, Il.Ast.(NumT NatT) -> true
    | _, Il.Ast.VarT (name, []) -> has_deftyp v (dispatch_deftyp name.it [] |> fst)
    | _ -> false
  and has_argtype v a =
    (* print_endline (spf "has_argtype %s %s ?" (Al.Print.string_of_value v) (Il.Print.string_of_arg a)); *)
    has_type v (type_of_arg a)

  and match_params args inst =
    match inst.it with
    | Il.Ast.InstD (_binds, params, deftyp) when (
        List.for_all2 has_argtype (List.map snd args) params
      ) -> Some (deftyp, do_binds params args)
    | _ -> None
  and dispatch_deftyp name args =
    match List.find_map (has_name name) !il with
    | Some insts ->
      ( match List.find_map (match_params args) insts with
        | Some matched -> matched
        | None -> raise (DispatchFail name) )
    | None -> failwith (Printf.sprintf "The syntax named %s does not exist in the input spec" name)
(** End of Helpers to handle type-family-based generation **)

let string_of_atom = El.Atom.to_string
let string_of_mixop = Il.Mixop.to_string

let flatten_args e = match e.it with
| Il.Ast.TupE es -> es
| _ -> [ e ]

let nth_typ typs n =
  match typs.it with
  | Il.Ast.TupT ts -> List.nth ts n |> snd
  | _ -> List.nth [typs] n

let replace ixs = List.mapi (fun i x -> match List.assoc_opt i ixs with Some x' -> x' | None -> x)

(** Initialize **)

let rts = ref Record.empty

let print_rts () =
  Record.iter (fun k v ->
    let (rt1, rt2, _) = v in
    Printf.sprintf "%s : %s -> %s" k (string_of_rt rt1) (string_of_rt rt2) |> print_endline
  ) !rts

let get_rt rule =
  let open Il.Ast in
  let rec estimate_rt e =
    match e.it with
    (* Propagte*)
    | CaseE ([[]; []], e')
    | TupE [e'] -> estimate_rt e'
    (* Concrete type *)
    | CaseE ([[atom]], { it = TupE []; _}) -> [ T (nullary (string_of_atom atom)) ]
    (* List type *)
    | ListE es -> List.concat_map estimate_rt es
    | CatE (e1, e2) -> estimate_rt e1 @ estimate_rt e2
    (* Var type *)
    | VarE id -> [ SubT (id.it, Il.Print.string_of_typ e.note) ]
    | SubE ({ it = VarE id; _ }, { it = VarT (id', _); _ }, _)  -> [ SubT (id.it, id'.it) ]
    | SubE (
      { it = CallE (name, _); _},
      { it = VarT (id, _); _ },
      _
    ) when name.it = "unpacked" -> [ SubT (id.it, id.it) ]
    (* Iter type *)
    | IterE (e', (List, _)) -> [ SeqT (List.hd (estimate_rt e')) ]
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
    | CaseE (_, args) ->
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

(** Seed generation **)

type context = {
  i: int;               (* Denote generating i-th value for IterT *)
  parent_case: string;  (* Denote case of most recent parent CaseV *)
  parent_name: string;  (* Denote name of most recent production *)
  depth_limit: int;     (* HARDCODE: Limit on block / loop / if depth *)
  is_func: bool;        (* HARDCODE: currently making function *)
  args: (string * Al.Ast.value) list (* Arguments for syntax *)
}
let default_context = {
  i = 0;
  parent_case = "";
  parent_name = "";
  depth_limit = 3;
  is_func = false;
  args = [];
}

let types_cache = ref []
let type_cache = ref zero
let locals_cache = ref []
let tids_cache = ref []
let globals_cache = ref []
let tables_cache = ref []
let elems_cache = ref []
let refs_cache = ref []

let init_cache () =
  types_cache := [];
  type_cache := zero;
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

let flatten_types = List.concat_map (fun rectype ->
  rectype
  |> casev_nth_arg 0
  |> casev_nth_arg 0
  |> unwrap_listv_to_list
  |> List.map (casev_nth_arg 2)
)

let get_type types tid =
  match !version with
  | 2 ->
    let arrow = List.nth types tid |> casev_nth_arg 0 in
    let f i =
      casev_nth_arg i arrow
      |> unwrap_listv_to_list
      |> List.map (fun v -> T v) in
    f 0, f 1
  | 3 ->
    let comptypes = flatten_types types in
    let comptype = List.nth comptypes tid in
    (* Assert: comptype is FUNC *)
    let arrow = comptype |> casev_nth_arg 0 in
    let f i =
      casev_nth_arg i arrow
      |> unwrap_listv_to_list
      |> List.map (fun v -> T v) in
    f 0, f 1
  | _ -> failwith "Unsupported version"

(* get output type of t *)
let estimate_out t types =
  let open Al.Ast in
  match t with
  | NumV n ->
    n
    |> Z.to_int
    |> get_type types
    |> snd
  | TupV [ CaseV ("MUT", _); v ] | v -> [ T v ]

let is_func_table = function
  | Al.Ast.CaseV ("TABLE", [ TupV [ _; CaseV ("FUNCREF", []) ]]) -> true
  | _ -> false

let enforce_func_type types =
  assert (!version = 3);
  let comptypes = flatten_types types in
  if List.exists (fun t -> casev_get_case t = "FUNC") comptypes then
    types
  else
    (
      CaseV ("SUB", [
        unary "FINAL" noneV;
        empty_list;
        unary "FUNC" (CaseV ("->", [empty_list; empty_list]))
      ])
      |> singleton
      |> unary "REC"
      |> unary "TYPE"
    ) :: types

let choose_type_idx kind types =
  types
  |> flatten_types
  |> List.mapi (fun i x -> i, x)
  |> List.filter (fun (_, x) -> casev_get_case x = kind)
  |> choose
  |> fst
let choose_func_type_idx = choose_type_idx "FUNC"
let choose_struct_type_idx = choose_type_idx "STRUCT"
let choose_array_type_idx = choose_type_idx "ARRAY"

exception OutOfLife

(* Generate specific syntax from input IL *)
let rec gen c name =
  let c' = { c with parent_name = name } in
  let c' = if name = "func" || name = "start" then { c' with is_func = true } else c' in
  let c' = { c' with args = [] } in

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
  (* HARDCODE: memidx to be always 0 for wasm 2.0 *)
  | "memidx" -> zero
  (* HARDCODE: typeidx of function is already cached *)
  | "typeidx" when c.parent_case = "FUNC" ->
    List.nth !tids_cache c.i |> numV_of_int |> do_cache type_cache
  (* HARDCODE: vN to be 16 bytes (128 bits) *)
  | "vN" -> numV (gen_bytes 16)
  (* HARDCODE: pack_size to be 8/16/32/64 *) (* TODO: Generalize this *)
  | "sz" -> numV_of_int (choose [8; 16; 32; 64])
  | _ ->
    let deftyp, bindings = dispatch_deftyp name c.args in
    let c' = { c' with args = bindings } in
    let result =
      match deftyp.it with
      | AliasT typ -> gen_typ c' typ
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
        (* Filters for preventing certain wasm instructions to be generated *)
        (* HARDCODE: checks if currently in a context that requires const instrution *)
        let const_required = List.mem c.parent_case !Langs.const_ctxs in
        let get_winstr_name mixop = string_of_atom (mixop |> List.hd |> List.hd) in
        let const_filter (mixop, _, _) = const_required --> List.mem (get_winstr_name mixop) !Langs.consts in
        let block_filter (mixop, _, _) =
          (c.depth_limit <= 0) --> not (List.mem (get_winstr_name mixop) [ "BLOCK"; "LOOP"; "IF" ])
        in
        let admin_filter (mixop, _, _) = not (List.mem (get_winstr_name mixop) [
          "REF.I31_NUM";
          "REF.STRUCT_ADDR";
          "REF.ARRAY_ADDR";
          "REF.FUNC_ADDR";
          "REF.EXN_ADDR";
          "REF.HOST_ADDR";
          "REF.EXTERN";
          "LABEL_";
          "FRAME_";
          "HANDLER_";
          "TRAP";
        ]) in
        (* End of filters *)
        let typcases' = typcases |> List.filter const_filter |> List.filter block_filter |> List.filter admin_filter in
        let rec try_instr life =
          if life = 0 then raise OutOfLife;
          let mixop, (_, typs, _), _ = choose typcases' in
          let case = get_winstr_name mixop in
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
              else try (
                let c'' = { c' with parent_case = case; depth_limit = c'.depth_limit - 1 } in
                let args =
                  (match case with
                  | "BLOCK" -> [ gen c'' "blocktype"; gen_wasm_expr c'' rt1 rt2 rt2 ]
                  | "LOOP" -> [ gen c'' "blocktype"; gen_wasm_expr c'' rt1 rt2 rt1 ]
                  | "IF" -> [
                    gen c'' "blocktype";
                    gen_wasm_expr c'' (hds rt1) rt2 rt2;
                    gen_wasm_expr c'' (hds rt1) rt2 rt2]
                  | "TRY_TABLE" -> [
                    gen_typ c'' (nth_typ typs 0);
                    gen_typ c'' (nth_typ typs 1);
                    gen_wasm_expr c'' rt1 rt2 rt2]
                  | _ -> gen_typs c'' ~fixed:induced_args typs
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
              ) with DispatchFail ("testop_" | "vtestop_" | "loadop_" | "half__") -> try_args (life' - 1) (* Unhabite testop for TESTOP Fxx _ *)
            in
            try_args 100
        in
        try_instr 100
      | VariantT typcases ->
        let typcases = Lib.List.filter_not (has_subid_hint "sem") typcases in
        let typcase = choose typcases in
        gen_typcase c' typcase
    in

    result
    |> cache_if (
      name = "globaltype" || name = "reftype" && c.parent_case = "ELEM"
    ) type_cache
    |> append_cache_if (name = "funcidx" && not c.is_func) refs_cache

and gen_typcase c (mixop, (_binds, typs, _prems), _hint) =
  let open El.Atom in
  match mixop with
    (* Propagation *)
    | [[]; []] -> gen_typs c typs |> List.hd
    (* TupV *)
    | [[]; []; []] ->
      Al.Ast.TupV (gen_typs c typs)
    (* limits *)
    | [[{ it = LBrack; _}]; [{ it = Dot2; _}]; [{it = RBrack; _}]] ->
      let pair = gen_typs c typs in
      let fst = List.hd pair in
      let snd = List.hd (List.tl pair) in
      (* Make snd larger than fst *)
      let new_snd = map2 unwrap_numv numV Z.add fst snd in
      Al.Ast.CaseV ("[", [ fst; new_snd ])
    (* Shape *)
    | [[]; [{it = Atom "X"; _}]; []] ->
      let shape = Al.Ast.CaseV ("X", (gen_typs c typs)) in
      validate_shape shape
    (* CaseV *)
    | ({it = Atom atomid; _} :: _) :: _
    | [[]; [{it = Atom atomid; _}]] (* Hack for I8 *) ->
      let c' = { c with parent_case = atomid } in
      let args =
        typs
        |> gen_typs c'
        (* Regenerate deferred wasm funcs *)
        |> List.map (gen_if_wasm_funcs c')
      in
      Al.Ast.CaseV (atomid, args)
    | _ ->
      let case = mixop |> Al.Al_util.get_atom |> Option.get |> string_of_atom in
      let args = gen_typs { c with parent_case = case } typs in
      Al.Ast.CaseV (case, args)

and gen_wasm_expr c rt1 rt2 label =
  let max_life = 100 in
  let rec try_expr life =
    if life = 0 then (
      Log.debug ("Out of life during genrating expr: " ^ string_of_rt rt1 ^ " -> " ^ string_of_rt rt2);
      let l = List.map (fun _ -> nullary "DROP") rt1 @ List.map default rt2 in
      listV_of_list l )
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

and gen_if_wasm_funcs c = function
  | CaseV ("DEFERRED_FUNCS", []) ->
    let l = List.init (List.length !tids_cache) (fun i -> gen { c with i = i } "func") in
    listV_of_list l
  | v -> v

and gen_typ c typ =
  match typ.it with
  (* HARDCODE: imported builtins *)
  | IterT ({ it = VarT (id, _); _ }, List) when id.it = "import" ->
    let import name kind t =
      Al.Ast.CaseV ("IMPORT", [ TextV "spectest_values"; TextV name; caseV (kind, [t])])
    in
    let const = none "MUT" in
    listV_of_list [
      (* import "print" "FUNC" zero; *)
      import "global_i32" "GLOBAL" (TupV [const; nullary "I32"]);
      import "global_i64" "GLOBAL" (TupV [const; nullary "I64"]);
      import "global_f32" "GLOBAL" (TupV [const; nullary "F32"]);
      import "global_f64" "GLOBAL" (TupV [const; nullary "F64"]);
      (* import "table" "TABLE" (TupV [ TupV [ NumV 10L; NumV 20L ]; nullary "FUNCREF" ]); *)
      (* import "memory" "MEM" (CaseV ("I8", [ TupV [ NumV 1L; NumV 2L ] ])); *)
    ]
  (* HARDCODE: export functios *)
  | IterT ({ it = VarT (id, _); _ }, List) when id.it = "export" ->
    let l =
      List.init (List.length !tids_cache) (fun i ->
        let funcidx = numV_of_int i in
        refs_cache := funcidx :: !refs_cache;
        caseV ("EXPORT", [TextV ("f" ^ string_of_int i); caseV ("FUNC", [funcidx])])
      )
    in
    listV_of_list l
  (* HARDCODE: list *)
  | VarT (id, [ { it = TypA typ'; _ } ]) when id.it = "list" ->
    let it = Il.Ast.IterT (typ', List) in
    gen_typ c { typ with it = it }
  (* General types *)
  | VarT (id, args) ->
    (* Helpers *)
    let rec e2v e =
      match e.it with
      | Il.Ast.NatE z -> numV z
      | Il.Ast.SubE (e, _, _) -> e2v e
      | Il.Ast.VarE id -> List.assoc id.it c.args
      | Il.Ast.CaseE (mixop, {it = TupE args; _}) ->
        let case = match get_atom mixop with Some atom -> string_of_atom atom | _ -> "" in
        CaseV (case, List.map e2v args)
      (* HARDCODE *)
      | Il.Ast.CallE (id, [ vt ]) when List.mem id.it ["size"; "sizenn"; "vsize"] ->
        ( match casev_get_case (a2v vt) with
        | "I32" -> 32
        | "I64" -> 64
        | "F32" -> 32
        | "F64" -> 64
        | "V128" -> 128
        | _ -> failwith "Invalid size" ) |> numV_of_int
      | _ -> failwith ("Can not convert ExpA " ^ (Il.Print.string_of_exp e) ^ " into value (yet)")
    and a2v a =
      match a.it with
      | Il.Ast.ExpA e -> e2v e
      | Il.Ast.TypA _ -> failwith "Can not convert TypA into value (yet)"
      | Il.Ast.DefA _ -> failwith "Can not convert DefA into value (yet)"
      | Il.Ast.GramA _ -> failwith "Can not convert GramA into value (yet)" in
    (* End of helpers*)
    let args' = List.map (fun a -> ("_arg", a2v a)) args in
    let c' = { c with args = args' } in
    gen c' id.it
  | NumT NatT -> numV_of_int (Random.int 3) (* 0, 1, 2 *)
  | IterT (typ', List) ->
    let name = match typ'.it with VarT (id, _) -> id.it | _ -> "" in
    let n =
      match name with
      | "table" | "data" | "elem" | "type" | "func" | "global" | "mem" -> Random.int 3 + 3 (* 3, 4, 5 *)
      | "byte" -> Random.int 3 + 1 (* 1, 2, 3, HACK for wasmtime *)
      | "typeuse" -> 0 (* HARDCODE: Disable generating subtype for now *)
      | _ -> Random.int 3 (* 0, 1, 2 *)
    in
    (* Hardcode: Defer generating functions *)
    if name = "func" then (
      tids_cache := (
        match !version with
        | 2 -> List.init n (fun _ -> Random.int (List.length !types_cache));
        | 3 -> List.init n (fun _ -> choose_func_type_idx !types_cache);
        | _ -> failwith "Unsupported version"
      );
      nullary "DEFERRED_FUNCS"
    )
    else
      let l = List.init n (fun i -> gen_typ { c with i = i } typ') in
      (* Ensure that there is at least one func type *)
      let l = if name = "type" && !version = 3 then enforce_func_type l else l in
      (* Store these module infos into chache *)
      if name = "type" then types_cache := l;
      if name = "local" then locals_cache := l;
      if name = "table" then tables_cache := l;
      if name = "global" then globals_cache := l;
      if name = "elem" then elems_cache := l;
      listV_of_list l
  | TupT typs -> TupV (typs |> List.map snd |> List.map (gen_typ c))
  | IterT (typ', Opt) ->
    if Random.bool() then optV None
    else optV (Some (gen_typ c typ'))
  | _ -> failwith ("TODO: unhandled type for gen_typ: " ^ Il.Print.string_of_typ typ)

and gen_typs c ?(fixed = []) typs =
  match typs.it with
  | TupT typs' ->
      List.fold_left_map (fun (c, i) (exp, typ) ->
        let k = Il.Print.string_of_exp exp in
        let v =
          match List.assoc_opt i fixed with
          | Some v -> v
          | None -> (gen_typ c) typ
        in
        ({ c with args = (k, v) :: c.args }, i+1), v
      ) (c, 0) typs' |> snd
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
    | CaseV ("_RESULT", [ OptV (Some t) ]) ->
      i32_opt, [ T t ], pair
    | CaseV ("_IDX", [ tid ]) when !version = 2 ->
      let rt1', rt2' = get_type !types_cache (unwrap_numv_to_int tid) in
      rt1' @ i32_opt, rt2', pair
    | CaseV ("_IDX", [ _ ]) when !version = 3 ->
      let tid = choose_func_type_idx !types_cache in
      let bt = casev_replace_nth_arg 0 (numV_of_int tid) bt in
      let pair = [0, bt] in
      let rt1', rt2' = get_type !types_cache tid in
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
    let no_mut = caseV ("MUT", [OptV None]) in
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
      | CaseV ("TABLE", [ TupV [ _; rt ] ] )
      | CaseV ("TABLE", [ TupV [ _; rt ]; _ ] ) -> rt
      | _ -> failwith "Unreachable: Table"
    in
    let subst = function SubT _ -> T rt | t -> t in
    List.map subst rt1, List.map subst rt2, [ 0, numV_of_int tid ]

  else if case = "TABLE.COPY" then
    let get_rt = function
      | CaseV ("TABLE", [ TupV [ _; rt ] ] )
      | CaseV ("TABLE", [ TupV [ _; rt ]; _ ] ) -> rt
      | _ -> failwith "Unreachable: Table"
    in
    let groups = groupi_by get_rt !tables_cache in
    let _, tids = choose groups in
    rt1, rt2, [ 0, numV_of_int (choose tids); 1, numV_of_int (choose tids) ]

  else if case = "TABLE.INIT" then
    let get_rt = function
      | CaseV ("TABLE", [ TupV [ _; rt ] ] )
      | CaseV ("TABLE", [ TupV [ _; rt ]; _ ] )
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
    rt1' @ [ T (nullary "I32") ], rt2', [ 0, numV_of_int table; 1, numV_of_int tid ]

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

(* Main entry *)
let gen_module () =
  init_cache ();
  gen default_context "module"


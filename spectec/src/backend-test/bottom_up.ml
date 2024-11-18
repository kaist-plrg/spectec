open Util
open Source

open Langs
(* open Valid *)

(* open Al.Ast *)
(* open Al.Al_util *)
open Il.Ast

open Il2al.Il_walk

(** Helpers **)
let replace old_e new_e e =
  if Il.Eq.eq_exp old_e e then new_e else e

let replace_id old_id new_id e =
  match e.it with
  | VarE id when id.it = old_id -> {e with it = VarE {id with it = new_id}}
  | _ -> e

let replace_id_using f e =
  match e.it with
  | VarE id -> {e with it = VarE {id with it = f (id.it)}}
  | _ -> e

let replace_id_with old_id new_e e =
  match e.it with
  | VarE id when id.it = old_id -> new_e
  | _ -> e

let rec dedup eq = function
| [] -> []
| hd :: tl -> hd :: dedup eq (Lib.List.filter_not (eq hd) tl)

let to_phrase ty x = x $$ no_region % ty

let mk_VarT x = VarT (x $ no_region, []) $ no_region
let il_case name tname args =
  CaseE (
    [El.Atom.Atom name $$ no_region % (El.Atom.info name)] :: (List.map (fun _ -> []) args),
    TupE args $$ no_region % (TupT (List.map (fun a -> (a, a.note)) args) $ no_region)
  ) $$ no_region % (mk_VarT tname)

let mixop_of_case e =
  match e.it with
  | CaseE (mixop, _) -> mixop
  | _ -> failwith (Il.Print.string_of_exp e ^ "is not a CaseE")

let case_of_case e =
  match mixop_of_case e with
  | [atom] :: tl when List.for_all ((=) []) tl -> atom.it
  | _ -> failwith (Il.Print.string_of_exp e ^ "is not a CaseE with single atom")

let args_of_case e =
  match e.it with
  | CaseE (_, {it = TupE args; _}) -> args
  | _ -> failwith (Il.Print.string_of_exp e ^ "is not a CaseE")

let nth_arg_of_case n e =
  match e.it with
  | CaseE (_, {it = TupE args; _}) -> List.nth args n
  | _ -> failwith (Il.Print.string_of_exp e ^ "is not a CaseE")

let rec replace_caseE_arg is it e =
  match is, e.it with
  | [], _ ->
    { e with it }
  | i :: is, CaseE (mixop, ({ it = TupE es; _ } as tup)) ->
    let es' = List.mapi (fun i' e' -> if i = i' then replace_caseE_arg is it e' else e') es in
    { e with it = CaseE (mixop, { tup with it = TupE es' })}
  | 0 :: is, CaseE (mixop, e') ->
    { e with it = CaseE (mixop, replace_caseE_arg is it e') }
  | _ -> failwith "Expected a CaseE"

let exp_to_int e =
  match (Il.Eval.reduce_exp !il_env e).it with
  | NatE z -> Z.to_int z
  | _ -> failwith (Il.Print.string_of_exp e ^ " is not an integer")

(** Helpers to handle type-family-based generation **)
  let has_name name def =
    match def.it with
    | TypD (id, _params, insts) when id.it = name -> Some insts
    | _ -> None
  let type_of_exp e =
    match e.it with
    | SubE (_, t, _) -> t
    | _ -> e.note
  let type_of_arg a =
    match a.it with
    | ExpA e -> type_of_exp e
    | TypA t -> t
    | DefA _ -> failwith "TODO"
    | GramA _ -> failwith "TODO"
  let typ_of_bind bind =
    match bind.it with
    | ExpB (_, t) -> t
    | TypB _
    | DefB _
    | GramB _ -> failwith "typ_of_bind"

  exception DispatchFail of string

  let rec has_deftyp a dt =
    (* print_endline (Printf.sprintf "has_deftype %s %s ?" (Il.Print.string_of_exp a) (Il.Print.string_of_deftyp `H dt)); *)
    match a.it, dt.it with
    | CaseE (mixop, {it = TupE []; _}), VariantT typcases ->
      List.exists (fun (mixop', _, _) -> Il.Mixop.eq mixop mixop') typcases
    | _, AliasT t -> has_type a t
    | _, VariantT [ [[]; []], ([ bind ], _, _), _ ] -> has_type a (typ_of_bind bind)
    (* HARDCODE: N x M *)
    (*
    | Al.Ast.CaseV ("X", as), VariantT [ typcase ] ->
      let (_mixop, (binds, _, _), _) = typcase in
      (* TODO: assert mixop = `%X%` *)
      List.for_all2 has_type as (List.map typ_of_bind binds)\
    *)
    | _ -> false
  and has_type a t =
    (* print_endline (Printf.sprintf "has_type %s %s ?" (Il.Print.string_of_exp a) (Il.Print.string_of_typ t)); *)
    match a.it, t.it with
    | NatE _, (NumT NatT) -> true
    | _, VarT (name, []) -> has_deftyp a (dispatch_deftyp name.it [] |> snd)
    | _ -> Il.Eq.eq_typ a.note t
  and has_argtype a p =
    (* print_endline (Printf.sprintf "has_argtype %s %s ?" (Il.Print.string_of_exp a) (Il.Print.string_of_arg p)); *)
    has_type a (type_of_arg p)

  and match_params args inst =
    match inst.it with
    | InstD (binds, params, deftyp) when (
        List.for_all2 has_argtype args params
      ) -> Some (binds, deftyp)
    | _ -> None
  and dispatch_deftyp name args =
    match List.find_map (has_name name) !il with
    | Some insts ->
      ( match List.find_map (match_params args) insts with
        | Some matched -> matched
        | None -> raise (DispatchFail name) )
    | None -> failwith (Printf.sprintf "The syntax named %s does not exist in the input spec" name)
(** End of Helpers to handle type-family-based generation **)

type context = {
  typ: typ;
  args: arg list;
}

let rec gen c x =
  let a2e a =
    match a.it with
    | ExpA e -> Il.Eval.reduce_exp !Langs.il_env e
    | _ -> failwith "Unsupported arg"
  in
  let args = List.map a2e c.args in
  let binds, deftyp = dispatch_deftyp x args in
  let replace_params = (fun e ->
    List.fold_left2 (fun e b a ->
      match b.it with
      | ExpB (x, _) -> replace_id_with x.it a e
      | _ -> e
    ) e binds args
  ) in
  match deftyp.it with
  | AliasT typ -> gen_typ c (transform_typ replace_params typ);
  | StructT _ -> failwith "StructT not supported"
  | VariantT typcases ->
    let typcases = Lib.List.filter_not (Gen.has_subid_hint "sem") typcases in
    let typcase = Utils.choose typcases in
    let typcase' =
      let (m, (bs, t, ps), hs) = typcase in
      let t' = transform_typ replace_params t in
      m, (bs, t', ps), hs
    in
    gen_typcase c typcase'
and gen_typcase c (mixop, (_binds, typs, _prems), _hint) =
  let args = TupE (gen_typs c typs) |> to_phrase c.typ in
  CaseE (mixop, args) |> to_phrase c.typ
and gen_typs c typs =
  match typs.it with
  | TupT typs' -> List.map (gen_typ c) (List.map snd typs')
  | _ -> [ gen_typ c typs ]
and gen_typ c typ =
  match typ.it with
  | NumT NatT -> NatE (Random.int 3 |> Z.of_int) |> to_phrase typ (* 0, 1, 2 *)
  | VarT (id, args) -> gen {typ; args} id.it
  | IterT (typ', Opt) ->
    if Random.bool() then OptE None |> to_phrase typ
    else OptE (Some (gen_typ c typ')) |> to_phrase typ
  | TupT ets -> TupE (ets |> List.map (fun (_, t) -> gen_typ c t)) |> to_phrase typ
  | _ -> failwith ("TODO: unhandled type for gen_typ: " ^ Il.Print.string_of_typ typ)
let gen_typ typ = gen_typ {typ; args = []} typ

(** End of Helpers **)

type valtype = exp
type restype = valtype list

let trules = ref []

let expected_shape e shape =
  Printf.sprintf "Expected %s to be %s" (Il.Print.string_of_exp e) shape |> failwith

let rule_to_arrow rule =
  let rec unwrap e =
    match e.it with
    | CaseE ([[]; []], e')
    | TupE [e'] -> unwrap e'
    | _ -> e
  in

  let RuleD (id, _, _, exp, _) = rule.it in
  match exp.it with
  | TupE [_c; _lhs; rhs] ->
    (match rhs.it with
    | CaseE (_, args) ->
      (match args.it with
      | TupE [t1; t2] | TupE [t1; _; t2] ->
        let name = id.it |> String.split_on_char '-' |> List.hd |> String.uppercase_ascii in
        name, (unwrap t1, unwrap t2)
      | _ -> expected_shape args "t1, t2 or t1, x*, t2"
      )
    | _ -> expected_shape rhs "e1 -> e2"
    )
  | _ -> expected_shape exp "C |- lhs : rhs"
let arrow_map = ref []

let rule_to_instr rule =
  let RuleD (_, _, _, exp, _) = rule.it in
  match exp.it with
  | TupE [_c; lhs; _rhs] -> lhs
  | _ -> expected_shape exp "C |- lhs : rhs"

let rule_to_prems rule =
  let RuleD (_, _, _, _, prems) = rule.it in
  prems

type sidecond =
  | TypeLenC of int * exp * int
  | RulePrC of (id * mixop * exp)
  | IfPrC of exp
  | TypeCondC of int * (restype * restype)
let sideconds: sidecond list ref = ref []

let as_sidecond pr =
  match pr.it with
  | IfPr e -> [IfPrC e]
  | RulePr (id, mixop, e) -> [RulePrC (id, mixop, e)]
  | _ -> []

let rec unify_vt map e1 e2 =
  let rec resolve e =
    match e.it with
    | VarE x -> (match List.assoc_opt x.it map with Some e -> resolve e | None -> e)
    | _ -> e
  in
  let e1 = resolve e1 in
  let e2 = resolve e2 in
  if Il.Eq.eq_exp e1 e2 then map else
  match e1.it, e2.it with (*TODO: Generalize to more cases *)
  | _, VarE x -> ((x.it, e1) :: map)
  | VarE x, _ -> ((x.it, e2) :: map)
  | CaseE (case1, args1), CaseE (case2, args2) when Il.Mixop.eq case1 case2 -> unify_vt map args1 args2
  | TupE es1, TupE es2 when List.length es1 = List.length es2 -> List.fold_left2 (fun map e1 e2 -> unify_vt map e1 e2) map es1 es2
  | _, _ -> failwith ("Unification fail of " ^ (Il.Print.string_of_exp e1) ^ ", " ^ (Il.Print.string_of_exp e2))

let rec unify_vts' map es1 es2 =
  match es1, es2 with
  | [], _ | _, [] -> map
  | e1::es1, e2::es2 -> unify_vts' (unify_vt map e1 e2) es1 es2
let unify_vts es1 es2 = unify_vts' [] es1 es2
let print_unify_result =
  List.iter (fun (x, e) ->
    print_endline (x ^ ": " ^ (Il.Print.string_of_exp e))
  )

let apply_unify_result result e =
  List.fold_left (fun e (x, e_x) ->
    transform_expr (replace_id_with x e_x) e
  ) e result

let apply_unify_result_prem result p =
  List.fold_left (fun p (x, e_x) ->
    transform_prem (replace_id_with x e_x) p
  ) p result

let fix_free_var ess =
  let destruct_var e =
    match e.it with
    | VarE x -> Some (x.it, e.note)
    | _ -> None
  in
  let free_vars = List.flatten ess |> List.filter_map destruct_var |> dedup (fun x y -> fst x = fst y) in
  List.fold_left (fun ess (x, typ) ->
    let e = gen_typ typ in
    List.map (List.map (transform_expr (replace_id_with x e))) ess
  ) ess free_vars

let fix_rts (cases: string list): restype list =
  List.fold_left (fun rts case ->
    let i = List.length rts - 1 in

    let (rt1, rt2) = !arrow_map |> List.assoc case in

    let length_cache = ref [] in (* TODO: Remove this and use sidecond *)
    let get_cached_length e =
      match List.find_opt (fun (k, _v) -> Il.Eq.eq_exp k e) !length_cache with
      | None ->
        let l = Random.int 3 in (* 0, 1, 2 *)
        length_cache := (e, l) :: !length_cache;

        let sidecond = TypeLenC (i, e, l) in
        sideconds := sidecond :: !sideconds;

        l
      | Some (_, l) -> l
    in

    let rec mk_vts rt =
      match rt.it with
      | ListE es -> es
      | CatE (e1, e2) -> mk_vts e1 @ mk_vts e2
      | IterE (e, (List, xes)) ->
        let length = get_cached_length e in
        List.init length (fun i ->
          List.fold_left (fun e (x, _) ->
            transform_expr (replace_id x.it (x.it ^ "." ^ string_of_int i)) e
          ) e xes
        )
      | _ -> [rt]
    in

    let remove_sub e = match e.it with | SubE (e, _, _) -> e | _ -> e in

    let append_idx = transform_expr (replace_id_using (fun x -> x ^ "@" ^ (string_of_int i))) in

    let vts1 = mk_vts rt1 |> List.map remove_sub |> List.map append_idx in
    let vts2 = mk_vts rt2 |> List.map remove_sub |> List.map append_idx in

    let rt = List.hd rts in
    let rts = List.tl rts in

    let extra_length = List.length vts1 - List.length rt in

    let rt, rts = if extra_length > 0 then
      let vals = Lib.List.take extra_length vts1 in
      let (@@) xs ys = List.rev (xs @ (List.rev ys)) in
      vals @@ rt, List.map ((@@) vals) rts
    else
      rt, rts
    in

    let prefix = if extra_length < 0 then
      Lib.List.take (-extra_length) (List.rev rt)
    else
      []
    in

    let unify_result = unify_vts rt (List.rev vts1) in

    (List.rev (prefix @ vts2) :: rt :: rts) |> List.map (List.map (apply_unify_result unify_result))
  ) [[]] cases
  |> List.rev
  |> fix_free_var

let concretize_instr trule instr =
  let free_vars = ref [] in
  transform_expr (fun e ->
    match e.it with
    | VarE _ -> free_vars := e :: !free_vars; e
    | _ -> e
  ) instr |> ignore;
  dedup Il.Eq.eq_exp !free_vars
  |> List.fold_left (fun (trule, instr) e ->
    let e' = gen_typ e.note in
    let trule' = {trule with it =
      match trule.it with
      | RuleD (id, binds, mixop, exp, prems) ->
        let exp' = exp |> transform_expr (replace e e') in
        let prems' = prems |> List.map (transform_prem (replace e e')) in
        RuleD (id, binds, mixop, exp', prems')
    } in
    let instr' = transform_expr (replace e e') instr in
    trule', instr'
  ) (trule, instr)

let fix_values vt: string list * restype list =
  match vt.it with
  (* HARDCODE: Default instr for each type *)
  | CaseE ([[{it = El.Atom.Atom nt; _}]], {it = TupE []; _}) ->
    (match nt with
    | "I32" | "I64" | "F32" | "F64" -> ["CONST"], [[vt]]
    | "V128" -> ["VCONST"], [[vt]]
    | _ -> failwith "Unknown type"
    )
  | CaseE ([[{it = El.Atom.Atom "REF"; _}];[];[]], {it = TupE [
      {it = CaseE ([[{it = El.Atom.Atom "NULL"; _}];[{it = El.Atom.Quest; _}]], {it = TupE [{it = OptE nul; _}]; _}); _};
      ht
    ]; _}) ->
    assert (!Flag.version = 3);
    (match nul with
    | Some _ -> ["REF.NULL"], [[vt]]
    | None ->
      (match ht.it with
      (* TODO: Add more cases? *)
      | CaseE ([[{it = El.Atom.Atom "I31"; _}]], {it = TupE []; _}) ->
        ["CONST"; "REF.I31"], [[il_case "I32" "valtype" []]; [vt]]
      | _ ->
        let vt' = vt |> replace_caseE_arg [0; 0] (OptE (Some (TupE [] $$ no_region % (TupT [] $ no_region)))) in
        ["REF.NULL"; "REF.AS_NON_NULL"], [[vt']; [vt]]
      )
    )
  | _ ->
    ["LOCAl.GET"], [[vt]]
let accumulate_rtss rtss =
  List.fold_left (fun stack rts ->
    let last_rt = List.hd (List.rev stack) in
    stack @ List.map (fun rt -> rt @ last_rt) rts
  ) [[]] rtss
let values_cnt = ref 0

let fix_immediate (cases: string list) rts: exp list =
  let rt = List.hd rts in
  let rts = List.tl rts in

  List.fold_left2 (fun (acc, rt1) case rt2 ->
    let i = List.length acc in

    let (rt1', rt2') = List.assoc case !arrow_map in
    (*
    print_endline (Il.Print.string_of_rule trule);
    print_endline (Il.Print.string_of_exp rt1');
    print_endline (Il.Print.string_of_exp rt2');
    *)
    let get_cached_length e =
      List.find_map (function
      | TypeLenC (i', e', l) when i = (i' + !values_cnt) && Il.Eq.eq_exp e e' -> Some l
      | _ -> None) !sideconds
    in
    let rec mk_vts rt =
      match rt.it with
      | ListE es -> es
      | CatE (e1, e2) -> mk_vts e1 @ mk_vts e2
      | IterE (e, (List, xes)) ->
        let length = get_cached_length e |> Option.get in
        List.init length (fun i ->
          List.fold_left (fun e (x, _) ->
            transform_expr (replace_id x.it (x.it ^ "." ^ string_of_int i)) e
          ) e xes
        )
      | _ -> [rt]
    in

    let remove_sub e = match e.it with | SubE (e, _, _) -> e | _ -> e in

    let vts1 = rt1' |> mk_vts |> List.map remove_sub in
    let vts2 = rt2' |> mk_vts |> List.map remove_sub in

    assert (List.length rt1 >= List.length vts1);
    assert (List.length rt2 >= List.length vts2);

    let unify_result = unify_vts rt1 (List.rev vts1) in
    let unify_result = unify_vts' unify_result rt2 (List.rev vts2) in

    let iter_to_list' e =
      match e.it with
      | IterE (e', (List, xes)) ->
        (match get_cached_length e' with
        | None -> e'
        | Some l ->
          let es = List.init l (fun i ->
            List.fold_left (fun e (x, _) ->
              transform_expr (replace_id x.it (x.it ^ "." ^ string_of_int i)) e
            ) e' xes) in
          let it = ListE es in
          { e with it })
      | _ -> e
    in
    let iter_to_list = transform_expr iter_to_list' in
    let iter_to_list_prem = transform_prem iter_to_list' in

    (* Transform trule *)
    let trule = List.find (fun r ->
      let RuleD (id, _, _, _, _) = r.it in
      String.uppercase_ascii id.it = case
    ) !trules in

    let trule = {trule with it =
      match trule.it with
      | RuleD (id, binds, mixop, exp, prems) ->
        let exp' = exp |> iter_to_list |> apply_unify_result unify_result in
        let prems' = prems |> List.map iter_to_list_prem |> List.map (apply_unify_result_prem unify_result) in
        RuleD (id, binds, mixop, exp', prems')
    } in

    let instr = rule_to_instr trule in
    let trule, instr = concretize_instr trule instr in

    (* print_endline (Il.Print.string_of_rule trule); *)
    (* print_endline (Il.Print.string_of_exp instr); *)

    sideconds := (rule_to_prems trule |> List.concat_map as_sidecond) @ !sideconds;

    instr :: acc, rt2
  ) ([], rt) cases rts |> fst |> List.rev

(* Helper for extracting sidecond *)
let extract_context_sidecond field f_elem sidecond =
  match sidecond with
  | IfPrC {it = CmpE (
      EqOp,
      {it = IdxE ({it = DotE (_C, {it = Atom field'; _}); _}, index); _},
      elem
    ); _}
  | IfPrC {it = CmpE (
      EqOp,
      elem,
      {it = IdxE ({it = DotE (_C, {it = Atom field'; _}); _}, index); _}
    ); _}
  ->
    if field' <> field then
      None
    else
      (match f_elem elem with
      | None -> None
      | Some x -> Some (exp_to_int index, x))
  | _ -> None

let rec gen_default_instrs rt1 rt2 =
  match rt1, rt2 with
  | hd1 :: tl1, hd2 :: tl2 when Il.Eq.eq_exp hd1 hd2 -> gen_default_instrs tl1 tl2
  | _ ->
    List.map (fun _ -> il_case "DROP" "instr" []) rt1
    @ List.map (fun vt -> il_case "CONST" "isntr" [vt]) rt2

let register_typ rt1 rt2 =
  let extract_type_sidecond sidecond =
    match sidecond with
    | RulePrC ({it = "Expand"; _}, [[]; _; []], {it = TupE [
        { it = IdxE ({ it = DotE (_C, {it = Atom "TYPES"; _}); _ }, idx); _ };
        func
      ]; _}) ->
      (try
        assert (case_of_case func = Atom "FUNC");
        let arrow = nth_arg_of_case 0 func in
        let unwrap_listE e = match e.it with | ListE es -> es | _ -> failwith "Not a list" in
        let rt1 = nth_arg_of_case 0 arrow |> nth_arg_of_case 0 |> unwrap_listE in
        let rt2 = nth_arg_of_case 1 arrow |> nth_arg_of_case 0 |> unwrap_listE in
        Some (exp_to_int idx, (rt1, rt2))
      with | _ -> None)
    | TypeCondC (idx, (rt1, rt2)) -> Some (idx, (rt1, rt2))
    | _ -> None
  in

  let type_conds = List.filter_map extract_type_sidecond !sideconds in

  let existing_types =
    let eq_exps l1 l2 = List.length l1 = List.length l2 && List.for_all2 Il.Eq.eq_exp l1 l2 in
    List.filter (fun (_, (rt1', rt2')) ->
      eq_exps rt1 rt1' && eq_exps rt2 rt2'
    ) type_conds
  in
  let tid =
    match existing_types with
    | [] ->
      let sorted = List.sort compare (List.map fst type_conds) in
      let rec aux expected = function
        | [] -> expected
        | x :: xs ->
            if x = expected then aux (expected + 1) xs
            else if x > expected then expected
            else aux expected xs
      in
      let idx = aux 0 sorted in
      sideconds := TypeCondC (idx, (rt1, rt2)) :: !sideconds;
      idx
    | _ -> Utils.choose existing_types |> fst
  in
  NatE (Z.of_int tid) |> to_phrase (mk_VarT "typeidx")

let wrap_as_func (instrs: exp list) (rt: restype) =
  (* 1. If sidecondition contains something about local, generate locals *)
  let extract_local_sidecond = extract_context_sidecond "LOCAL" (fun e ->
    match e.it with
    | CaseE ([[]; []; []], {it = TupE [init; t]; _}) -> (* Wasm 3 *)
      let is_set e =
        match e.it with
        | CaseE ([[{it = Atom "SET"; _}]], _) -> true
        | _ -> false
      in
      Some (is_set init, t)
    | _ -> None)
  in
  let local_conds = List.filter_map extract_local_sidecond !sideconds in
  let lub_total = 1 + List.fold_left max (-1) (List.split local_conds |> fst) in
  (* TODO: This may not generate the case where, i-th local is initially unset, then set by LOCAL.SET, then read by LOCAL.GET *)
  let lub_param = 1 + List.fold_left (fun m (i, (require_set, _)) -> if require_set then max m i else m) (-1) local_conds in
  let param_num = lub_param + Random.int 3 in
  let local_num = max (lub_total - param_num) 0 + Random.int 3 in

  (* 2. If sidecondition contains something about label, generate labels *)
  let extract_label_sidecond = extract_context_sidecond "LABELS" (fun e ->
    match e.it with
    | CaseE ([[]; []], {it = TupE [{it = ListE rt; _}]; _}) -> Some rt
    | _ -> None)
  in
  let label_conds = List.filter_map extract_label_sidecond !sideconds in
  let block_cnt = 1 + List.fold_left max (-1) (List.split label_conds |> fst) in

  let rec wrap_as_block i acc (rt:restype) =
    if i = block_cnt then acc, rt else
    match List.assoc_opt i label_conds with
    | None ->
      let blocktype = [register_typ [] rt] |> il_case "_IDX" "blocktype" in
      wrap_as_block (i+1)
      [il_case "BLOCK" "instr" [blocktype; ListE acc |> to_phrase (IterT (mk_VarT "instr", List) $ no_region)]]
      rt
    | Some rt'->
      let blocktype = [register_typ [] rt'] |> il_case "_IDX" "blocktype" in
      let suffix = gen_default_instrs rt rt' in
      wrap_as_block (i+1)
      [il_case "BLOCK" "instr" [blocktype; ListE (acc @ suffix) |> to_phrase (IterT (mk_VarT "instr", List) $ no_region)]]
      rt'
  in

  let instrs, rt = wrap_as_block 0 instrs rt in

  let typeidx = register_typ
    (List.init param_num (fun i ->
      match List.assoc_opt i local_conds with
      | None -> gen_typ (mk_VarT "valtype")
      | Some (_, t) -> t
    ))
    rt
  in
  let locals = ListE (List.init local_num (fun i ->
    let i = i + param_num in
    let t =
      match List.assoc_opt i local_conds with
      | None -> gen_typ (mk_VarT "valtype")
      | Some (_, t) -> t
    in
    il_case "LOCAL" "local" [t]
  )) |> to_phrase (IterT (mk_VarT "local", List) $ no_region) in
  let expr = ListE instrs |> to_phrase (mk_VarT "expr") in

  il_case "FUNC" "func" [typeidx; locals; expr]

let wrap_as_module (func: exp) =
  ignore func;

  il_case "MODULE" "module" [func]


(* Generates the simplest module, which contains the instruction sequence with whose names are `cases` *)
let gen_test_containing_seq (cases: string list): exp =
  (* 0. Init *)
  print_endline "Seed: ";
  print_int !Flag.seed;
  print_endline "";
  Random.init !Flag.seed;
  trules := get_typing_rules ();
  arrow_map := !trules |> List.map rule_to_arrow;
  sideconds := [];

  (* 1. Fix rt *)
  let rts = fix_rts cases in (* May throw, if this combination is impossible *)
  (* Print *)
  print_endline "1===========";
  rts |> List.iter (fun rt ->
    rt |> List.iter (fun vt -> Il.Print.string_of_exp vt |> print_endline);
    print_endline "";
  );

  (* 2. Prepend values *)
  let (casess, rtss) = List.map fix_values (List.hd rts |> List.rev) |> List.split in
  let cases' = List.flatten casess in
  let cases = cases' @ cases in
  let rts = (accumulate_rtss rtss) @ List.tl rts in
  values_cnt := List.length cases';

  print_endline "2===========";
  cases |> List.iter print_endline;
  let print_rt rt = List.iter (fun vt -> print_endline (Il.Print.string_of_exp vt)) rt; print_endline "" in
  rts |> List.iter print_rt;

  (* 3. Fix immediates *)
  let instrs = fix_immediate cases rts in (* May throw, if it is impossible to fill in immeidates *)
  print_endline "3===========";
  instrs |> List.iter (fun i ->
    print_endline (Il.Print.string_of_exp i);
  );
  print_endline "===========";
  !sideconds |> List.iter (function
    | TypeLenC _ -> ()
    | IfPrC e -> print_endline ("-- " ^ Il.Print.string_of_exp e)
    | RulePrC (id, mixop, exp) -> print_endline ("-- " ^ Il.Print.(id.it ^ ": " ^ string_of_mixop mixop ^ string_of_exp exp))
    | TypeCondC (idx, (rt1, rt2)) -> print_endline (
      Printf.sprintf "-- C.TYPES[%d] ~~ %s -> %s"
      idx
      (List.map Il.Print.string_of_exp rt1 |> String.concat " ")
      (List.map Il.Print.string_of_exp rt2 |> String.concat " ")
    )
  );

  (* 4. Wrap as a function *)
  let func = wrap_as_func instrs (List.rev (List.hd (List.rev rts))) in (* TODO: It's too confusing to decide when to rev or not *)
  print_endline "4===========";
  print_endline (Il.Print.string_of_exp func);
  !sideconds |> List.iter (function
    | TypeCondC (idx, (rt1, rt2)) -> print_endline (
      Printf.sprintf "-- C.TYPES[%d] ~~ %s -> %s"
      idx
      (List.map Il.Print.string_of_exp rt1 |> String.concat " ")
      (List.map Il.Print.string_of_exp rt2 |> String.concat " ")
    )
    | _ -> ()
  );

  (* 5. Wrap as a module *)
  wrap_as_module func

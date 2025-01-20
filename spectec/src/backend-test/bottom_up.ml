open Utils

open Util
open Source

open Langs
(* open Valid *)

(* open Al.Ast *)
(* open Al.Al_util *)
open Il.Ast
open Il.Print

open Il2al.Il_walk

let (let*) = Option.bind

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

let transform_exp f = transform_exp {base_transformer with transform_exp = f}
let transform_typ f = transform_typ {base_transformer with transform_exp = f}
let transform_rule f = transform_rule {base_transformer with transform_exp = f}
let transform_prem f = transform_prem {base_transformer with transform_exp = f}

let rec dedup eq = function
| [] -> []
| hd :: tl -> hd :: dedup eq (Lib.List.filter_not (eq hd) tl)

let rec count_freq eq = function
| [] -> []
| hd :: tl ->
  let sames, diffs = List.partition (eq hd) tl in
  (hd, 1 + List.length sames) :: count_freq eq diffs

let to_phrase ty x = x $$ no_region % ty

(* Il.Eval Wrapper *)
let reduce_exp e = Il.Eval.reduce_exp !il_env e
let reduce_prem pr =
  match pr.it with
  | IfPr e -> {pr with it = IfPr (reduce_exp e)}
  | RulePr (id, mixop, e) -> {pr with it = RulePr (id, mixop, reduce_exp e)}
  | _ -> pr
let match_exp e1 e2 = Il.Eval.match_exp !il_env Il.Subst.empty e1 e2

(* Smart Constructors *)
let mk_VarT x = VarT (x $ no_region, []) $ no_region
let il_var x t = VarE (x $ no_region) |> to_phrase t
let il_case name tname args =
  CaseE (
    (if name = "" then [] else [Xl.Atom.Atom name |> to_phrase (Xl.Atom.info name)]) :: (List.map (fun _ -> []) args),
    TupE args |> to_phrase (TupT (List.map (fun a -> (a, a.note)) args) $ no_region)
  ) |> to_phrase (mk_VarT tname)
let il_list es t =
  ListE es |> to_phrase (IterT (t, List) $ no_region)
let il_opt e_opt t =
  OptE e_opt |> to_phrase (IterT (t, Opt) $ no_region)
let il_tup es =
  TupE es |> to_phrase (TupT (List.map (fun e -> e, e.note) es) $ no_region)
let some_opt = OptE (Some (il_tup [])) |> to_phrase (IterT (TupT [] $ no_region, Opt) $ no_region)
let none_opt = OptE None |> to_phrase (IterT (TupT [] $ no_region, Opt) $ no_region)
let il_some x t =
  let e = il_case x t [] in
  OptE (Some e) |> to_phrase (IterT (TupT [e, e.note] $ no_region, Opt) $ no_region)
let il_none x t =
  let e = il_case x t [] in
  OptE None |> to_phrase (IterT (TupT [e, e.note] $ no_region, Opt) $ no_region)
let il_zero = NumE (Xl.Num.zero `NatT) |> to_phrase (mk_VarT "u32")

let remove_sub e =
  match e.it with
  | SubE (e, _, _) -> e
  | _ -> e

let mixop_of_case e =
  match (remove_sub e).it with
  | CaseE (mixop, _) -> mixop
  | _ -> failwith (string_of_exp e ^ " is not a CaseE")

let case_of_case e =
  match mixop_of_case e with
  | (atom :: _) :: _ -> atom.it
  | _ -> failwith (string_of_exp e ^ " is not a CaseE with at least one atom")

let args_of_case e =
  match (remove_sub e).it with
  | CaseE (_, {it = TupE args; _}) -> args
  | _ -> failwith (string_of_exp e ^ " is not a CaseE")

let nth_arg_of_case n e =
  match (remove_sub e).it with
  | CaseE (_, {it = TupE args; _}) -> List.nth args n
  | _ -> failwith (string_of_exp e ^ " is not a CaseE")

let rec replace_caseE_arg is re e =
  match is, e.it with
  | _, SubE (e', t1, t2) -> { e with it = SubE (replace_caseE_arg is re e', t1, t2) }
  | [], _ ->
    re
  | i :: is, CaseE (mixop, ({ it = TupE es; _ } as tup)) ->
    let es' = List.mapi (fun i' e' -> if i = i' then replace_caseE_arg is re e' else e') es in
    { e with it = CaseE (mixop, { tup with it = TupE es' })}
  | 0 :: is, CaseE (mixop, e') ->
    { e with it = CaseE (mixop, replace_caseE_arg is re e') }
  | _ -> failwith "Expected a CaseE"

let exp_to_int e =
  match (reduce_exp e).it with
  | NumE (`Nat z)
  | NumE (`Int z) -> Z.to_int z
  | _ -> failwith (string_of_exp e ^ " is not an integer")
let exp_of_int i =
  NumE (`Nat (Z.of_int i)) |> to_phrase (NumT `NatT $ no_region)

let exp_to_list e =
  match e.it with
  | ListE es -> es
  | _ -> failwith (string_of_exp e ^ " is not a list")

let contains_false =
  List.exists (fun p ->
    match p.it with
    | IfPr e ->
      let e' = reduce_exp e in
      e'.it = BoolE false
    | _ -> false
  )


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
    | TypA _ -> failwith "TODO"
    | DefA _ -> failwith "TODO"
    | GramA _ -> failwith "TODO"
  let typ_of_bind bind =
    match bind.it with
    | ExpB (_, t) -> t
    | TypB _
    | DefB _
    | GramB _ -> failwith "typ_of_bind"
  let extract_tup_typ typ =
    match typ.it with
    | TupT ets -> List.split ets |> snd
    | _ -> failwith "extract_tup_typ"
  let a2e a =
    match a.it with
    | ExpA e -> e
    | _ -> failwith "Expected an ExpA"
  let e2a e =
    ExpA e $ e.at

  exception DispatchFail of string

  let rec has_deftyp a dt =
    match a.it, dt.it with
    | CaseE (mixop, {it = TupE args; _}), VariantT typcases ->
      List.exists (fun (mixop', (_, typ, _), _) ->
        Xl.Mixop.eq mixop mixop'
        &&
        List.for_all2 has_type args (extract_tup_typ typ)
      ) typcases
    | _, AliasT t -> has_type a t
    | _, VariantT [ [[]; []], ([ bind ], _, _), _ ] -> has_type a (typ_of_bind bind)
    | _ -> false
  and has_type a t =
    Il.Eval.sub_typ !il_env a.note t ||
    match a.it, t.it with
    | NumE _, (NumT `NatT) -> true
    | _, VarT (name, []) -> has_deftyp a (dispatch_deftyp name.it [] |> snd)
    | _ -> false
  and has_argtype a p =
    match a.it, (a2e p).it with
    | CaseE (mixop1, {it = TupE args1; _}), CaseE (mixop2, {it = TupE args2; _}) ->
      Xl.Mixop.eq mixop1 mixop2
      && List.for_all2 has_argtype args1 (List.map e2a args2)
      && has_type a (type_of_arg p)
    | _ ->
      has_type a (type_of_arg p)

  and match_params args inst =
    match inst.it with
    | InstD (_binds, params, deftyp) when (
        List.for_all2 has_argtype args params
      ) -> Some (params, deftyp)
    | _ -> None
  and dispatch_deftyp name args =
    match List.find_map (has_name name) !il with
    | Some insts ->
      ( match List.find_map (match_params args) insts with
        | Some matched -> matched
        | None -> raise (DispatchFail (
          name ^ "(" ^ (args |> List.map (fun e -> string_of_exp e) |> String.concat ", ") ^ ")"
        ))
      )
    | None -> failwith (Printf.sprintf "The syntax named %s does not exist in the input spec" name)
(** End of Helpers to handle type-family-based generation **)

exception UnifyFail of exp * exp
let unify_fail _map e1 e2 =
  raise @@ UnifyFail (e1, e2)
(* TODO: Eventually, replace this with Il.Eval.subst *)
let rec unify_exp ?(on_fail=unify_fail) map e1 e2 =
  let rec resolve e =
    match e.it with
    | VarE x -> (match List.assoc_opt x.it map with Some e -> resolve e | None -> e)
    | _ -> e
  in
  let e1 = resolve e1 in
  let e2 = resolve e2 in
  if Il.Eq.eq_exp e1 e2 then map else
  let f = unify_exp ~on_fail map in
  match e1.it, e2.it with (*TODO: Generalize to more cases *)
  | _, VarE x -> ((x.it, e1) :: map)
  | VarE x, _ -> ((x.it, e2) :: map)
  | SubE (e1, t1, _), SubE (e2, t2, _) when Il.Eval.sub_typ !il_env t1 t2 || Il.Eval.sub_typ !il_env t2 t1 ->
    f e1 e2
  | _, SubE (e, t, _) ->
    if has_type e1 t then
      unify_exp ~on_fail map e1 e
    else
      on_fail map e1 e2
  | SubE (e, t, _), _ ->
    if has_type e2 t then
      unify_exp ~on_fail map e e2
    else
      on_fail map e1 e2
  | CaseE (case1, args1), CaseE (case2, args2) when Xl.Mixop.eq case1 case2 ->
    f args1 args2
  | CallE (id1, args1), CallE (id2, args2) when Il.Eq.eq_id id1 id2 ->
    List.fold_left2 (fun map a1 a2 ->
      match a1.it, a2.it with
      | ExpA e1, ExpA e2 -> f e1 e2
      | _ -> map
    ) map args1 args2
  | TupE es1, TupE es2 when List.length es1 = List.length es2 ->
    List.fold_left2 (unify_exp ~on_fail) map es1 es2
  | IterE (e1, (iter1, xes1)), IterE (e2, (iter2, xes2))
    when iter1 = iter2
      && List.length xes1 = List.length xes2
      && List.for_all2 (fun (x1, e1) (x2, e2) -> Il.Eq.eq_id x1 x2 && Il.Eq.eq_exp e1 e2) xes1 xes2 ->
    f e1 e2
  | ListE es1, ListE es2 when List.length es1 = List.length es2 ->
    List.fold_left2 (unify_exp ~on_fail) map es1 es2
  | OptE None, OptE None -> map
  | OptE (Some e1), OptE (Some e2) -> f e1 e2
  | _, _ -> on_fail map e1 e2

type sidecond =
  | IterLenC of int * exp * int
  | RulePrC of id * mixop * exp
  | IfPrC of exp
  | TypeCondC of int * exp
  | ContextLenC of string * int
let sideconds: sidecond list ref = ref []

let as_sidecond pr =
  let simplify e = reduce_exp (transform_exp remove_sub e) in
  match pr.it with
  | IfPr e -> [IfPrC (simplify e)]
  | RulePr (id, mixop, e) -> [RulePrC (id, mixop, (simplify e))]
  | _ -> []

let string_of_sidecond = function
  | IterLenC (i, e, l) -> Printf.sprintf "-- |%s*| = %d @ %d" (string_of_exp e) l i
  | RulePrC (x, mixop, e) -> Printf.sprintf "-- %s: %s %s" x.it (string_of_mixop mixop) (string_of_exp e)
  | IfPrC e -> "-- " ^ string_of_exp e
  | TypeCondC (i, e) -> Printf.sprintf "-- C.TYPES[%d] = %s" i (string_of_exp e)
  | ContextLenC (x, i) -> Printf.sprintf "-- |C.%s[%d]| >= i" x i

(* Helper for extracting sidecond *)
let extract_context_sidecond field f_elem sidecond =
  match sidecond with
  | IfPrC {it = CmpE (
      `EqOp, `BoolT,
      {it = IdxE ({it = DotE (_C, {it = Atom field'; _}); _}, index); _},
      elem
    ); _}
  | IfPrC {it = CmpE (
      `EqOp, `BoolT,
      elem,
      {it = IdxE ({it = DotE (_C, {it = Atom field'; _}); _}, index); _}
    ); _}
  ->
    if field' <> field then
      None
    else
      let* x = f_elem elem in
      Some (exp_to_int index, x)
  | IfPrC {it = CmpE (
      `EqOp, `BoolT,
      {it = ProjE ({it = UncaseE (
        {it = IdxE ({it = DotE (_C, {it = Atom field'; _}); _}, index); _},
      _); _}, _); _},
      elem
    ); _}
  | IfPrC {it = CmpE (
      `EqOp, `BoolT,
      elem,
      {it = ProjE ({it = UncaseE (
        {it = IdxE ({it = DotE (_C, {it = Atom field'; _}); _}, index); _},
      _); _}, _); _}
    ); _}
  ->
    if field' <> field then
      None
    else
      let* x = f_elem elem in
      Some (exp_to_int index, x)
  | RulePrC (id, [[]; [{it = Turnstile; _}]; [{it = Sub; _}]; []], {it = TupE [
      _C;
      {it = IdxE ({it = DotE (_C', {it = Atom field'; _}); _}, index); _};
      elem
    ]; _}) when field' = field && String.ends_with ~suffix:"_sub" id.it ->
      let* x = f_elem elem in
      Some (exp_to_int index, x)
  | RulePrC (id, [[]; [{it = Turnstile; _}]; [{it = Sub; _}]; []], {it = TupE [
      _C;
      elem;
      {it = ProjE ({it = UncaseE ({it = IdxE ({it = DotE (_C', {it = Atom field'; _}); _}, index); _}, [[]; []]); _}, 0); _}
    ]; _}) when field' = field && String.ends_with ~suffix:"_sub" id.it ->
      let* x = f_elem elem in
      Some (exp_to_int index, x)
  | _ -> None

let extract_context_len_sidecond field sidecond =
  match sidecond with
  | IfPrC {it = CmpE (
      `LtOp, `NatT,
      len,
      {it = LenE ({it = DotE (_C, {it = Atom field'; _}); _}); _}
    ); _}
  | IfPrC {it = CmpE (
      `GtOp, `NatT,
      {it = LenE ({it = DotE (_C, {it = Atom field'; _}); _}); _},
      len
    ); _}
  when field = field'
  ->
    Some (exp_to_int len + 1)
  | IfPrC {it = CmpE (
      `LeOp, `NatT,
      len,
      {it = LenE ({it = DotE (_C, {it = Atom field'; _}); _}); _}
    ); _}
  | IfPrC {it = CmpE (
      `GeOp, `NatT,
      {it = LenE ({it = DotE (_C, {it = Atom field'; _}); _}); _},
      len
    ); _}
  when field = field'
  ->
    Some (exp_to_int len)
  | ContextLenC (field', len) when field = field' -> Some len
  | _ -> None

let extract_type_sidecond sidecond =
  (* TODO: Handle (or not) Wasm 2.0 *)
  match sidecond with
  | RulePrC ({it = "Expand"; _}, [[]; _; []], {it = TupE [
      { it = IdxE ({ it = DotE (_C, {it = Atom "TYPES"; _}); _ }, idx); _ };
      typ
    ]; _}) -> Some (exp_to_int idx, typ)
  | TypeCondC (idx, typ) -> Some (idx, typ)
  | _ -> None

let alloc min idxs =
  let rec aux expected = function
    | [] -> expected
    | x :: xs ->
        if x = expected then aux (expected + 1) xs
        else if x > expected then expected
        else aux expected xs
  in
  aux min (List.sort compare idxs)

let register_typ ?(dry=false) t =
  let type_conds = List.filter_map extract_type_sidecond !sideconds in

  let tids = ref [] in
  let extract_tid e =
    match e with
    | {it = CaseE ([[{it = Atom "_IDX"; _}]; []], _); note = {it = VarT ({it = ("typeuse" | "heaptype"); _}, []); _}; _}  ->
      let tid = e |> nth_arg_of_case 0 |> nth_arg_of_case 0 |> exp_to_int in
      push tid tids;
      e
    | _ -> e
  in
  transform_exp extract_tid t |> ignore;
  let tid_max = List.fold_left max 0 !tids in

  let existing_types = List.filter (fun (_, typ) -> Il.Eq.eq_exp t typ) type_conds in
  let tid =
    match existing_types with
    | [] ->
      let idx = alloc tid_max (List.map fst type_conds) in
      if not dry then push (TypeCondC (idx, t)) sideconds;
      idx
    | _ -> choose existing_types |> fst
  in
  il_case "" "typeidx" [exp_of_int tid]

let arrow_to_func rt1 rt2 =
  let f_rt rt = il_case "" "resulttype" [il_list rt (mk_VarT "valtype")] in
  let func = CaseE (
    [[]; [Xl.Atom.Arrow |> to_phrase (Xl.Atom.info "->")]; []],
    il_tup [f_rt rt1; f_rt rt2]
  ) |> to_phrase (mk_VarT "functype") in
  match !Flag.version with
  | 3 -> il_case "FUNC" "comptype" [func]
  | _ -> func

let register_func_typ rt1 rt2 = register_typ (arrow_to_func rt1 rt2)

(* Helpers for Top Down Gen *)

type context = {
  typ: typ;
  args: arg list;
  prems: prem list;
  refer: exp option;
}

let has_same_mixop e_opt tc =
  match e_opt, tc with
  | None, _ -> true
  | Some e, (mixop, _, _) ->
    match e.it with
    | CaseE (mixop', _) -> Xl.Mixop.eq mixop mixop'
    | _ -> true

(* HARDCODE: force valid expressions + append sidecondtions by this expression *)
let validate x e =
  match x, e.it with
  | "limits", _ ->
    let l = nth_arg_of_case 0 e in
    let r = nth_arg_of_case 1 e in
    if nth_arg_of_case 0 l <= nth_arg_of_case 0 r then e else
    e |> replace_caseE_arg [0] r |> replace_caseE_arg [1] l
  | ("typeuse" | "heaptype"), CaseE ([[{it = Atom "_IDX"; _}]; []], _) ->
    let tid = e |> nth_arg_of_case 0 |> nth_arg_of_case 0 |> exp_to_int in
    let sidecond = ContextLenC ("TYPES", tid + 1) in
    push sidecond sideconds;
    e
  | _ -> e

exception SyntaxError

let try_gen_from_prems c e =
  let prems = c.prems in

  let rec extract_eq_cond e e' =
    match e'.it with
    | BinE (`OrOp, `BoolT, e1, e2) ->
        (match extract_eq_cond e e1, extract_eq_cond e e2 with
        | Some es1, Some es2 -> Some (union ~eq:Il.Eq.eq_exp es1 es2)
        | _ -> None)
    | BinE (`AndOp, `BoolT, e1, e2) ->
        (match extract_eq_cond e e1, extract_eq_cond e e2 with
        | Some es1, Some es2 -> Some (intersect ~eq:Il.Eq.eq_exp es1 es2)
        | Some es, None -> Some es
        | None, Some es -> Some es
        | _ -> None)
    | CmpE (`EqOp, `BoolT, e1, e2) when Il.Eq.eq_exp e e1 -> Some [e2]
    | CmpE (`EqOp, `BoolT, e2, e1) when Il.Eq.eq_exp e e1 -> Some [e2]
    | _ -> None
  in

  let extract_eq_cond_prem e p =
    match p.it with
    | IfPr e' -> extract_eq_cond e e'
    | _ -> None
  in

  match e.it with
  | VarE id when id.it <> "_" ->
    (match List.find_map (extract_eq_cond_prem e) prems with
    | None -> c.refer
    | Some es ->
      match c.refer with
      | None -> Some (choose es)
      | Some r ->
        match List.find_opt (fun e -> Il.Eq.eq_exp e r) es with
        | None -> Some (choose es)
        | some -> some
    )
  | IterE (e', (List, [x, _]))  ->
    let e_l = LenE e |> to_phrase (NumT `NatT $ no_region) in
    (match List.find_map (extract_eq_cond_prem e_l) prems with
      | Some e_ns ->
        let e_n = choose e_ns in
        (* TODO?: This overwrites c.refer *)
        let n = exp_to_int e_n in
        let es = List.init n (fun i ->
          transform_exp (replace_id x.it (x.it ^ "." ^ string_of_int i)) e'
        ) in
        Some {e with it = ListE es}
      | None -> c.refer
    )
  | _ -> c.refer

let rec gen c x =
  Debug_log.(log "bottom_up.gen" (fun _ -> x) string_of_exp) @@ fun _ ->
  let a2e a =
    match a.it with
    | ExpA e -> reduce_exp e
    | _ -> failwith @@ "Unsupported arg for " ^ x
  in
  let args = List.map a2e c.args in
  let params, deftyp = dispatch_deftyp x args in
  let params = List.map a2e params in
  let map = List.fold_left2 unify_exp [] params args in
  let replace_params = (fun e ->
    List.fold_left (fun e (x, e') ->
      replace_id_with x e' e
    ) e map
  ) in
  (match deftyp.it with
  | AliasT typ -> gen_typ c (transform_typ replace_params typ)
  | StructT typfields ->
    let ref_fields =
      match c.refer with
      | Some ({it = StrE fs; _}) -> List.map (fun (_atom, e) -> Some e) fs
      | _ -> List.init (List.length typfields) (fun _ -> None)
    in
    let gen_typfield (atom, (_binds, typ, _prems), _hints) refer =
      atom, gen_typ {c with refer} (transform_typ replace_params typ)
    in
    StrE (List.map2 gen_typfield typfields ref_fields) |> to_phrase c.typ (* TODO: The order of fields may be different *)
  | VariantT typcases ->
    let typcases = Lib.List.filter_not (has_subid_hint "sem") typcases in
    let typcases = Lib.List.filter_not (has_subid_hint "admin") typcases in
    let typcases = List.filter (has_same_mixop c.refer) typcases in
    let typcase = Utils.choose typcases in
    let typcase' =
      let (m, (bs, t, ps), hs) = typcase in
      let t' = transform_typ replace_params t in
      let ps' = List.map (transform_prem replace_params) ps in
      let ps' = List.map reduce_prem ps' in
      if contains_false ps' then raise SyntaxError;
      m, (bs, t', ps'), hs
    in
    gen_typcase c typcase'
  ) |> validate x (* TODO: This will destroy the entanglement with reference *)
and gen_typcase c (mixop, (_binds, typs, prems), _hints) =
  let refer = Option.bind c.refer (fun e ->
    match e.it with CaseE (_, args) -> Some args
    | _ -> None
  ) in
  let args = il_tup (gen_typs {c with prems; refer} typs) in
  CaseE (mixop, args) |> to_phrase c.typ
and gen_typs c typs =
  match typs.it with
  | TupT typs' ->
    let refs =
      match c.refer with
      | Some {it = TupE es; _} -> List.map Option.some es
      | _ -> List.init (List.length typs') (fun _ -> None)
    in
    List.fold_left_map (fun (replaces, prems) ((e, t), refer) ->
      let e', prems' =
        let c = {c with prems; refer} in
        let refer = try_gen_from_prems {c with refer} e in
        let c = {c with refer} in

        let t' = List.fold_left (fun t (e, e') -> transform_typ (replace e e') t) t replaces in
        try_n 100 "generating syntax" (fun () ->
          let e' = gen_typ c t' in
          let prems' = List.map (transform_prem (replace e e')) prems in
          (e', prems') |> unless (prems' |> contains_false)
        )
      in
      ((e, e') :: replaces, prems'),
      e'
    ) ([], c.prems) (List.combine typs' refs) |> snd
  | _ -> [ gen_typ c typs ]
and gen_typ c typ =
  let refer = c.refer in
  let c = { c with refer = None } in
  match typ.it with
  | VarT (id, [{it = TypA t; _}]) when id.it = "list" -> (* HARDCODE: list *)
    (match refer with
    | Some {it = CaseE ([[]; []], {it = TupE [el]; _}); _} ->
      CaseE ([[]; []], il_tup [gen_typ {c with refer = Some el} (IterT (t, List) $ no_region)])
        |> to_phrase typ
    | _ ->
      CaseE ([[]; []], il_tup [gen_typ c (IterT (t, List) $ no_region)])
        |> to_phrase typ
    )
  | VarT (id, args) -> gen {typ; args; prems = []; refer} id.it
  | NumT `NatT ->
    (match refer with
    | Some ({it = NumE (`Nat _); _} as r) -> r
    | _ -> NumE (`Nat (Random.int 3 |> Z.of_int)) |> to_phrase typ (* 0, 1, 2 *)
    )
  | NumT `IntT ->
    (match refer with
    | Some ({it = NumE (`Int _); _} as r) -> r
    | _ -> NumE (`Int (Random.int 5 - 2 |> Z.of_int)) |> to_phrase typ (* -2 ~ 2 *)
    )
  | IterT (typ', List) ->
    (match refer with
    | Some {it = ListE rs; _} ->
      ListE (List.map (fun r -> gen_typ {c with refer = Some r} typ') rs) |> to_phrase typ
    | _ ->
      let len = Random.int 3 in (* 0, 1, 2 *)
      ListE (List.init len (fun _ -> gen_typ c typ')) |> to_phrase typ
    )
  | IterT (typ', Opt) ->
    (match refer with
    | Some ({it = OptE None; _} as r) -> r
    | Some ({it = OptE (Some r); _}) -> OptE (Some (gen_typ {c with refer = Some r} typ')) |> to_phrase typ
    | _ ->
      if Random.bool() then
        OptE None |> to_phrase typ
      else
        OptE (Some (gen_typ c typ')) |> to_phrase typ
    )
  | TupT ets ->
    (match refer with
    | Some {it = TupE rs; _} ->
      TupE (List.map2 (fun (_, t) r -> gen_typ {c with refer = Some r} t) ets rs) |> to_phrase typ
    | _ ->
      TupE (ets |> List.map (fun (_, t) -> gen_typ c t)) |> to_phrase typ
    )
  | _ -> failwith ("TODO: unhandled type for gen_typ: " ^ string_of_typ typ)
let gen_typ typ = gen_typ {typ; args = []; prems = []; refer = None} typ

(** End of Helpers **)

type valtype = exp
type restype = valtype list

let trules = ref []

let expected_shape e shape =
  Printf.sprintf "Expected %s to be %s" (string_of_exp e) shape |> failwith

let rule_to_arrow rule =
  let rec unwrap e =
    match e.it with
    | CaseE ([[]; []], e')
    | TupE [e'] -> unwrap e'
    | _ -> e
  in

  let RuleD (_, _, _, exp, _) = rule.it in
  match exp.it with
  | TupE [_c; _lhs; rhs] ->
    (match rhs.it with
    | CaseE (_, args) ->
      (match args.it with
      | TupE [t1; t2] | TupE [t1; _; t2] -> unwrap t1, unwrap t2
      | _ -> expected_shape args "t1, t2 or t1, x*, t2"
      )
    | _ -> expected_shape rhs "e1 -> e2"
    )
  | _ -> expected_shape exp "C |- lhs : rhs"

let rule_to_instr rule =
  let RuleD (_, _, _, exp, _) = rule.it in
  match exp.it with
  | TupE [_c; lhs; _rhs] -> lhs
  | _ -> expected_shape exp "C |- lhs : rhs"

let rule_to_prems rule =
  let RuleD (_, _, _, _, prems) = rule.it in
  prems

let find_trules case =
  List.filter (fun r ->
    let RuleD (id, _, _, _, _) = r.it in
    let case' =
      String.uppercase_ascii id.it
      |> String.split_on_char '-'
      |> List.hd
    in
    case = case'
  ) !trules

let case_to_rule case =
  let trules = if case = "" then !trules else find_trules case in
  choose trules

(* TODO: This should eventually consider subtype, automatically *)
let unify_vt map vt1 vt2 =
  let on_fail map e1 e2 =
    match e1.it, e2.it with
    | CaseE ([[atom]], _), CaseE ([[{it = Atom "_IDX"; _}]; []], {it = TupE [{it = VarE _; _}]; _}) ->
      (match atom.it with
      | Atom "ANY"
      | Atom "EQ"
      | Atom "STRUCT"
      | Atom "ARRAY"
      | Atom "FUNC" -> map
      | _ -> unify_fail map e1 e2
      )
    | CaseE ([[{it = Atom ("EQ" | "ANY"); _}]], _), CaseE ([[{it = Atom "I31"; _}]], _) ->
      map
    | _ ->
      unify_fail map e1 e2
  in
  unify_exp ~on_fail map vt1 vt2

let rec unify_vts' map es1 es2 =
  match es1, es2 with
  | [], _ | _, [] -> map
  | e1::es1, e2::es2 -> unify_vts' (unify_vt map e1 e2) es1 es2
let unify_vts es1 es2 =
  unify_vts' [] es1 es2
let print_unify_result =
  List.iter (fun (x, e) ->
    print_endline (x ^ ": " ^ (string_of_exp e))
  )
let result_of_subst s = Il.Subst.Map.bindings s.Il.Subst.varid
let print_subst s =
  print_unify_result @@ result_of_subst s

let apply_unify_result result e =
  List.fold_left (fun e (x, e_x) ->
    transform_exp (replace_id_with x e_x) e
  ) e result
  |> reduce_exp

let apply_unify_result_prem result = transform_prem @@ apply_unify_result result

let apply_unify_result_rule result = transform_rule @@ apply_unify_result result

let fix_free_var_of_rt trules ess =
  try_n 10 "Fixing free vars of rt" (fun () ->
    let free_vars = ref [] in
    List.map (transform_exp (fun e ->
      match e.it with
      | VarE x -> push (x.it, e.note) free_vars; e
      | _ -> e
    )) (List.flatten ess) |> ignore;
    dedup (fun x y -> (fst x) = (fst y)) !free_vars
    |>
    List.fold_left (fun acc (x, typ) ->
      let* (ess, trules) = acc in
      let e = gen_typ typ in
      let trules = List.map (transform_rule (replace_id_with x e)) trules in
      let prems = List.concat_map rule_to_prems trules in
      if contains_false prems then
        None
      else
        Some (List.map (List.map (transform_exp (replace_id_with x e))) ess, trules)
    ) (Some (ess, trules))
  )

let get_cached_length i e =
  match e.note.it with
  | IterT ({it = VarT ({it = "laneidx"; _}, _); _}, _) -> (* HARDCODE: For VSHUFFLE *)
    Some 16
  | _ ->
    List.find_map (function
      | IterLenC (i', e', l) when i = i' && Il.Eq.eq_exp e e' -> Some l
      | _ -> None
    ) !sideconds

exception RejectedSample of string

let fix_lengths i max es =
  let len_opts = List.map (get_cached_length i) es in
  let len_opt = List.fold_left (fun acc cur ->
    match acc, cur with
    | None, None -> None
    | None, Some x -> Some x
    | Some x, None -> Some x
    | Some x, Some y -> if x = y then acc else raise (RejectedSample "Length mismatch of IterE")
  ) None len_opts in

  let l =
    match len_opt with
    | Some l -> l
    | None -> Random.int max (* [0, max) *)
  in

  List.iter2 (fun e o -> if o = None then push (IterLenC (i, e, l)) sideconds) es len_opts;
  l

(* 1. fix_rts: pre-determine concrete types of each cases *)
let fix_rts (cases: string list): restype list * rule list =
  Debug_log.(log "bottom_up.fix_rts"
    (fun () -> String.concat " " cases)
    (fun (_rts, rules) -> rules |> List.map string_of_rule |> String.concat "\n" )
  ) @@ fun _ ->
  let rts, trules = List.fold_left (fun (rts, trules) case ->
    let i = List.length trules in
    (* Helpers *)
    let len_cache = ref [] in
    let init_len_cache () =
      len_cache := []
    in
    let iter_to_list ?(enforce=false) e =
      match e.it with
      | IterE (e', (List, xes)) ->
        let xs, es = List.split xes in
        let length_opt = es
          |> List.map (get_cached_length i)
          |> List.fold_left (fun acc -> Option.fold ~none:acc ~some:Option.some) None
        in
        if not enforce && length_opt = None then e else
        let length =
          match length_opt with
          | None -> fix_lengths i 3 es
          | Some l -> l
        in
        let es = List.init length (fun i ->
          List.fold_left (fun e x ->
            transform_exp (replace_id x.it (x.it ^ "." ^ string_of_int i)) e
          ) e' xs
        ) in
        let it = ListE es in
        { e with it }
      | IterE (e', (ListN ({it = VarE {it = n; _}; _}, None), [])) ->
        let l = match List.assoc_opt n !len_cache with
          | Some l -> l
          | None -> Random.int 3 |>> (fun l -> push (n, l) len_cache)
        in
        let es = List.init l (fun _ -> e') in
        let it = ListE es in
        { e with it }
      | IterE _ ->
        if enforce then
          failwith @@ "Unhandled iter" ^ string_of_exp e ^ " for iter_to_list"
        else
          e
      | _ -> e
    in

    let rec mk_vts rt =
      match rt.it with
      | ListE es -> es
      | CatE (e1, e2) -> mk_vts e1 @ mk_vts e2
      | IterE _ -> mk_vts (iter_to_list rt ~enforce:true)
      | _ -> [rt]
    in

    let append_idx' x = x ^ "@" ^ (string_of_int i) in
    let append_idx = replace_id_using (fun x -> if x = "C" then x else append_idx' x) in
    let append_idx_exp = transform_exp append_idx in
    let append_idx_rule = transform_rule append_idx in
    (* End of Helpers *)

    let handler = if case = "" then (try_n 100 "Fixing the instruction") else (fun f -> f () |> Option.get) in
    handler (fun () -> try(
      let trule = case_to_rule case in
      let (rt1, rt2) = rule_to_arrow trule in

      init_len_cache ();
      let vts1 = mk_vts rt1 |> List.map append_idx_exp in
      let vts2 = mk_vts rt2 |> List.map append_idx_exp in
      let trule = trule |> transform_rule iter_to_list |> append_idx_rule in
      let pre_unify = !len_cache |> List.map (fun (x, l) -> append_idx' x , exp_of_int l) in

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

      let unify_result, trules = try_n 10 "Fixing rts" (fun () ->
        let unify_result = unify_vts rt (List.rev vts1) @ pre_unify in
        let trules = (trule :: trules) |> (List.map @@ apply_unify_result_rule unify_result) in
        let prems = trules |> List.concat_map rule_to_prems in
        if contains_false prems then
          None
        else
          Some (unify_result, trules)
      ) in

      let rts = (List.rev (prefix @ vts2) :: rt :: rts) |> List.map (List.map (apply_unify_result unify_result)) in
      Some (rts, trules)
    ) with | _ -> None)
  ) ([[]], []) cases in

  let rts, trules = List.rev rts, List.rev trules in
  fix_free_var_of_rt trules rts

(* 2. fix_values: generate necessary values in front of main instrs *)
let fix_values vt: rule list =
  let f case vt =
    let trule = find_trules case |> List.hd in
    let _, rt = rule_to_arrow trule in
    let vt' = Util.Lib.List.last (exp_to_list rt) in
    let result = unify_vt [] vt vt' in
    apply_unify_result_rule result trule
  in
  let g case = find_trules case |> List.hd in
  let vt = vt |> remove_sub |> reduce_exp in
  match vt.it with
  (* HARDCODE: Default instr name for each type *)
  | CaseE ([[{it = Atom nt; _}]], {it = TupE []; _}) ->
    (match nt with
    | "I32" | "I64" | "F32" | "F64" ->
      [f "CONST" vt]
    | "V128" ->
      [f "VCONST" vt]
    | _ -> failwith "Unknown type"
    )
  | CaseE ([[{it = Atom "REF"; _}];[];[]], {it = TupE [
      {it = OptE nul; _};
      ht
    ]; _}) ->
    assert (!Flag.version = 3);
    (match nul with
    | Some _ -> (* Nullable *)
      [f "REF.NULL" vt]
    | None -> (* Non-nullable *)
      let ht =
        match ht.it with
        | CaseE ([[{it = Atom ("ANY" | "EQ"); _}]], {it = TupE []; _}) ->
          choose [il_case "I31" "heaptype" []; il_case "STRUCT" "heaptype" []; il_case "ARRAY" "heaptype" []]
        | _ -> ht
      in
      let i32 = il_case "I32" "valtype" [] in
      (match ht.it with
      (* TODO: Add more cases? *)
      | CaseE ([[{it = Atom "I31"; _}]], {it = TupE []; _}) ->
        [f "CONST" i32; g "REF.I31"]
      | CaseE ([[{it = Atom "STRUCT"; _}]], {it = TupE []; _}) ->
        [g "STRUCT.NEW_DEFAULT"]
      | CaseE ([[{it = Atom "ARRAY"; _}]], {it = TupE []; _}) ->
        [f "CONST" i32; g "ARRAY.NEW_DEFAULT"]
      | CaseE ([[{it = Atom "_IDX"; _}]; []], _) ->
        let idx = ht |> nth_arg_of_case 0 |> nth_arg_of_case 0 |> exp_to_int in
        let conds = List.filter_map extract_type_sidecond !sideconds in
        (match List.find_opt (fun (j, _) -> idx = j) conds with
        | None ->
          [f "CONST" i32; f "ARRAY.NEW_DEFAULT" vt]
        | Some (_, t) ->
          (match case_of_case t with
          | Atom "STRUCT" -> [f "STRUCT.NEW_DEFAULT" vt]
          | Atom "ARRAY" -> [f "CONST" i32; f "ARRAY.NEW_DEFAULT" vt]
          | Atom "FUNC" -> [f "REF.FUNC" vt]
          | _ -> failwith "unreachable"
          )
        )
      | _ ->
        let vt' = vt |> replace_caseE_arg [0] (il_some "NULL" "nul")  in
        [f "REF.NULL" vt'; f "REF.AS_NON_NULL" vt]
      )
    )
  | _ -> failwith @@ "fix_value: Unrecognized valtype - " ^ string_of_exp vt
let accumulate_rtss rtss =
  List.fold_left (fun stack rts ->
    let last_rt = List.hd (List.rev stack) in
    stack @ List.map (fun rt -> rt @ last_rt) rts
  ) [[]] rtss
let values_cnt = ref 0

let handle_context_type_prems trule =
  let extract_type_prem p =
    match p.it with
    | RulePr ({it = "Expand"; _}, [[]; _; []], {it = TupE [
        { it = IdxE ({ it = DotE (_C, {it = Atom "TYPES"; _}); _ }, idx); _ };
        typ
      ]; _}) ->
      (match idx.it with
      | ProjE ({it = UncaseE (case, _); _}, _) -> Some (case, typ)
      | _ -> None
      )
    | _ -> None
  in

  let prems = rule_to_prems trule in
  let type_prems = List.filter_map extract_type_prem prems in

  List.fold_left (fun trule (e, t) ->
    let dry = not Il.Free.(Set.is_empty (free_exp t).varid) in
    let idx = register_typ ~dry t in
    let unify_result = unify_exp [] e idx in
    apply_unify_result_rule unify_result trule
  ) trule type_prems

let register_iterlen_cond i result p =
  match p.it with
  | IfPr ({it = CmpE (
      `LtOp, `NatT,
      l,
      ({it = LenE {it = IterE (e, _); _}; _})
    ); _}) ->
    (* TODO: Check if l is already in unify result *)
    let idx = exp_of_int @@ Random.int 3 in
    let subst =
      match match_exp idx l with
      | Some subst -> subst
      | None -> Il.Subst.empty
      | exception Il.Eval.Irred -> Il.Subst.empty
    in

    let l' = l |> Il.Subst.subst_exp subst |> reduce_exp in

    let sidecond = IterLenC (i, e, exp_to_int l' + 1 + Random.int 2) in
    push sidecond sideconds;
    result_of_subst subst @ result
  | _ -> result
let reset_iterlen_cond i =
  sideconds := List.filter (fun sc ->
    match sc with
    | IterLenC (j, _, _) -> i <> j (* TODO: This deletes typelen cond, which is OK for now *)
    | _ -> true
  ) !sideconds

let concretize_instr trule instr =
  (* 1. Generate from syntax *)
  let trule, instr = try_n 100 "concretizing instr" (fun () -> try (
    let instr' = gen {typ = mk_VarT "instr"; args = []; prems = []; refer = Some instr} "instr" in
    let unify_result = unify_exp [] instr instr' in

    let trule = apply_unify_result_rule unify_result trule in

    let prems = rule_to_prems trule in
    if contains_false prems then
      None
    else
      Some (trule, instr')
  ) with | SyntaxError -> None) in

  (* 2. Fill in all free variables with random value *)
  let free_vars = ref [] in
  transform_exp (fun e ->
    match e.it with
    | VarE id when id.it <> "_" -> push e free_vars; e
    | _ -> e
  ) instr |> ignore;

  let replaces = ref [] in

  let trule, instr =
  dedup Il.Eq.eq_exp (List.rev !free_vars)
  |> List.fold_left (fun (trule, instr) e ->
    let t = List.fold_left (fun t (e, e') -> transform_typ (replace e e') t) e.note !replaces in
    let e' = gen_typ t in
    push (e, e') replaces;
    let trule' = {trule with it =
      match trule.it with
      | RuleD (id, binds, mixop, exp, prems) ->
        let exp' = exp |> transform_exp (replace e e') in
        let prems' = prems |> List.map (transform_prem (replace e e')) in
        RuleD (id, binds, mixop, exp', prems')
    } in
    let instr' = transform_exp (replace e e') instr in
    trule', instr'
  ) (trule, instr)
  in

  trule, instr

let rec simplify_equality prems =
  (* If there is equality prems within these prems, where one side is a variable, simplify the whole prems *)
  (* Assumption: No cyclic binding *)
  let is_eq_prem prem =
    match prem.it with
    | IfPr ({it = CmpE (`EqOp, `BoolT, {it = VarE x; _}, ({it = CaseE _; _} as e)); _})
    | IfPr ({it = CmpE (`EqOp, `BoolT, ({it = CaseE _; _} as e), {it = VarE x; _}); _}) ->
      Either.Left ((x, e), prem)
    | RulePr (id, _, {it = TupE [_C; {it = VarE x; _}; e]; _}) when String.ends_with ~suffix:"_sub" id.it ->
      (* TODO: subtype is currently considered eq *)
      Either.Left((x, e), prem)
    | RulePr (id, _, {it = TupE [_C; e; {it = VarE x; _}]; _}) when String.ends_with ~suffix:"_sub" id.it ->
      (* TODO: subtype is currently considered eq *)
      if (Il.Free.free_exp e).varid |> Il.Free.Set.mem "C" then
        Either.Right prem
      else
        Either.Left((x, e), prem)
    | _ -> Either.Right prem
  in
  match List.partition_map is_eq_prem prems with
  | ((x, e), _) :: tl, prems ->
    let prems' = List.split tl |> snd in
    List.map (transform_prem @@ replace_id_with x.it e) (prems' @ prems) |> simplify_equality
  | _ -> prems

let fix_iterlen_prems prems =
  let lens = ref [] in
  let get_len e =
    List.find_map (fun (e', l) -> if Il.Eq.eq_exp e e' then Some l else None) !lens
  in
  let fix_lens es =
    let ls = List.map get_len es in
    let l_opt = List.fold_left (fun acc cur ->
      match acc, cur with
      | None, None -> None
      | None, s
      | s, None -> s
      | Some l1, Some l2 -> if l1 = l2 then Some l1 else raise (RejectedSample "iter len for new prems")
    ) None ls in
    let l =
      match l_opt with
      | Some l -> l
      | None -> Random.int 3
    in
    List.iter2 (fun e l_opt -> if l_opt = None then push (e, l) lens) es ls;
    l
  in
  let iter_to_list e =
    match e.it with
    | IterE (e', (List, xes)) ->
      let xs, es = List.split xes in
      let l = fix_lens es in
      let es = List.init l (fun i ->
        List.fold_left (fun e x ->
          transform_exp (replace_id x.it (x.it ^ "." ^ string_of_int i)) e
        ) e' xs
      ) in
      {e with it = ListE es}
    | _ -> e
  in
  List.map (transform_prem iter_to_list) prems

let unroll_rule p =
  match p.it with
  | RulePr (id, _mixop, exp) when id.it <> "Expand" ->
    let rules = get_rules id in

    let rules = List.filter_map (fun r ->
      let RuleD (_, _, _, exp', _) = r.it in
      try
        Some (r, unify_exp [] exp exp')
      with
        | UnifyFail _ -> None
    ) rules in

    (match rules with
    | [r, unify_result] -> (* Only if there is exactly one applicable rule *)
      let RuleD (_, _, _, _, prems) = r.it in
      let prems =
        List.map (apply_unify_result_prem unify_result) prems
        |> fix_iterlen_prems
      in
      prems, unify_result (* TODO: Needs variable renaming *)
    | _ -> [p], []
    )
  | _ -> [p], []

let rec concretize_prems prems =
  (* 1. Unroll RulePr *)
  let premss, unify_results =
    List.map unroll_rule prems
    |> List.split
  in
  let prems = List.concat premss in
  let unify_result = List.concat unify_results in
  let prems = List.map (apply_unify_result_prem unify_result) prems in
  if unify_result <> [] then concretize_prems prems else

  (* 2. Concretize free vars *)
  let free_vars = ref [] in
  List.map (transform_prem (fun e ->
    match e.it with
    | VarE _ -> push e free_vars; e
    | _ -> e
  )) prems |> ignore;
  let freq = count_freq Il.Eq.eq_exp !free_vars in
  let free_vars = List.filter_map (fun (e, cnt) ->
    match e.it with
    | VarE {it = "C"; _} -> None
    | _ -> if cnt = 1 then None else Some e
  ) freq in

  if free_vars = [] then prems else

  let prems' = try_n 1000 "Concretizing premise" (fun () ->
    List.fold_left (fun prems_opt e ->
      let* prems = prems_opt in
      let e' = gen_typ e.note in
      let prems' =
        prems
        |> List.map (transform_prem (replace e e'))
        |> List.map reduce_prem
      in
      if contains_false prems' then
        None
      else
        Some prems'
    ) (Some prems) free_vars
  ) in

  concretize_prems prems'

(* Unconditioned concretization of free vars *)
let concretize_free exp =
  let free_vars = ref [] in
  transform_exp (fun e ->
    match e.it with
    | VarE id when not @@ List.mem id.it ["_"; "C"] -> push e free_vars; e
    | _ -> e
  ) exp |> ignore;

  let replaces = ref [] in

  dedup Il.Eq.eq_exp (List.rev !free_vars)
  |> List.fold_left (fun exp e ->
    let t = List.fold_left (fun t (e, e') -> transform_typ (replace e e') t) e.note !replaces in
    let e' = gen_typ t in
    push (e, e') replaces;
    transform_exp (replace e e') exp
  ) exp

let concretize_free_prem prem =
  match prem.it with
  | IfPr e -> {prem with it = IfPr (concretize_free e)}
  | RulePr (id, mixop, e) -> {prem with it = RulePr (id, mixop, concretize_free e)}
  | _ -> prem

(* 3. fix_immediate: determine and concretize the immediates of each instr *)
let rec fix_immediate (trules: rule list): exp list =
  Debug_log.(log "bottom_up.fix_immediate"
    (fun () -> trules |> List.map string_of_rule |> String.concat "\n" )
    (fun exps -> exps |> List.map string_of_exp |> String.concat "; " )
  ) @@ fun _ ->
  List.mapi (fun i trule ->
    Debug_log.(log "bottom_up.fix_immediate.loop"
      (fun () -> string_of_rule trule)
      (fun e -> string_of_exp e)
    ) @@ fun _ ->
    let i = i - !values_cnt in

    let iter_to_list' e =
      match e.it with
      | IterE (e', (List, xes)) ->
        if Il.Eq.eq_typ e'.note (mk_VarT "instr") then e else (* instr* will be handled manaully *)

        let xs, es = List.split xes in
        let l = fix_lengths i 3 es in
        let es = List.init l (fun i ->
          List.fold_left (fun e x ->
            transform_exp (replace_id x.it (x.it ^ "." ^ string_of_int i)) e
          ) e' xs
        ) in
        let it = ListE es in
        { e with it }
      | IterE (e', (Opt, xes)) ->
        let _xs, es = List.split xes in
        let l = fix_lengths i 1 es in (* l is 0 or 1 *)

        { e with it =
          if l = 0 then
            OptE None
          else
            OptE (Some e') }
      | _ -> e
    in
    let expand_iterpr p =
      match p.it with
      | IterPr (p, (List, xes)) ->
        let xs, es = List.split xes in
        let l = fix_lengths i 3 es in
        List.init l (fun i ->
          List.fold_left (fun p x ->
            transform_prem (replace_id x.it (x.it ^ "." ^ string_of_int i)) p
          ) p xs
        )
      | _ -> [p]
    in

    let iter_to_list = transform_exp iter_to_list' in
    let iter_to_list_prem p = transform_prem iter_to_list' p |> expand_iterpr in

    let trule = try_n 10 "fixing iter len" (fun () ->
      let RuleD (id, binds, mixop, exp, prems) = trule.it in

      let unify_result' = List.fold_left (register_iterlen_cond i) [] prems in
      let exp' = exp |> iter_to_list |> apply_unify_result unify_result' in
      let prems' = prems |> List.concat_map iter_to_list_prem |> List.map (apply_unify_result_prem unify_result') in

      if contains_false prems' then
        let _ = reset_iterlen_cond i in
        None
      else
        let it = RuleD (id, binds, mixop, exp', prems') in
        Some {trule with it}
    ) in

    let trule = handle_special_prems trule in
    let trule = handle_context_type_prems trule in
    let instr = rule_to_instr trule in
    let trule, instr = concretize_instr trule instr in

    let prems =
      rule_to_prems trule
      |> simplify_equality (* TODO: This should be moved to someting like unify *)
      |> concretize_prems
      |> List.map concretize_free_prem
    in

    sideconds := (
      prems |> List.concat_map as_sidecond
    ) @ !sideconds;

    instr
  ) trules

(* Hardcoded handlers for special kinds of relations *)
and handle_special_prems trule =
  let RuleD (id, binds, mixop, exp, prems) = trule.it in

  (* 1. Handle Instrs_ok *)
  let is_instrs_ok p =
    match p.it with
    | RulePr ({it = "Instrs_ok"; _}, _, {it = TupE [
        _C;
        {it = IterE _; _} as instrs;
        arrow
      ]; _}) ->
      (* Assumption: Arrow is already concretized *)
      let rt1 = nth_arg_of_case 0 arrow |> nth_arg_of_case 0 |> exp_to_list in
      let rt2 = nth_arg_of_case (if !Flag.version = 3 then 2 else 1) arrow |> nth_arg_of_case 0 |> exp_to_list in
      Either.Left (instrs, rt1, rt2)
    | _ ->
      Either.Right p
  in
  let oks, prems = List.partition_map is_instrs_ok prems in

  let exp, prems = List.fold_left (fun (exp, prems) (instrs, rt1, rt2) ->
    let instrs' = il_list (gen_default_instrs rt1 rt2) (mk_VarT "instr") in
    let f = replace instrs instrs' in
    transform_exp f exp,
    List.map (transform_prem f) prems
  ) (exp, prems) oks in

  (* 2. Handle Blocktype_ok *)
  let is_blocktype_ok p =
    match p.it with
    | RulePr ({it = "Blocktype_ok"; _}, _, {it = TupE [
        _C;
        bt;
        arrow
      ]; _}) ->
      (* Assumption: Arrow is already concretized *)
      let rt1 = nth_arg_of_case 0 arrow |> nth_arg_of_case 0 |> exp_to_list in
      let rt2 = nth_arg_of_case (if !Flag.version = 3 then 2 else 1) arrow |> nth_arg_of_case 0 |> exp_to_list in
      Either.Left (bt, rt1, rt2)
    | _ ->
      Either.Right p
  in
  let oks, prems = List.partition_map is_blocktype_ok prems in

  let exp, prems = List.fold_left (fun (exp, prems) (bt, rt1, rt2) ->
    let bt' =
      match rt1, rt2 with
      | [], [] when Random.bool () -> il_case "_RESULT" "blocktype" [il_opt None (mk_VarT "valtype")]
      | [], [vt] when Random.bool () -> il_case "_RESULT" "blocktype" [il_opt (Some vt) (mk_VarT "valtype")]
      | _ -> il_case "_IDX" "blocktype" [register_func_typ rt1 rt2]
    in
    let f = replace bt bt' in
    transform_exp f exp,
    List.map (transform_prem f) prems
  ) (exp, prems) oks in

  (* re-construct *)
  let it = RuleD (id, binds, mixop, exp, prems) in
  {trule with it}

and gen_default_instr' vt =
  let trules = fix_values vt in
  fix_immediate trules

and gen_default_instrs rt1 rt2 =
  match rt1, rt2 with
  | hd1 :: tl1, hd2 :: tl2 when Il.Eq.eq_exp hd1 hd2 -> gen_default_instrs tl1 tl2
  | _ ->
    List.map (fun _ -> il_case "DROP" "instr" []) rt1
    @ List.concat_map gen_default_instr' rt2

(* 3.5 patch: Make manual, syntactic patch to instrs. Eventually should be removed, or automated *)
let rec patch instrs =
  let handle_rec i instr =
    let instrs = nth_arg_of_case i instr in
    let instrs' =
      match instrs.it with
      | ListE is -> {instrs with it = ListE (patch is)}
      | _ -> instrs
    in
    replace_caseE_arg [i] instrs' instr
  in
  List.map (fun instr ->
    (match case_of_case instr with
    | Atom "VEXTRACT_LANE" ->
      let sx_opt =
        match instr |> nth_arg_of_case 0 |> nth_arg_of_case 0 |> case_of_case with
        | Atom ("I8" | "I16") -> {(nth_arg_of_case 1 instr) with it = OptE (Some (gen_typ (mk_VarT "sx")))}
        | _ -> {(nth_arg_of_case 1 instr) with it = OptE None}
      in
      instr
      |> replace_caseE_arg [1] sx_opt
    | Atom "VLOAD" ->
      let vloadop = nth_arg_of_case 1 instr in
      let vloadop' =
        match vloadop.it with
        | OptE (Some e) ->
          let e' =
            match case_of_case e with
            | Atom "SHAPE" ->
              let sz, m = choose [8,8; 16,4; 32,2] in
              let sz = il_case "" "sz" [exp_of_int sz] in
              let m = exp_of_int m in
              e |> replace_caseE_arg [0] sz |> replace_caseE_arg [1] m
            | Atom "ZERO" ->
              let sz = gen_typ (mk_VarT "sz") |> replace_caseE_arg [0] (exp_of_int (32 // 64)) in
              e |> replace_caseE_arg [0] sz
            | _ -> e
          in
          { vloadop with it = OptE (Some e') }
        | _ -> vloadop
      in
      instr
      |> replace_caseE_arg [1] vloadop'
    (* Recursive cases *)
    | Atom "BLOCK" -> instr |> handle_rec 1
    | Atom "LOOP"  -> instr |> handle_rec 1
    | Atom "IF" -> instr |> handle_rec 1 |> handle_rec 2
    | Atom "TRY_TABLE"  -> instr |> handle_rec 2
    | _ -> instr
    )
  ) instrs

(* 4. wrap_as_func: Wrap the generated instruction sequence with func, including params and blocks *)
let wrap_as_func (instrs: exp list) (rt: restype) =
  (* 1. If sidecondition contains something about local, generate locals *)
  let extract_local_sidecond = extract_context_sidecond "LOCALS" (fun e ->
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
    | ListE rt -> Some rt
    | _ -> None)
  in
  let label_conds = List.filter_map extract_label_sidecond !sideconds in
  let block_cnt = 1 + List.fold_left max (-1) (List.split label_conds |> fst) in

  let rec wrap_as_block i acc (rt:restype) =
    if i = block_cnt then acc, rt else
    match List.assoc_opt i label_conds with
    | None ->
      let blocktype = [register_func_typ [] rt] |> il_case "_IDX" "blocktype" in
      wrap_as_block (i+1)
      [il_case "BLOCK" "instr" [blocktype; ListE acc |> to_phrase (IterT (mk_VarT "instr", List) $ no_region)]]
      rt
    | Some rt'->
      let blocktype = [register_func_typ [] rt'] |> il_case "_IDX" "blocktype" in
      let suffix = gen_default_instrs rt rt' in
      wrap_as_block (i+1)
      [il_case "BLOCK" "instr" [blocktype; ListE (acc @ suffix) |> to_phrase (IterT (mk_VarT "instr", List) $ no_region)]]
      rt'
  in

  let instrs, rt = wrap_as_block 0 instrs rt in

  (* 3. If sidecondtion contains something about C.RETURN, append instrs and change the return type *)
  let extract_return_cond cond =
    match cond with
    | IfPrC {it = CmpE (`EqOp, `BoolT, {it = DotE (_C, {it = Atom "RETURN"; _}); _}, {it = OptE (Some rt); _}); _} -> Some rt
    | IfPrC {it = CmpE (`EqOp, `BoolT, {it = OptE (Some rt); _}, {it = DotE (_C, {it = Atom "RETURN"; _}); _}); _} -> Some rt
    | _ -> None
  in
  let return_conds = List.filter_map extract_return_cond !sideconds in
  let instrs, rt =
    match return_conds with
    | rt' :: _ -> (* TODO: What if there are multiple 'rt''s? *)
      let rt' = rt' |> nth_arg_of_case 0 |> exp_to_list in
      let suffix = gen_default_instrs rt rt' in
      instrs @ suffix, rt'
    | _ ->
      instrs, rt
  in

  (* Wrap as function *)
  let typeidx = register_func_typ
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

let gen_stuffs fname extract default name tname f_args =
  let conds = List.filter_map extract !sideconds in
  let lens = List.filter_map (extract_context_len_sidecond fname) !sideconds in
  let cnt = 1 + List.fold_left max (-1) (List.split conds |> fst) in
  let cnt = List.fold_left max cnt lens in
  List.init cnt (fun i ->
    let v =
      match List.assoc_opt i conds with
      | None -> default ()
      | Some x -> x
    in
    il_case name tname (f_args v)
  )

(* 5. wrap_as_func: Wrap the generated function sequence with module, including types, globals, etc. *)
let wrap_as_module (func: exp) =
  (* 2. Generate globals *)
  let extract_global_sidecond = extract_context_sidecond "GLOBALS" (fun e ->
    match e.it with
    | CaseE ([[]; []; []], {it = TupE [mut; t]; _}) ->
      let is_mut e =
        match e.it with
        | OptE None -> false
        | OptE (Some _) -> true
        | _ -> Random.bool ()
      in
      Some (is_mut mut, t)
    | _ -> None)
  in

  let construct_gt mut t =
    il_case "" "globaltype" [
      (if mut then il_some else il_none) "MUT" "mut";
      t
    ]
  in

  let globals = gen_stuffs
    "GLOBALS"
    extract_global_sidecond
    (fun _ -> false, il_case "I32" "valtype" []) (* default *)
    "GLOBAL"
    "global"
    (fun (mut, t) ->
      (* TODO: GLOBAL must be const *)
      [construct_gt mut t; ListE (gen_default_instr' t) |> to_phrase (mk_VarT "expr")]
    )
  in

  (* 3. Generate tables *)
  let extract_table_sidecond = extract_context_sidecond "TABLES" (fun e ->
    match e.it with
    | CaseE ([[]; []; []; []], {it = TupE [at; lim; rt]; _}) ->
      Some (at, lim, rt)
    | _ -> None)
  in

  let default_table () =
    let mixop = Xl.Atom.[
      [LBrack |> to_phrase (info "[")];
      [Dot2 |> to_phrase (info "..")];
      [RBrack |> to_phrase (info "]")]
    ] in
    il_case "I32" "addrtype" [], (* TODO: Maybe... this can be automated? *)
    CaseE (mixop, il_tup [il_zero; il_zero]) |> to_phrase (mk_VarT "limits"),
    il_case "REF" "reftype" [il_some "NULL" "nul"; il_case "FUNC" "heaptype" []]
  in

  let tables = gen_stuffs
    "TABLES"
    extract_table_sidecond
    default_table
    "TABLE"
    "table"
    (fun (at, lim, rt) ->
      [il_case "" "tabletype" [at; lim; rt]; ListE (gen_default_instr' rt) |> to_phrase (mk_VarT "expr")]
    )
  in

  (* 4. Generate mems *)
  let extract_mem_sidecond = extract_context_sidecond "MEMS" (fun e -> Some e) in
  let default_mem () = gen_typ (mk_VarT "memtype") in
  let mems = gen_stuffs
    "MEMS"
    extract_mem_sidecond
    default_mem
    "MEMORY"
    "mem"
    (fun mt -> [mt])
  in

  (* 5. Generate tags *)
  let extract_tag_sidecond sidecond =
    match sidecond with
    | RulePrC ({it = "Expand"; _}, [[]; _; []], {it = TupE [
        { it = IdxE ({ it = DotE (_C, {it = Atom "TAGS"; _}); _ }, idx); _ };
        func
      ]; _}) -> Some (exp_to_int idx, func)
    | _ -> None
  in
  let default_tag () = arrow_to_func [] [] in
  let tags = gen_stuffs
    "TAGS"
    extract_tag_sidecond
    default_tag
    "TAG"
    "tag"
    (fun func -> [register_typ func])
  in

  (* 6. Generate elems *)
  let extract_elem_sidecond = extract_context_sidecond "ELEMS" (fun rt -> Some(rt, [], il_case "PASSIVE" "elemmode" [])) in
  let default_elem () =
    il_case "REF" "reftype" [il_some "NULL" "nul"; il_case "FUNC" "heaptype" []],
    [],
    il_case "PASSIVE" "elemmode" []
  in
  let elems = gen_stuffs
    "ELEMS"
    extract_elem_sidecond
    default_elem
    "ELEM"
    "elem"
    (fun (rt, es, mode) -> [rt; il_list es (mk_VarT "expr"); mode])
  in

  (* 7. Generate datas *)
  let extract_data_sidecond = (fun _ -> None) in (* Only number matters *)
  (* TODO: Generate various data *)
  let default_data () = List.init (Random.int 3) (fun _ -> gen_typ (mk_VarT "byte")), il_case "PASSIVE" "datamode" [] in
  let datas = gen_stuffs
    "DATAS"
    extract_data_sidecond
    default_data
    "DATA"
    "data"
    (fun (bs, datamode) -> [il_list bs (mk_VarT "byte"); datamode])
  in

  (* 8. Generate funcs *)
  let extract_func_sidecond = function
    | RulePrC ({it = "Expand"; _}, [[]; _; []], {it = TupE [
        { it = IdxE ({ it = DotE (_C, {it = Atom "FUNCS"; _}); _ }, idx); _ };
        func
      ]; _}) ->
      let arrow = func |> nth_arg_of_case 0 in
      let nth i arrow = nth_arg_of_case i arrow |> nth_arg_of_case 0 |> exp_to_list in
      Some (exp_to_int idx, (nth 0 arrow, nth 1 arrow))
    | _ -> None
  in
  let default_func () = [], [] in
  let local_get i = il_case "LOCAL.GET" "instr" [exp_of_int i] in
  let funcs = gen_stuffs
    "FUNCS"
    extract_func_sidecond
    default_func
    "FUNC"
    "func"
    (fun (rt1, rt2) -> [
      register_func_typ rt1 rt2;
      il_list [] (mk_VarT "local");
      il_list (List.mapi (fun i _ -> local_get i) rt1 @ gen_default_instrs rt1 rt2) (mk_VarT "instr");
    ])
  in
  let funcs = funcs @ [func] in

  (* 9. Generate types *)
  let to_rectype comptype =
    il_case "REC" "rectype" [
      il_list [il_case "SUB" "subtype" [
        il_some "FINAL" "fin";
        il_list [] (mk_VarT "typeuse");
        comptype;
      ]] (mk_VarT "subtype")
    ]
  in

  let types = gen_stuffs
    "TYPES"
    extract_type_sidecond
    (fun _ -> arrow_to_func [] [])
    "TYPE"
    "type"
    (fun t -> [
      match !Flag.version with
      | 3 -> to_rectype t
      | _ -> t
    ])
  in

  let imports = [] in
  let exports = [] in

  il_case "MODULE" "module" [
    il_list types (mk_VarT "type");
    il_list imports (mk_VarT "import");
    il_list funcs (mk_VarT "func");
    il_list globals (mk_VarT "global");
    il_list tables (mk_VarT "table");
    il_list mems (mk_VarT "mem");
    il_list tags (mk_VarT "tag");
    il_list elems (mk_VarT "elem");
    il_list datas (mk_VarT "data");
    OptE None |> to_phrase (IterT (mk_VarT "start", Opt) $ no_region);
    il_list exports (mk_VarT "export");
  ]

(* HARDCODE: premise rewriter for `Resulttype_sub: C |- t1* <: t2*` of RETURN_CALL *)
let handle_trivial_equality r =
  let rec is_simple_var e =
    match e.it with
    | VarE _ -> true
    | IterE (e, (_, [_, _])) -> is_simple_var e
    | _ -> false
  in
  let is_trivial_equality prem =
    match prem.it with
    | IfPr ({it = CmpE (`EqOp, `BoolT, l, r); _})
        when is_simple_var l && is_simple_var r ->
      Some (l, r)
    | RulePr (id, _, {it = TupE [_C; l; r]; _})
        when String.ends_with ~suffix:"_sub" id.it && is_simple_var l && is_simple_var r ->
      Some (l, r)
    | _ -> None
  in

  let RuleD (id, binds, mixop, exp, prems) = r.it in
  let eqs = List.filter_map is_trivial_equality prems in
  let aux e = List.fold_left (fun e (e1, e2) -> replace e1 e2 e) e eqs in
  let exp' = transform_exp aux exp in
  let prems' = List.map (transform_prem aux) prems in
  {r with it = RuleD (id, binds, mixop, exp', prems')}

(* Generates the simplest module, which contains the instruction sequence with whose names are `cases` *)
let gen_module (cases: string list): Al.Ast.value =
  (* 0. Init *)
  trules := get_typing_rules () |> List.map handle_trivial_equality;
  sideconds := [];

  (* 1. Fix rt *)
  Log.debug ("===1===");
  let rts, trules = fix_rts cases in (* May throw, if this combination is impossible *)

  (* 2. Prepend values *)
  Log.debug ("===2===");
  let truless = List.map fix_values (List.hd rts |> List.rev) in
  let trules' = List.flatten truless in
  let trules = trules' @ trules in
  values_cnt := List.length trules';

  (* 3. Fix immediates *)
  Log.debug ("===3===");
  let instrs = fix_immediate trules in (* May throw, if it is impossible to fill in immediates *)

  (* 3.5 Manual patch *)
  Log.debug ("===3.5===");
  let instrs = patch instrs in

  (* 4. Wrap as a function *)
  Log.debug ("===4===");
  let func = wrap_as_func instrs (List.rev (List.hd (List.rev rts))) in (* TODO: It's too confusing to decide when to rev or not *)

  (* 5. Wrap as a module *)
  Log.debug ("===5===");
  let module_ = wrap_as_module func |> reduce_exp in
  Log.debug (string_of_exp module_);

  (* 6. IL2AL *)
  Log.debug ("===6===");
  let al_module = module_
  |> Il2al.Translate.translate_exp
  |> Backend_interpreter.Interpreter.eval_expr Backend_interpreter.Ds.Env.empty
  in

  Log.debug (Al.Print.string_of_value al_module);

  al_module

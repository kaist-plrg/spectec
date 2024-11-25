open Utils

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

(* Smart Constructors *)
let mk_VarT x = VarT (x $ no_region, []) $ no_region
let il_case name tname args =
  CaseE (
    (if name = "" then [] else [El.Atom.Atom name $$ no_region % (El.Atom.info name)]) :: (List.map (fun _ -> []) args),
    TupE args $$ no_region % (TupT (List.map (fun a -> (a, a.note)) args) $ no_region)
  ) $$ no_region % (mk_VarT tname)
let il_list es t =
  ListE es $$ no_region % (IterT (t, List) $ no_region)
let il_tup es =
  TupE es $$ no_region % (TupT (List.map (fun e -> e, e.note) es) $ no_region)
let some_opt = OptE (Some (il_tup [])) |> to_phrase (IterT (TupT [] $ no_region, Opt) $ no_region)
let none_opt = OptE None |> to_phrase (IterT (TupT [] $ no_region, Opt) $ no_region)
let il_some x t = CaseE ([[El.Atom.Atom x |> to_phrase (El.Atom.info x)]; [El.Atom.Quest |> to_phrase (El.Atom.info "?")]], il_tup [some_opt]) |> to_phrase (mk_VarT t)
let il_none x t = CaseE ([[El.Atom.Atom x |> to_phrase (El.Atom.info x)]; [El.Atom.Quest |> to_phrase (El.Atom.info "?")]], il_tup [none_opt]) |> to_phrase (mk_VarT t)
let il_zero = NatE Z.zero |> to_phrase (mk_VarT "u32")

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

let rec replace_caseE_arg is re e =
  match is, e.it with
  | [], _ ->
    re
  | i :: is, CaseE (mixop, ({ it = TupE es; _ } as tup)) ->
    let es' = List.mapi (fun i' e' -> if i = i' then replace_caseE_arg is re e' else e') es in
    { e with it = CaseE (mixop, { tup with it = TupE es' })}
  | 0 :: is, CaseE (mixop, e') ->
    { e with it = CaseE (mixop, replace_caseE_arg is re e') }
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

(* HARDCODE: force valid expressions *)
let validate x e =
  match x with
  | "limits" ->
    let l = nth_arg_of_case 0 e in
    let r = nth_arg_of_case 1 e in
    if nth_arg_of_case 0 l <= nth_arg_of_case 0 r then e else
    e |> replace_caseE_arg [0] r |> replace_caseE_arg [1] l
  | _ -> e
let rec gen c x =
  (* HARDCODE: list *)
  if x = "list" then
    let t = (List.hd c.args) |> (fun a -> match a.it with | TypA t -> t | _ -> failwith "syntax list(syntax X)") in
    gen_typ c (IterT (t, List) $ no_region)
  else
  let a2e a =
    match a.it with
    | ExpA e -> Il.Eval.reduce_exp !Langs.il_env e
    | _ -> failwith @@ "Unsupported arg for " ^ x
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
  (match deftyp.it with
  | AliasT typ -> gen_typ c (transform_typ replace_params typ)
  | StructT typfields ->
    let gen_typfield (atom, (_binds, typ, _prems), _hints) =
      atom, gen_typ c (transform_typ replace_params typ)
    in
    StrE (List.map gen_typfield typfields) |> to_phrase c.typ
  | VariantT typcases ->
    let typcases = Lib.List.filter_not (has_subid_hint "sem") typcases in
    let typcases = Lib.List.filter_not (has_subid_hint "admin") typcases in
    let typcase = Utils.choose typcases in
    let typcase' =
      let (m, (bs, t, ps), hs) = typcase in
      let t' = transform_typ replace_params t in
      m, (bs, t', ps), hs
    in
    gen_typcase c typcase'
  ) |> validate x
and gen_typcase c (mixop, (_binds, typs, _prems), _hints) =
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
  | IterT (typ', List) ->
    let len = Random.int 3 in (* 0, 1, 2 *)
    ListE (List.init len (fun _ -> gen_typ c typ')) |> to_phrase typ
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
  let free_vars = ref [] in
  List.map (transform_expr (fun e ->
    match e.it with
    | VarE x -> free_vars := (x.it, e.note) :: !free_vars; e
    | _ -> e
  )) (List.flatten ess) |> ignore;
  dedup (fun x y -> (fst x) = (fst y)) !free_vars
  |>
  List.fold_left (fun ess (x, typ) ->
    let e = gen_typ typ in
    List.map (List.map (transform_expr (replace_id_with x e))) ess
  ) ess

(* 1. fix_rts: pre-determine concrete types of each cases *)
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

(* 2. fix_values: generate necessary values in front of main instrs *)
let fix_values vt: string list * restype list =
  match vt.it with
  (* HARDCODE: Default instr name for each type *)
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
        let vt' = vt |> replace_caseE_arg [0; 0] some_opt in
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

let rec simplify_equality prems =
  (* If there is equality prems within these prems, where one side is a variable, simplify the whole prems *)
  (* Assumption: No cyclic binding *)
  let is_eq_prem prem =
    match prem.it with
    | IfPr ({it = CmpE (EqOp, {it = VarE x; _}, e); _})
    | IfPr ({it = CmpE (EqOp, e, {it = VarE x; _}); _}) ->
      Either.Left ((x, e), prem)
    | RulePr (id, _, {it = TupE [_C; {it = VarE x; _}; e]; _}) when String.ends_with ~suffix:"_sub" id.it ->
      (* TODO: subtype is currently considered eq *)
      Either.Left((x, e), prem)
    | _ -> Either.Right prem
  in
  match List.partition_map is_eq_prem prems with
  | ((x, e), _) :: tl, prems ->
    let prems' = List.split tl |> snd in
    List.map (transform_prem @@ replace_id_with x.it e) (prems' @ prems) |> simplify_equality
  | _ -> prems

let concretize_prems prems =
  let free_vars = ref [] in
  List.map (transform_prem (fun e ->
    match e.it with
    | VarE _ -> free_vars := e :: !free_vars; e
    | _ -> e
  )) prems |> ignore;
  dedup Il.Eq.eq_exp !free_vars
  |> List.fold_left (fun prems e ->
    match e.it with
    | VarE {it = "C"; _} -> prems
    | _ ->
      let e' = gen_typ e.note in
      prems |> List.map (transform_prem (replace e e'))
  ) prems

(* 3. fix_immediate: determine and concretize the immediates of each instr *)
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
        let l =
          match get_cached_length e' with
          | None -> Random.int 3
          | Some l -> l
        in
        let es = List.init l (fun i ->
          List.fold_left (fun e (x, _) ->
            transform_expr (replace_id x.it (x.it ^ "." ^ string_of_int i)) e
          ) e' xes) in
        let it = ListE es in
        { e with it }
      | _ -> e
    in
    let iter_to_list = transform_expr iter_to_list' in
    let iter_to_list_prem = transform_prem iter_to_list' in (* TODO: Handle IterPr *)

    (* Transform trule *)
    print_endline case;
    let trule = List.filter (fun r ->
      let RuleD (id, _, _, _, _) = r.it in
      String.uppercase_ascii id.it
      |> String.split_on_char '-'
      |> List.hd
      = case
    ) !trules |> choose in

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

    sideconds := (
      rule_to_prems trule
      (* |> simplify_equality *) (* TODO: This should be moved to someting like unify *)
      |> concretize_prems
      |> List.concat_map as_sidecond
    ) @ !sideconds;

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

let extract_context_len_sidecond field sidecond =
  match sidecond with
  | IfPrC {it = CmpE (
      LtOp _,
      len,
      {it = LenE ({it = DotE (_C, {it = Atom field'; _}); _}); _}
    ); _}
  | IfPrC {it = CmpE (
      GtOp _,
      {it = LenE ({it = DotE (_C, {it = Atom field'; _}); _}); _},
      len
    ); _}
  when field = field'
  ->
    Some (exp_to_int len + 1)
  | IfPrC {it = CmpE (
      LeOp _,
      len,
      {it = LenE ({it = DotE (_C, {it = Atom field'; _}); _}); _}
    ); _}
  | IfPrC {it = CmpE (
      GeOp _,
      {it = LenE ({it = DotE (_C, {it = Atom field'; _}); _}); _},
      len
    ); _}
  when field = field'
  ->
    Some (exp_to_int len)
  | _ -> None


(* TODO: This code heavily overlaps with fix_value. Do something. *)
let gen_default_instr' vt =
  let names, rts = fix_values vt in
  fix_immediate names ([] :: rts)

let rec gen_default_instrs rt1 rt2 =
  match rt1, rt2 with
  | hd1 :: tl1, hd2 :: tl2 when Il.Eq.eq_exp hd1 hd2 -> gen_default_instrs tl1 tl2
  | _ ->
    List.map (fun _ -> il_case "DROP" "instr" []) rt1
    @ List.concat_map gen_default_instr' rt2

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

let register_typ rt1 rt2 =
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

(* 4. wrap_as_func: Wrap the generated instruction sequence with func, including params and blocks *)
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
  ignore func;

  (* 1. Generate types *)
  let arrow_to_type rt1 rt2 =
    let func = CaseE ([[]; [El.Atom.Arrow $$ no_region % El.Atom.info "->"]; []], il_tup [il_list rt1 (mk_VarT "valtype"); il_list rt2 (mk_VarT "valtype")]) $$ no_region % mk_VarT "functype" in
    match !Flag.version with
    | 3 ->
        il_case "REC" "rectype" [
          il_list [il_case "SUB" "subtype" [
            il_some "FINAL" "fin";
            il_list [] (mk_VarT "typeuse");
            il_case "FUNC" "comptype" [func];
          ]] (mk_VarT "subtype")
        ]
    | _ -> func
  in

  let types = gen_stuffs
    "TYPES"
    extract_type_sidecond
    (fun _ -> [], [])
    "TYPE"
    "type"
    (fun (rt1, rt2) -> [arrow_to_type rt1 rt2])
  in

  (* 2. Generate globals *)
  let extract_global_sidecond = extract_context_sidecond "GLOBALS" (fun e ->
    match e.it with
    | CaseE ([[]; []; []], {it = TupE [mut; t]; _}) ->
      let is_mut e =
        match e.it with
        | CaseE ([[{it = Atom "MUT"; _}]; [{it = El.Atom.Quest; _}]], {it = TupE [{it = OptE (Some _); _}]; _}) -> true
        | CaseE ([[{it = Atom "MUT"; _}]; [{it = El.Atom.Quest; _}]], {it = TupE [{it = OptE None; _}]; _}) -> false
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
    | CaseE ([[]; []; []], {it = TupE [lim; rt]; _}) ->
      Some (lim, rt)
    | _ -> None)
  in

  let default_table () =
    let mixop = El.Atom.[
      [LBrack |> to_phrase (info "[")];
      [Dot2 |> to_phrase (info "..")];
      [RBrack |> to_phrase (info "]")]
    ] in
    CaseE (mixop, il_tup [il_zero; il_zero]) |> to_phrase (mk_VarT "limits"),
    il_case "REF" "reftype" [il_some "NULL" "nul"; il_case "FUNC" "heaptype" []]
  in

  let tables = gen_stuffs
    "TABLES"
    extract_table_sidecond
    default_table
    "TABLE"
    "table"
    (fun (lim, rt) ->
      [il_case "" "tabletype" [lim; rt]; ListE (gen_default_instr' rt) |> to_phrase (mk_VarT "expr")]
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
  let extract_tag_sidecond = (fun _ -> None) in (* TODO *)
  let default_tag () = gen_typ (mk_VarT "typeidx") in
  let tags = gen_stuffs
    "TAGS"
    extract_tag_sidecond
    default_tag
    "TAG"
    "tag"
    (fun tid -> [tid])
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
  let default_data () = List.init (Random.int 3) (fun _ -> gen_typ (mk_VarT "byte")), il_case "PASSIVE" "datamode" [] in
  let datas = gen_stuffs
    "DATAS"
    extract_data_sidecond
    default_data
    "DATA"
    "data"
    (fun (bs, datamode) -> [il_list bs (mk_VarT "byte"); datamode])
  in

  let funcs = [func] in (* TODO *)

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


(* Generates the simplest module, which contains the instruction sequence with whose names are `cases` *)
let gen_module (cases: string list): Al.Ast.value =
  (* 0. Init *)
  trules := get_typing_rules ();
  arrow_map := !trules |> List.map rule_to_arrow;
  sideconds := [];
  let cases = List.map (fun x -> if x = "" then choose !arrow_map |> fst else x) cases in

  (* 1. Fix rt *)
  let rts = fix_rts cases in (* May throw, if this combination is impossible *)

  (* 2. Prepend values *)
  let (casess, rtss) = List.map fix_values (List.hd rts |> List.rev) |> List.split in
  let cases' = List.flatten casess in
  let cases = cases' @ cases in
  let rts = (accumulate_rtss rtss) @ List.tl rts in
  values_cnt := List.length cases';

  (* 3. Fix immediates *)
  let instrs = fix_immediate cases rts in (* May throw, if it is impossible to fill in immeidates *)

  (* 4. Wrap as a function *)
  let func = wrap_as_func instrs (List.rev (List.hd (List.rev rts))) in (* TODO: It's too confusing to decide when to rev or not *)

  (* 5. Wrap as a module *)
  let module_ = wrap_as_module func in

  (* 6. IL2AL *)
  let al_module = module_
  |> Il2al.Translate.translate_exp
  |> Backend_interpreter.Interpreter.eval_expr Backend_interpreter.Ds.Env.empty
  in

  Log.debug (Al.Print.string_of_value al_module);

  al_module

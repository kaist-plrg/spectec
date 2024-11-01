open Util
open Source

open Langs
(* open Valid *)

(* open Al.Ast *)
(* open Al.Al_util *)

open Il2al.Il_walk

(** Helpers **)
let replace old_e new_e e =
  if Il.Eq.eq_exp old_e e then new_e else e

let replace_id old_id new_id e =
  match e.it with
  | Il.Ast.VarE id when id.it = old_id -> {e with it = Il.Ast.VarE {id with it = new_id}}
  | _ -> e

let replace_id_using f e =
  match e.it with
  | Il.Ast.VarE id -> {e with it = Il.Ast.VarE {id with it = f (id.it)}}
  | _ -> e

let replace_id_with old_id new_e e =
  match e.it with
  | Il.Ast.VarE id when id.it = old_id -> new_e
  | _ -> e

let rec dedup eq = function
| [] -> []
| hd :: tl -> hd :: dedup eq (Lib.List.filter_not (eq hd) tl)

let to_phrase ty x = x $$ no_region % ty

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

  exception DispatchFail of string

  let rec has_deftyp a dt =
    (* print_endline (Printf.sprintf "has_deftype %s %s ?" (Il.Print.string_of_exp a) (Il.Print.string_of_deftyp `H dt)); *)
    match a.it, dt.it with
    | Il.Ast.CaseE (mixop, {it = TupE []; _}), Il.Ast.VariantT typcases ->
      List.exists (fun (mixop', _, _) -> Il.Mixop.eq mixop mixop') typcases
    | _, Il.Ast.AliasT t -> has_type a t
    | _, Il.Ast.VariantT [ [[]; []], ([ bind ], _, _), _ ] -> has_type a (typ_of_bind bind)
    (* HARDCODE: N x M *)
    (*
    | Al.Ast.CaseV ("X", as), Il.Ast.VariantT [ typcase ] ->
      let (_mixop, (binds, _, _), _) = typcase in
      (* TODO: assert mixop = `%X%` *)
      List.for_all2 has_type as (List.map typ_of_bind binds)\
    *)
    | _ -> false
  and has_type a t =
    (* print_endline (Printf.sprintf "has_type %s %s ?" (Il.Print.string_of_exp a) (Il.Print.string_of_typ t)); *)
    match a.it, t.it with
    | Il.Ast.NatE _, Il.Ast.(NumT NatT) -> true
    | _, Il.Ast.VarT (name, []) -> has_deftyp a (dispatch_deftyp name.it [])
    | _ -> Il.Eq.eq_typ a.note t
  and has_argtype a p =
    (* print_endline (Printf.sprintf "has_argtype %s %s ?" (Il.Print.string_of_exp a) (Il.Print.string_of_arg p)); *)
    has_type a (type_of_arg p)

  and match_params args inst =
    match inst.it with
    | Il.Ast.InstD (_binds, params, deftyp) when (
        List.for_all2 has_argtype args params
      ) -> Some deftyp
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
  typ: Il.Ast.typ;
  args: Il.Ast.arg list;
}

let rec gen c x =
  match x with
  | "iN" -> Il.Ast.NatE (Utils.gen_bytes 4) |> to_phrase c.typ (* TODO *)
  | _ ->
    let a2e a = match a.it with | Il.Ast.ExpA e -> e | _ -> failwith "Unsupported arg" in
    let deftyp = dispatch_deftyp x (List.map a2e c.args) in
    match deftyp.it with
    | AliasT typ -> gen_typ c typ
    | StructT _ -> failwith "StructT not supported"
    | VariantT typcases ->
      let typcases = Lib.List.filter_not (Gen.has_subid_hint "sem") typcases in
      let typcase = Utils.choose typcases in
      gen_typcase c typcase
and gen_typcase c (mixop, (_binds, typs, _prems), _hint) =
  let args = Il.Ast.TupE (gen_typs c typs) |> to_phrase c.typ in
  Il.Ast.CaseE (mixop, args) |> to_phrase c.typ
and gen_typs c typs =
  match typs.it with
  | TupT typs' -> List.map (gen_typ c) (List.map snd typs')
  | _ -> [ gen_typ c typs ]
and gen_typ c typ =
  match typ.it with
  | NumT NatT -> Il.Ast.NatE (Random.int 3 |> Z.of_int) |> to_phrase typ (* 0, 1, 2 *)
  | VarT (id, args) -> gen {typ; args} id.it
  | IterT (typ', Opt) ->
    if Random.bool() then Il.Ast.OptE None |> to_phrase typ
    else Il.Ast.OptE (Some (gen_typ c typ')) |> to_phrase typ
  | _ -> failwith ("TODO: unhandled type for gen_typ: " ^ Il.Print.string_of_typ typ)
let gen_typ typ = gen_typ {typ; args = []} typ

(** End of Helpers **)

type valtype = Il.Ast.exp
type restype = valtype list

let trules = ref []

let expected_shape e shape =
  Printf.sprintf "Expected %s to be %s" (Il.Print.string_of_exp e) shape |> failwith

let rule_to_arrow rule =
  let open Il.Ast in

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
  let open Il.Ast in
  let RuleD (_, _, _, exp, _) = rule.it in
  match exp.it with
  | TupE [_c; lhs; _rhs] -> lhs
  | _ -> expected_shape exp "C |- lhs : rhs"

let rule_to_prems rule =
  let Il.Ast.RuleD (_, _, _, _, prems) = rule.it in
  prems

type sidecond =
  | TypeLenC of int * Il.Ast.exp * int
  | IfPrC of Il.Ast.exp
let sideconds: sidecond list ref = ref []

let as_sidecond pr =
  match pr.it with
  | Il.Ast.IfPr e -> [IfPrC e]
  | _ -> []

let rec unify_vts' map es1 es2 =
  let rec resolve e =
    match e.it with
    | Il.Ast.VarE x -> (match List.assoc_opt x.it map with Some e -> resolve e | None -> e)
    | _ -> e
  in
  match es1, es2 with
  | [], _ | _, [] -> map
  | e1::es1, e2::es2 ->
    match resolve e1, resolve e2 with (*TODO: Generalize this so that it can handle, i.e., function calls *)
    | e1, e2 when Il.Eq.eq_exp e1 e2 -> unify_vts' map es1 es2
    | e, {it = Il.Ast.VarE x; _}
    | {it = Il.Ast.VarE x; _}, e ->
      unify_vts' ((x.it, e) :: map) es1 es2
    | e1, e2 -> failwith ("Unification fail of " ^ (Il.Print.string_of_exp e1) ^ ", " ^ (Il.Print.string_of_exp e2))
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
    | Il.Ast.VarE x -> Some (x.it, e.note)
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
      | Il.Ast.ListE es -> es
      | Il.Ast.CatE (e1, e2) -> mk_vts e1 @ mk_vts e2
      | Il.Ast.IterE (e, (List, xes)) ->
        let length = get_cached_length e in
        List.init length (fun i ->
          List.fold_left (fun e (x, _) ->
            transform_expr (replace_id x.it (x.it ^ "." ^ string_of_int i)) e
          ) e xes
        )
      | _ -> [rt]
    in

    let remove_sub e = match e.it with | Il.Ast.SubE (e, _, _) -> e | _ -> e in

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
      | Il.Ast.RuleD (id, binds, mixop, exp, prems) ->
        let exp' = exp |> transform_expr (replace e e') in
        let prems' = prems |> List.map (transform_prem (replace e e')) in
        Il.Ast.RuleD (id, binds, mixop, exp', prems')
    } in
    let instr' = transform_expr (replace e e') instr in
    trule', instr'
  ) (trule, instr)

let fix_immediate (cases: string list) rts: Il.Ast.exp list =
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
      | TypeLenC (i', e', l) when i = i' && Il.Eq.eq_exp e e' -> Some l
      | _ -> None) !sideconds
    in
    let rec mk_vts rt =
      match rt.it with
      | Il.Ast.ListE es -> es
      | Il.Ast.CatE (e1, e2) -> mk_vts e1 @ mk_vts e2
      | Il.Ast.IterE (e, (List, xes)) ->
        let length = get_cached_length e |> Option.get in
        List.init length (fun i ->
          List.fold_left (fun e (x, _) ->
            transform_expr (replace_id x.it (x.it ^ "." ^ string_of_int i)) e
          ) e xes
        )
      | _ -> [rt]
    in

    let remove_sub e = match e.it with | Il.Ast.SubE (e, _, _) -> e | _ -> e in

    let vts1 = rt1' |> mk_vts |> List.map remove_sub in
    let vts2 = rt2' |> mk_vts |> List.map remove_sub in

    assert (List.length rt1 >= List.length vts1);
    assert (List.length rt2 >= List.length vts2);

    let unify_result = unify_vts rt1 (List.rev vts1) in
    let unify_result = unify_vts' unify_result rt2 (List.rev vts2) in

    let iter_to_list' e =
      match e.it with
      | Il.Ast.IterE (e', (List, xes)) ->
        (match get_cached_length e' with
        | None -> e'
        | Some l ->
          let es = List.init l (fun i ->
            List.fold_left (fun e (x, _) ->
              transform_expr (replace_id x.it (x.it ^ "." ^ string_of_int i)) e
            ) e xes) in
          let it = Il.Ast.ListE es in
          { e with it })
      | _ -> e
    in
    let iter_to_list = transform_expr iter_to_list' in
    let iter_to_list_prem = transform_prem iter_to_list' in

    (* Transform trule *)
    let trule = List.find (fun r ->
      let Il.Ast.RuleD (id, _, _, _, _) = r.it in
      String.uppercase_ascii id.it = case
    ) !trules in

    let trule = {trule with it =
      match trule.it with
      | Il.Ast.RuleD (id, binds, mixop, exp, prems) ->
        let exp' = exp |> iter_to_list |> apply_unify_result unify_result in
        let prems' = prems |> List.map iter_to_list_prem |> List.map (apply_unify_result_prem unify_result) in
        Il.Ast.RuleD (id, binds, mixop, exp', prems')
    } in

    let instr = rule_to_instr trule in
    let trule, instr = concretize_instr trule instr in

    (* print_endline (Il.Print.string_of_rule trule); *)
    (* print_endline (Il.Print.string_of_exp instr); *)

    sideconds := (rule_to_prems trule |> List.concat_map as_sidecond) @ !sideconds;

    instr :: acc, rt2
  ) ([], rt) cases rts |> fst |> List.rev

let gen_values rt: Il.Ast.exp list =
  List.map (fun t ->
    CaseE ("TODO: " ^ (Il.Print.string_of_t) t, TupE ())
  )

let wrap_as_func (instrs: Il.Ast.exp list) =
  ignore instrs;

  Al.Ast.CaseV ("FUNC", [])

let wrap_as_module (func: Al.Ast.value) =
  ignore func;

  Al.Ast.CaseV ("MODULE", [])


(* Generates the simplest module, which contains the instruction sequence with whose names are `cases` *)
let gen_test_containing_seq (cases: string list): Al.Ast.value =
  (* 0. Init *)
  Random.init !Flag.seed;
  trules := get_typing_rules ();
  arrow_map := !trules |> List.map rule_to_arrow;

  (* 1. Fix rt *)
  let rts = fix_rts cases in (* May throw, if this combination is impossible *)
  (* Print *)
  print_endline "===========";
  rts |> List.iter (fun rt ->
    rt |> List.iter (fun vt -> Il.Print.string_of_exp vt |> print_endline);
    print_endline "";
  );


  (* 2. Fix immediates *)
  let instrs = fix_immediate cases rts in (* May throw, if it is impossible to fill in immeidates *)
  print_endline "===========";
  instrs |> List.iter (fun i ->
    print_endline (Il.Print.string_of_exp i);
  );
  print_endline "===========";
  !sideconds |> List.iter (function
    | TypeLenC _ -> ()
    | IfPrC e -> print_endline ("-- " ^ Il.Print.string_of_exp e)
  );


  (* 3. Prepend values *)
  let values = gen_values (List.hd rts) in

  (* 4. Wrap as a function *)
  let func = wrap_as_func (values @ instrs) in

  (* 5. Wrap as a module *)
  wrap_as_module func

open Util
open Source

open Langs
(* open Valid *)

(* open Al.Ast *)
open Al.Al_util

(** Helpers **)
let replace_id old_id new_id e =
  let f e =
    match e.it with
    | Il.Ast.VarE id when id.it = old_id -> {e with it = Il.Ast.VarE {id with it = new_id}}
    | _ -> e
  in
  Il2al.Il_walk.transform_expr f e

let replace_id_using f e =
  let f e =
    match e.it with
    | Il.Ast.VarE id -> {e with it = Il.Ast.VarE {id with it = f (id.it)}}
    | _ -> e
  in
  Il2al.Il_walk.transform_expr f e

let replace_id_with old_id new_e e =
  let f e =
    match e.it with
    | Il.Ast.VarE id when id.it = old_id -> new_e
    | _ -> e
  in
  Il2al.Il_walk.transform_expr f e

let rec dedup eq = function
| [] -> []
| hd :: tl -> hd :: dedup eq (Lib.List.filter_not (eq hd) tl)

let to_phrase ty x = x $$ no_region % ty

type context = {
  typ: Il.Ast.typ;
}

let rec gen c x =
  let deftyp, _ = Gen.dispatch_deftyp x [] in
  match deftyp.it with
  | AliasT typ -> gen_typ c typ
  | StructT _ -> failwith "StructT not supported"
  | VariantT typcases ->
    let typcases = Lib.List.filter_not (Gen.has_subid_hint "sem") typcases in
    let typcase = Utils.choose typcases in
    gen_typcase c typcase
and gen_typcase c (mixop, (_binds, typs, _prems), _hint) =
  match mixop with
    (* Propagation *)
    | [[]; []] -> gen_typs c typs |> List.hd
    | _ ->
      let args = Il.Ast.TupE (gen_typs c typs) |> to_phrase c.typ in
      Il.Ast.CaseE (mixop, args) |> to_phrase c.typ
and gen_typs c typs =
  match typs.it with
  | TupT typs' -> List.map (gen_typ c) (List.map snd typs')
  | _ -> [ gen_typ c typs ]
and gen_typ c typ =
  match typ.it with
  | VarT (id, []) -> gen {typ} id.it
  | IterT (typ', Opt) ->
    if Random.bool() then Il.Ast.OptE None |> to_phrase typ
    else Il.Ast.OptE (Some (gen_typ c typ')) |> to_phrase typ
  | _ -> failwith ("TODO: unhandled type for gen_typ: " ^ Il.Print.string_of_typ typ)
let gen_typ typ = gen_typ {typ} typ

(** End of Helpers **)

type valtype = Il.Ast.exp
type restype = valtype list

let trules = ref []

let rule_to_arrow rule =
  let open Il.Ast in

  let rec unwrap e =
    match e.it with
    | CaseE ([[]; []], e')
    | TupE [e'] -> unwrap e'
    | _ -> e
  in

  let expected e kind =
    Printf.sprintf "Expected %s to be %s" (Il.Print.string_of_exp e) kind |> failwith
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
      | _ -> expected args "t1, t2 or t1, x*, t2"
      )
    | _ -> expected rhs "e1 -> e2"
    )
  | _ -> expected exp "C |- lhs : rhs"
let arrow_map = ref []

type sidecond =
  | TypeLen of int * Il.Ast.exp * int

let sideconds: sidecond list ref = ref []

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

let apply_unify_result result ess =
  List.fold_left (fun ess (x, e) ->
    List.map (List.map (replace_id_with x e)) ess
  ) ess result

let fix_free_var ess =
  let destruct_var e =
    match e.it with
    | Il.Ast.VarE x -> Some (x.it, e.note)
    | _ -> None
  in
  let free_vars = List.flatten ess |> List.filter_map destruct_var |> dedup (fun x y -> fst x = fst y) in
  List.fold_left (fun ess (x, typ) ->
    let e = gen_typ typ in
    List.map (List.map (replace_id_with x e)) ess
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

        let sidecond = TypeLen (i, e, l) in
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
            replace_id x.it (x.it ^ "." ^ string_of_int i) e
          ) e xes
        )
      | _ -> [rt]
    in

    let remove_sub e = match e.it with | Il.Ast.SubE (e, _, _) -> e | _ -> e in

    let append_idx = replace_id_using (fun x -> x ^ "@" ^ (string_of_int i)) in

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

    (List.rev (prefix @ vts2) :: rt :: rts) |> apply_unify_result unify_result
  ) [[]] cases
  |> fix_free_var

let fix_immediate (cases: string list) rts: Al.Ast.value list =
  let rt = List.hd rts in
  let rts = List.tl rts in

  List.fold_left2 (fun (acc, rt1) case rt2 ->
    ignore rt1;
    let trule = List.find (fun r -> let Il.Ast.RuleD (id, _, _, _, _) = r.it in String.uppercase_ascii id.it = case) !trules in
    let (lhs, rhs) = List.assoc case !arrow_map in
    print_endline (Il.Print.string_of_rule trule);
    print_endline (Il.Print.string_of_exp lhs);
    print_endline (Il.Print.string_of_exp rhs);
    nullary case :: acc, rt2
  ) ([], rt) cases rts |> fst

let gen_values rt: Al.Ast.value list = ignore rt; []

let wrap_as_func (instrs: Al.Ast.value list) =
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
  (*
  print_endline "===========";
  rts |> List.iter (fun rt ->
    rt |> List.iter (fun vt -> Il.Print.string_of_exp vt |> print_endline);
    print_endline "";
  );
  *)

  (* 2. Fix immediates *)
  let instrs = fix_immediate cases rts in (* May throw, if it is impossible to fill in immeidates *)
  print_endline "===========";
  instrs |> List.iter (fun i ->
    print_endline (Al.Print.string_of_value i);
  );

  (* 3. Prepend values *)
  let values = gen_values (List.hd rts) in

  (* 4. Wrap as a function *)
  let func = wrap_as_func (values @ instrs) in

  (* 5. Wrap as a module *)
  wrap_as_module func

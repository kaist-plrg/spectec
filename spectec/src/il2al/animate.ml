(*
This transformation
 1) reorders premises and
 2) explicitly denote a premise if it is an assignment.
by performing dataflow analysis.
*)

open Util
open Source
open Il.Ast
open Free


(* Helpers *)

let rec list_count' pred acc = function
| [] -> acc
| hd :: tl -> list_count' pred (acc + if pred hd then 1 else 0) tl
let list_count pred = list_count' pred 0

let not_ f x = not (f x)

let (--) xs ys = List.filter (fun x -> not (List.mem x ys)) xs

(* Helper for handling free-var set *)
let subset x y = Set.subset x.varid y.varid

type tag =
  | Condition
  | Assign of string list
(* type row = tag * prem * int list *)
let unwrap (_, p, _) = p
let targets (t, _, _) = match t with Assign targets -> targets | _ -> assert false

(* are all free variables in the premise known? *)
let is_tight env (tag, prem, _) =
  match tag with
  | Condition -> subset (free_prem false prem) env
  | _ -> false

(* are all free variables except to-be-assigned known? *)
let is_assign env (tag, prem, _) =
  match tag with
  | Assign frees -> subset (diff (free_prem false prem) {empty with varid = (Set.of_list frees)}) env
  | _ -> false

(* is this assign premise encoded premise for pop? *)
let is_pop env row =
  is_assign env row &&
  match (unwrap row).it with
  | LetPr (_, rhs, _) -> Il.Print.string_of_typ rhs.note = "stackT"
  | _ -> false

(* iteratively select pop, condition and assignment premises,
 * effectively sorting the premises as a result. *)
let rec select_pops prems acc env fb =
  match prems with
  | [] -> Some acc
  | _ ->
    let (pops, non_pops) = List.partition (is_pop env) prems in
    match pops with
    | [] ->
      select_tight prems acc env fb
    | _ ->
      let pops' = List.map unwrap pops in
      let new_env = pops
        |> List.map targets
        |> List.concat
        |> List.fold_left (fun env x ->
          union env { empty with varid = Set.singleton x }
        ) env
      in
      select_pops non_pops (acc @ pops') new_env fb

and select_tight prems acc env fb =
  match prems with
  | [] -> Some acc
  | _ ->
    let (tights, non_tights) = List.partition (is_tight env) prems in
    select_assign non_tights (acc @ List.map unwrap tights) env fb

and select_assign prems acc env fb =
  match prems with
  | [] -> Some acc
  | _ ->
    let (pops, non_pops) = List.partition (is_pop env) prems in
    let (assigns, non_assigns) =
      if pops = [] then
        List.partition (is_assign env) prems
      else
        pops, non_pops
    in
    match assigns with
    | [] ->
      let len = List.length acc in
      if len > fst !fb then
        fb := (len, acc @ List.map unwrap non_assigns);
      None
    | _ ->
      let assigns' = List.map unwrap assigns in
      let new_env = assigns
        |> List.map targets
        |> List.concat
        |> List.fold_left (fun env x ->
          union env { empty with varid = Set.singleton x }
        ) env
      in
      select_tight non_assigns (acc @ assigns') new_env fb

let select_target_col rows cols =
  let count col = list_count (fun (_, _, coverings) -> List.exists ((=) col) coverings) rows in
  List.fold_left (fun (cand, cnt) c ->
    let cnt' = count c in
    if cnt' < cnt then (c, cnt') else (cand, cnt)
  ) (-1, List.length rows + 1) cols |> fst

let covers (_, _, coverings) col = List.exists ((=) col) coverings
let disjoint_row (_, _, coverings1) (_, _, coverings2) = List.for_all (fun c -> List.for_all ((<>) c) coverings1) coverings2

(* Saves best attempt of knuth, to recover from knuth failure.
 * Can be removed when knuth is guaranteeded to be succeed. *)
let best = ref (0, [])

let rec knuth rows cols selected_rows = match cols with
  | [] -> [ selected_rows ]
  | _ ->
    if fst !best > List.length cols then (
      let is_condition (tag, _, _) = match tag with | Condition -> true | _ -> false in
      best:= (List.length cols, selected_rows @ List.filter is_condition rows) );
    let target_col = select_target_col rows cols in
    List.concat_map (fun r ->
      if not (covers r target_col) then [] else
        let new_rows = List.filter (disjoint_row r) rows in
        let new_cols = List.filter (not_ (covers r)) cols in
        knuth new_rows new_cols (r :: selected_rows)
    ) rows

let rec index_of acc xs x = match xs with
  | [] -> None
  | h :: t -> if h = x then Some acc else index_of (acc + 1) t x

let free_exp_list e = (free_exp false e).varid |> Set.elements
let free_arg_list e = (free_arg false e).varid |> Set.elements

let rec powset = function
| [] -> [ [] ]
| hd :: tl -> List.concat_map (fun pow -> [ hd :: pow ; pow ]) (powset tl)

let wrap x = [x]

let singletons = List.map wrap

let group_arg e _ =
  match e.it with
  | CallE (_, args) -> List.map free_arg_list args
  | _ -> assert false

let large_enough_subsets xs =
  let yss = powset xs in
  let n = List.length xs in
  (* Assumption: Allowing one variable to be known *)
  (* TODO: Increase this limit by reducing execution time *)
  let min = if n >= 2 then n-1 else n in
  List.filter ( fun ys -> min <= List.length ys ) yss

let (@@) f g x = f x @ g x

let is_not_lhs e = match e.it with
| LenE _ | IterE (_, (ListN (_, Some _), _)) | DotE _ -> true
| _ -> false

(* Hack to handle RETURN_CALL_ADDR, eventually should be removed *)
let is_atomic_lhs e = match e.it with
| CaseE ([{it = Atom "FUNC"; _}]::_, { it = CaseE ([[]; [{it = Arrow; _}]; []], { it = TupE [ { it = IterE (_, (ListN _, _)); _} ; { it = IterE (_, (ListN _, _)); _} ] ; _} ); _ }) -> true
| _ -> false

(* Hack to handle ARRAY.INIT_DATA, eventually should be removed *)
let is_call e = match e.it with
| CallE _ -> true
| _ -> false

let subset_selector e =
  if is_not_lhs e then (fun _ -> [])
  else if is_call e then singletons @@ group_arg e
  else if is_atomic_lhs e then wrap
  else large_enough_subsets

let rows_of_eq vars len i l r at =
  (free_exp_list l -- free_exp_list r)
  |> subset_selector l
  |> List.filter_map (fun frees ->
    let covering_vars = List.filter_map (index_of len vars) frees in
    if List.length frees = List.length covering_vars then (
      Some (Assign frees, LetPr (l, r, frees) $ at, [i] @ covering_vars) )
    else
      None
  )

let rec rows_of_prem vars len i p =
  match p.it with
  | IfPr e ->
    (match e.it with
      | CmpE (EqOp, l, r) ->
        [ Condition, p, [i] ]
        @ rows_of_eq vars len i l r p.at
        @ rows_of_eq vars len i r l p.at
      | MemE (l, r) ->
        [ Condition, p, [i] ]
        @ rows_of_eq vars len i l { r with it = TheE r } p.at
      | _ -> [ Condition, p, [i] ]
    )
  | LetPr (_, _, targets) ->
    let covering_vars = List.filter_map (index_of len vars) targets in
    [ Assign targets, p, [i] @ covering_vars ]
  | RulePr (_, _, { it = TupE args; _ }) ->
    (* Assumpton: the only possible assigned-value is the last arg (i.e. ... |- lhs ) *)
    let _, l = Util.Lib.List.split_last args in
    let frees = (free_exp_list l) in
    [
      Condition, p, [i];
      Assign frees, p, [i] @ List.filter_map (index_of len vars) (free_exp_list l)
    ]
  | IterPr (p', iter) ->
    let to_iter (tag, p', coverings) = tag, IterPr (p', iter) $ p.at, coverings in
    List.map to_iter (rows_of_prem vars len i p')
  | _ -> [ Condition, p, [i] ]

let build_matrix prems known_vars =
  let all_vars = prems |> List.map (free_prem false) |> List.fold_left union empty in
  let unknown_vars = (diff all_vars known_vars).varid |> Set.elements in
  let len_prem = List.length prems in

  let rows = List.mapi (rows_of_prem unknown_vars len_prem) prems |> List.concat in
  let cols = List.init (len_prem + List.length unknown_vars) (fun i -> i) in
  rows, cols


(* Animate the list of premises *)

let animate_prems known_vars prems =
  (* Set --otherwise prem to be the first prem (if any) *)
  let is_other = function {it = ElsePr; _} -> true | _ -> false in
  let (other, non_other) = List.partition is_other prems in
  let rows, cols = build_matrix non_other known_vars in

  (* 1. Run knuth *)
  best := (List.length cols + 1, []);
  let (candidates, k_fail) =
    match knuth rows cols [] with
    | [] -> [ snd !best ], true
    | xs -> List.map List.rev xs, false
  in

  (* 2. Reorder *)
  let best' = ref (-1, []) in
  match List.find_map (fun cand -> select_pops cand other known_vars best') candidates with
  | None ->
    if (not k_fail) then
      let unhandled_prems = Lib.List.drop (fst !best') (snd !best') in
      Error.error (over_region (List.map at unhandled_prems)) "prose translation" "There might be a cyclic binding"
    else
      snd !best'
  | Some x -> x

(* Animate rule *)
let animate_rule r = match r.it with
  | RuleD(id, _ , _, _, _) when id.it = "pure" || id.it = "read" -> r (* TODO: remove this line *)
  | RuleD(id, binds, mixop, args, prems) -> (
    match (mixop, args.it) with
    (* lhs ~> rhs *)
    | ([ [] ; [{it = SqArrow; _}] ; []] , TupE ([_lhs; _rhs])) ->
      let new_prems = animate_prems {empty with varid = Set.of_list Encode.input_vars} prems in
      RuleD(id, binds, mixop, args, new_prems) $ r.at
    | _ -> r
  )

(* Animate clause *)
let animate_clause c = match c.it with
  | DefD (binds, args, e, prems) ->
    let new_prems = animate_prems (free_list (free_arg false) args) prems in
    DefD (binds, args, e, new_prems) $ c.at

(* Animate defs *)
let rec animate_def d = match d.it with
  | RelD (id, mixop, t, rules) ->
    let rules' = List.map animate_rule rules in
    RelD (id, mixop, t, rules') $ d.at
  | DecD (id, t1, t2, clauses) ->
    let new_clauses = List.map animate_clause clauses in
    DecD (id, t1, t2, new_clauses) $ d.at
  | RecD ds -> RecD (List.map animate_def ds) $ d.at
  | TypD _ | GramD _ | HintD _ -> d

(* Main entry *)
let transform (defs : script) =
  List.map animate_def defs

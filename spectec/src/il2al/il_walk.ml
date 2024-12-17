open Util.Source
open Il.Ast

(* Walker-based transformer *)
let transform_id f x =
  let e = VarE x $$ no_region % (TupT [] $ no_region) in
  match (f e).it with
  | VarE x' -> x'
  | _ -> x

let rec transform_exp f e =
  let new_ = transform_exp f in
  let it =
    match e.it with
    | VarE _
    | BoolE _
    | NatE _
    | TextE _ -> e.it
    | UnE (op, e1) -> UnE (op, new_ e1)
    | BinE (op, e1, e2) -> BinE (op, new_ e1, new_ e2)
    | CmpE (op, e1, e2) -> CmpE (op, new_ e1, new_ e2)
    | IdxE (e1, e2) -> IdxE (new_ e1, new_ e2)
    | SliceE (e1, e2, e3) -> SliceE (new_ e1, new_ e2, new_ e3)
    | UpdE (e1, p, e2) -> UpdE (new_ e1, p, new_ e2)
    | ExtE (e1, p, e2) -> ExtE (new_ e1, p, new_ e2)
    | StrE efs -> StrE efs (* TODO efs *)
    | DotE (e1, atom) -> DotE (new_ e1, atom)
    | CompE (e1, e2) -> CompE (new_ e1, new_ e2)
    | LenE e1 -> LenE (new_ e1)
    | TupE es -> TupE ((List.map new_) es)
    | CallE (id, as1) -> CallE (id, List.map (transform_arg f) as1)
    | IterE (e1, iterexp) -> IterE (new_ e1, transform_iterexp f iterexp)
    | ProjE (e1, i) -> ProjE (new_ e1, i)
    | UncaseE (e1, op) -> UncaseE (new_ e1, op)
    | OptE eo -> OptE ((Option.map new_) eo)
    | TheE e1 -> TheE (new_ e1)
    | ListE es -> ListE ((List.map new_) es)
    | CatE (e1, e2) -> CatE (new_ e1, new_ e2)
    | MemE (e1, e2) -> MemE (new_ e1, new_ e2)
    | CaseE (mixop, e1) -> CaseE (mixop, new_ e1)
    | SubE (e1, _t1, t2) -> SubE (new_ e1, _t1, t2)
  in
  let note = transform_typ f e.note in
  f { e with it; note }

and transform_arg f a =
  { a with it = match a.it with
    | ExpA e -> ExpA (transform_exp f e)
    | TypA t -> TypA t
    | DefA id -> DefA id
    | GramA id -> GramA id }

and transform_iterexp f (iter, xes) =
  let xs, es = List.split xes in
  let xs' = List.map (transform_id f) xs in
  let es' = List.map (transform_exp f) es in
  (iter, List.combine xs' es')

and transform_typ f t =
  { t with it = match t.it with
    | VarT (id, args) -> VarT (id, List.map (transform_arg f) args)
    | TupT ets -> TupT (ets |> List.map (fun (e, t) -> e, transform_typ f t))
    | IterT (t, iter) -> IterT (transform_typ f t, iter) (* TODO: iter *)
    | t' -> t' }

let rec transform_prem f p =
  { p with it = match p.it with
    | RulePr (id, mixop, e) -> RulePr (id, mixop, transform_exp f e)
    | IfPr e -> IfPr (transform_exp f e)
    | LetPr (e1, e2, xs) -> LetPr (transform_exp f e1, transform_exp f e2, xs)
    | ElsePr -> ElsePr
    | IterPr (p, iterexp) -> IterPr (transform_prem f p, transform_iterexp f iterexp) }

let transform_rule f r =
  { r with it = match r.it with
    | RuleD (id, binds, mixop, e, ps) -> RuleD (id, binds, mixop, transform_exp f e, List.map (transform_prem f) ps)
  }

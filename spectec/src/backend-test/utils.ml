let choose l =
  let n = List.length l in
  assert (n > 0);
  let i = Random.int n in
  List.nth l i

let contains x = List.exists (fun x' -> x = x')

(* TODO: move this to al/walk.ml *)
open Al.Ast

let rec walk_value f v =
  let new_ = walk_value f in
  match f v with
  | NumV _
  | TextV _
  | FrameV _
  | LabelV _
  | StoreV _ -> v
  | ListV a -> ListV (ref (Array.map new_ !a))
  | StrV r -> StrV (Util.Record.Record.map new_ r)
  | CaseV (c, vl) -> CaseV (c, List.map new_ vl)
  | OptV v_opt ->  OptV (Option.map new_ v_opt)
  | TupV vl -> TupV (List.map new_ vl)
  | ArrowV (v1, v2) -> ArrowV (new_ v1, new_ v2)

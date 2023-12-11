let choosei l =
  let n = List.length l in
  assert (n > 0);
  let i = Random.int n in
  i, List.nth l i
let choose l = choosei l |> snd

let contains x = List.exists (fun x' -> x = x')

let find_index_all f l =
  let fi i x = if f x then Some i else None in
  l |> List.mapi fi |> List.filter_map (fun o -> o)

let groupi_by f xs =
  let ixs = List.mapi (fun i x -> (i, x)) xs in
  List.fold_left (fun groups (i, x) ->
    let tag = f x in
    let rec new_ groups = match groups with
    | [] -> [ tag, [ i ] ]
    | (tag', is) :: gs when tag = tag' -> (tag, i :: is) :: gs
    | g :: gs -> g :: new_ gs in
    new_ groups
  ) [] ixs


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

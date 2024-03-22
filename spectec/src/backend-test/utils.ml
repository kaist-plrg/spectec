let choosei l =
  let n = List.length l in
  assert (n > 0);
  let i = Random.int n in
  i, List.nth l i
let choose l = choosei l |> snd
let (//) x y = choose [x; y]

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


(* Interesting values *)
module IntSet = Set.Make (Z)
module Interesting = Map.Make (String)
let interesting_value_map =

  (* generating interesting values given number of bits *)
  let gen_interesting_integers num_bits =
    assert (num_bits > 0);

    let rec aux num_bits acc =
      if num_bits = 0 then acc
      else if num_bits > 0 then
        (* 2^(n-1) *)
        let v = Z.shift_left Z.one (num_bits-1) in
        (* 2^(n-1), 2^(n-1)+1, 2^n-1 *)
        [ v; Z.succ v; Z.pred (Z.shift_left v 1) ]
        |> IntSet.of_list
        |> IntSet.union acc
        |> aux (num_bits-1)
      else assert false
    in
    let interesting_values = aux num_bits IntSet.empty in

    interesting_values
    (* Add negative interesting integers *)
    |> IntSet.map Z.neg
    (* Add min integer *)
    |> IntSet.add (Z.shift_left Z.minus_one num_bits)
    |> IntSet.union interesting_values
  in

  let bit_widths = [ 8; 16; 32; 64 ] in
  List.fold_right
    (fun i -> Interesting.add ("I" ^ string_of_int i) (gen_interesting_integers (i-1)))
    bit_widths
    Interesting.empty

let print_interesting_integers () =
  Interesting.iter (fun k s ->
    print_endline k;
    IntSet.iter (fun z ->
      print_string (Z.to_string z ^ " ")
    ) s;
    print_endline "";
    print_endline "";
  ) interesting_value_map

let append_byte bs b = Z.(logor (shift_left bs 8) b)
let gen_byte _ = Random.int 256 |> Z.of_int
let gen_bytes n = List.init n gen_byte |> List.fold_left append_byte Z.zero

(* TODO: move this to al/walk.ml *)
open Al.Ast

let rec walk_value f v =
  let new_ = walk_value f in
  match f v with
  | NumV _
  | BoolV _
  | TextV _
  | FrameV _
  | LabelV _ -> v
  | ListV a -> ListV (ref (Array.map new_ !a))
  | StrV r -> StrV (Util.Record.map Fun.id new_ r)
  | CaseV (c, vl) -> CaseV (c, List.map new_ vl)
  | OptV v_opt ->  OptV (Option.map new_ v_opt)
  | TupV vl -> TupV (List.map new_ vl)

let copy_value = walk_value (fun v -> v)

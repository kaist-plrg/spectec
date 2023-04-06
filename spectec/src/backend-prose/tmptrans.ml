open Util.Source
open El

let translate_expr e = match e.it with
  | Ast.VarE s -> Ir.NameE (N s.it)
  | Ast.ParenE ({ it = SeqE [{it=AtomE (Atom "\"CONST\"");_}]; _ }, false) -> Ir.YetE ""
  | _ -> Print.structured_string_of_exp e |> failwith

(* 1. Handle lhs of reduction rules *)

let hds l = l |> List.rev |> List.tl

let assert_type e res =
  match e.it with
  (* ({CONST I32 c}) *)
  | Ast.ParenE({it = Ast.SeqE({it = (Ast.AtomE (Atom "CONST")); _} :: {it = (Ast.AtomE (Atom "I32")); _}  :: _); _}, _) ->
      res := !res @ [Ir.AssertI "Due to validation, a value of value type i32 is on the top of the stack"]
  | _ ->
      res := !res @ [Ir.AssertI "Due to validation, a value is on the top of the stack"]

let pop left res = match left.it with
  | Ast.SeqE(es) -> hds es |> List.iter (fun e ->
    assert_type e res;
    res := !res @ [PopI (Some (translate_expr e))]
  )
  | Ast.ParenE({it = Ast.SeqE({it = Ast.AtomE(Atom "LABEL_"); _} :: _); at = _}, _) ->
    res := !res @ [YetI "Bubble-up semantics."]
  | _ -> ()

let string_of_destructed (left, right, prems) =
  let filter_nl xs = List.filter_map (function Ast.Nl -> None | Ast.Elem x -> Some x) xs in
  let map_nl_list f xs = List.map f (filter_nl xs) in
  Print.string_of_exp left ^
  " ~> " ^
  Print.string_of_exp right ^
  String.concat "" (map_nl_list (fun x -> "\n    -- " ^ Print.string_of_premise x) prems)

let handle_reduction_group red_group acc =
  (* assert: every redunction rule in red_group has same lhs *)
  let name =
    List.fold_left
      (fun inner_acc red -> inner_acc ^ string_of_destructed red ^ "\n")
      ""
      red_group
  in

  (* DEBUG *)
  print_endline name;

  let res = ref [] in

  let (left, _, _) = List.hd red_group in
  let left = match left.it with
    | Ast.InfixE(_, Semicolon, left) -> left
    | _ -> left
  in
  pop left res;

  Ir.Program (name, !res) :: acc

(* if r is a reduction rule, desturct it into triplet of (lhs, rhs, premises) *)
let destruct_as_rule r = match r.it with
  | Ast.RuleD(name, _, e, prems) -> (match e.it with
    | Ast.InfixE(left, SqArrow, right) ->
      if String.starts_with ~prefix:"Step_" name.it then
        Some (left, right, prems)
      else
        None
    | _ -> None)
  | _ -> None

let rec group_by f = function
  | [] -> []
  | [x] -> [[x]]
  | hd :: tl ->
    let pred x = (f hd  = f x) in
    let (l, r) = List.partition pred tl in
    (hd :: l) :: (group_by f r)

let translate el =
  (* Filter and destruct redctions only *)
  let reductions = el |> List.filter_map destruct_as_rule in

  (* Group reductions by lefthand side *)
  let reduction_groups = group_by (fun (left, _, _) ->
    Print.string_of_exp left
  ) reductions in

  (* Handle each redction group *)
  List.fold_right handle_reduction_group reduction_groups []

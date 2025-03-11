open Reference_interpreter
open Source
open Types
open Ast

let debug = true

let spectec_home = Sys.getenv "SPECTEC_HOME"
let test_stf_home = Printf.sprintf "%s/%s" spectec_home "spectec/test-stf"
let test_case_dir = Printf.sprintf "%s/%s" test_stf_home "wasm"

module List = struct include List

  let find_index_opt (f: 'a -> bool) (l: 'a list) : int option =
    let rec find_index_opt n = function
      | h :: t -> if f h then Some n else find_index_opt (n+1) t
      | [] -> None in
    find_index_opt 0 l

  let find_index
    ?(stringifier : ('a -> string) option)
    (f: 'a -> bool)
    (l: 'a list)
    : int =
      match find_index_opt f l with
      | Some i -> i
      | None ->
        match stringifier with
        | None -> failwith "Not in the list"
        | Some f ->
          l
          |> List.map f
          |> String.concat ", "
          |> Printf.sprintf "Not in the list: [%s]"
          |> failwith
end

let (<|) f x = f x

(* Make Ast *)

let mk_name (s: string) : name = Utf8.decode s

let mk_idx (i: int) : idx = Int32.of_int i @@ no_region
let mk_i32 (i: int) : num = Value.I32 (Int32.of_int i) @@ no_region

let rec_type_of_def_type (st: str_type) : rec_type = RecT [ SubT (Final, [], st) ]
let rec_type_of_func_type (ft: func_type) : rec_type =
  rec_type_of_def_type <| DefFuncT ft
let rec_type_of_array_type (at: array_type) : rec_type =
  rec_type_of_def_type <| DefArrayT at

  
let instr_of' (ctx: str_type list) (rt: ref_type) : instr' list =
  let str_type_list_find_index = List.find_index ~stringifier:string_of_str_type in

  match rt with
  | Null, ht -> [ RefNull ht ]
  | NoNull, ht ->

    let i31_ref = [ Const (mk_i32 42); RefI31 ] in
    let mk_struct_ref (i: int) =
      [ Const (mk_i32 42); StructNew (mk_idx i, Implicit) ] in
    let mk_array_ref (i: int) =
      [ Const (mk_i32 42); ArrayNew (mk_idx i, Implicit) ] in
    let mk_func_ref (i: int) = [ RefFunc (mk_idx i) ] in

    match ht with
    | AnyHT | EqHT ->
      (match List.nth_opt ctx 0 with
      | None -> i31_ref
      | Some st ->
        match st with
        | DefStructT _ -> mk_struct_ref 0
        | DefArrayT _ -> mk_array_ref 0
        | DefFuncT _ -> mk_func_ref 0)
    | I31HT -> i31_ref
    | StructHT ->
      str_type_list_find_index (function DefStructT _ -> true | _ -> false) ctx
      |> mk_struct_ref
    | ArrayHT ->
      str_type_list_find_index (function DefArrayT _ -> true | _ -> false) ctx
      |> mk_array_ref
    | FuncHT ->
      str_type_list_find_index (function DefFuncT _ -> true | _ -> false) ctx
      |> mk_func_ref
    | ExnHT -> failwith "TODO"
    | DefHT dt ->
      let st = expand_def_type dt in
      let i =
        st
        |> (=)
        |> Fun.flip str_type_list_find_index ctx in
      (match st with
      | DefStructT _ -> mk_struct_ref i
      | DefArrayT _ -> mk_array_ref i
      | DefFuncT _ -> mk_func_ref i
      )
    | ExternHT | NoExternHT ->
      failwith "Instruction for extern reference is not supported"
    | NoneHT | NoFuncHT | NoExnHT | BotHT ->
      rt
      |> string_of_ref_type
      |> Printf.sprintf "No possible instruction for reference type %s"
      |> failwith
    | VarHT _ ->
      rt
      |> string_of_ref_type
      |> Printf.sprintf "No possible instruction for administrative reference type %s"
      |> failwith
let instr_of (types: type_ list) (rt: ref_type) : instr list =
  let ctx =
    types
    |> List.map it
    |> List.concat_map (roll_def_types Int32.zero)
    |> List.map expand_def_type in

  rt
  |> instr_of' ctx
  |> List.map (fun i -> i @@ no_region)

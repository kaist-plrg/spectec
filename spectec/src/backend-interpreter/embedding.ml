open Reference_interpreter
open Al
open Al_util


(* Helper for handling json *)

type json = Yojson.Safe.t

let json2int: json -> int = function
  | `Int i -> i
  | `Float f -> int_of_float f
  | json ->
    json
    |> Yojson.Safe.show
    |> Printf.sprintf "json2int: Not a integer: %s"
    |> failwith

let json2string: json -> string = function
  | `String s -> s
  | json ->
    json
    |> Yojson.Safe.show
    |> Printf.sprintf "json2string: Not a string: %s"
    |> failwith

let json2list (f: json -> 'a) (json: json): 'a list =
  match json with
  | `List l -> List.map f l
  | json ->
    json
    |> Yojson.Safe.show
    |> Printf.sprintf "json2list: Not a list: %s"
    |> failwith

let rec al2json: Ast.value -> json = function
  | CaseV (name, args) ->
    let fields = List.mapi (fun i e -> "_" ^ string_of_int i, al2json e) args in
    `Assoc (("constructor", `String name) :: fields)
  | OptV opt ->
    let jsons = opt |> Option.to_list |> List.map al2json in
    `List jsons
  | ListV vs ->
    let jsons = !vs |> Array.to_list |> List.map al2json in
    `List jsons
  | v -> failwith ("todo: " ^ (Print.structured_string_of_value v))

let rec json2al: json -> Ast.value = function
  | `List l -> l |> List.map json2al |> listV_of_list
  | `Int i -> intV (Z.of_int i)
  | `Float f -> intV (Z.of_float f)
  | `Assoc fields when List.mem_assoc "constructor" fields ->
    let variant_name = List.assoc "constructor" fields in
    let args =
      List.init
        (List.length fields - 1)
        (fun i ->
          fields
          |> List.assoc ("_" ^ string_of_int i)
          |> json2al
        ) in
    caseV (json2string variant_name, args)
  | _ -> textV "yet"

let parse_input input =
  let json = Yojson.Safe.from_string input in
  match json with
  | `Assoc fields ->
    (match List.assoc "name" fields, List.assoc "args" fields with
    | `String name, `List args -> name, args
    | _ -> "", []
    )
  | _ -> "", []


(* Embedding functions *)

type embedding_function = json list -> json

let nullary constructor = caseV (constructor, [])
let embedding_error = nullary "error"

let module_decode: embedding_function = function
  | [ json ] ->
    let int_list = json2list json2int json in
    let len = List.length int_list in
    let b = Bytes.create len in
    let rec fill i = function
      | [] -> ()
      | x :: xs ->
          if x < 0 || x > 255 then
            invalid_arg "int_list_to_bytes: integer out of byte range";
          Bytes.set b i (Char.chr x);
          fill (i + 1) xs
    in
    fill 0 int_list;
    let bytes = Bytes.to_string b in

    let result =
      try
        bytes |> Decode.decode "" |> Construct.al_of_module
      with Decode.Code _ -> embedding_error in
    al2json result
  | args ->
    args
    |> List.map Yojson.Safe.show
    |> String.concat ", "
    |> Printf.sprintf "module_decode: wrong arity %s"
    |> failwith

let module_validate: embedding_function = function
  | [ json ] ->
    let al = json2al json in
    let result =
      try
        let module_ = Construct.al_to_module al in
        Reference_interpreter.Valid.check_module module_ |> ignore;
        nullary ""
      with Reference_interpreter.Valid.Invalid _ -> embedding_error
      in
    al2json result
  | args ->
    args
    |> List.map Yojson.Safe.show
    |> String.concat ", "
    |> Printf.sprintf "module_validate: wrong arity %s"
    |> failwith

module EmbeddingFuncMap = Map.Make (String)
let embedding_func_map =
  EmbeddingFuncMap.empty
  |> EmbeddingFuncMap.add "module_decode" module_decode
  |> EmbeddingFuncMap.add "module_validate" module_validate

let mem name = EmbeddingFuncMap.mem name embedding_func_map

let call_func name args =
  let func = EmbeddingFuncMap.find name embedding_func_map in
  func args

let run input =
  let funcname, args = parse_input input in
  (* print_endline (funcname ^ "(" ^ String.concat ", " (List.map Print.string_of_value args) ^ ")"); *)

  if EmbeddingFuncMap.mem funcname embedding_func_map then
    Ds.WasmContext.init_context ();
    let embedding_func = EmbeddingFuncMap.find funcname embedding_func_map in
    embedding_func args
    |> Yojson.Safe.to_string
    |> print_endline

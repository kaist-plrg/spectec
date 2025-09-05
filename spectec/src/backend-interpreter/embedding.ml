open Reference_interpreter
open Al
open Al_util

let rec al2field: Ast.value -> string * Yojson.Safe.t = function
  | CaseV (name, [json]) -> name, al2json json
  | v -> failwith ("todo: " ^ (Print.structured_string_of_value v))

and al2json: Ast.value -> Yojson.Safe.t = function
  | CaseV ("_RESULT", [json]) -> al2json json
  | CaseV ("MODULE", args) ->
    (* Assume Wasm 3.0 *)
    let module_fields =
      [ "types"; "imports"; "tags"; "globals"; "mems"; "tables";
        "funcs"; "datas"; "elems"; "start"; "exports" ] in
    let fields = List.combine module_fields (List.map al2json args) in
    `Assoc fields
  | CaseV ("", []) -> `List []
  | OptV opt ->
    let jsons = opt |> Option.to_list |> List.map al2json in
    `List jsons
  | ListV vs ->
    let jsons = !vs |> Array.to_list |> List.map al2json in
    `List jsons
  | v -> failwith ("todo: " ^ (Print.structured_string_of_value v))

let rec json2al: Yojson.Safe.t -> Ast.value = function
  | `List l -> l |> List.map json2al |> listV_of_list
  | `Int i -> intV (Z.of_int i)
  | `Float f -> intV (Z.of_float f)
  | `Assoc [] ->
    (* TODO: hard coding for error object *)
    caseV ("ERROR", [])
  | `Assoc fields ->
    (* TODO: hard coding for module object *)
    let types_ = List.find (fun (s, _) -> s = "types") fields |> snd |> json2al in
    let funcs = List.find (fun (s, _) -> s = "funcs") fields |> snd |> json2al in
    let tables = List.find (fun (s, _) -> s = "tables") fields |> snd |> json2al in
    let mems = List.find (fun (s, _) -> s = "mems") fields |> snd |> json2al in
    let globals = List.find (fun (s, _) -> s = "globals") fields |> snd |> json2al in
    let tags = List.find (fun (s, _) -> s = "tags") fields |> snd |> json2al in
    let elems = List.find (fun (s, _) -> s = "elems") fields |> snd |> json2al in
    let datas = List.find (fun (s, _) -> s = "datas") fields |> snd |> json2al in
    let start = List.find (fun (s, _) -> s = "start") fields |> snd |> json2al |> listv_to_optv in
    let imports = List.find (fun (s, _) -> s = "imports") fields |> snd |> json2al in
    let exports = List.find (fun (s, _) -> s = "exports") fields |> snd |> json2al in
    caseV ("MODULE", [ types_; imports; tags; globals; mems; tables; funcs; datas; elems; start; exports ])
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

let json2int: Yojson.Safe.t -> int = function
  | `Int i -> i
  | `Float f -> int_of_float f
  | json ->
    json
    |> Yojson.Safe.show
    |> Printf.sprintf "json2int: Not a integer: %s"
    |> failwith

let json2list (f: Yojson.Safe.t -> 'a) (json: Yojson.Safe.t): 'a list =
  match json with
  | `List l -> List.map f l
  | json ->
    json
    |> Yojson.Safe.show
    |> Printf.sprintf "json2list: Not a list: %s"
    |> failwith

type embedding_function = Yojson.Safe.t list -> Yojson.Safe.t

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
      with Decode.Code _ -> nullary "ERROR" in
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
      with Reference_interpreter.Valid.Invalid _ -> nullary "ERROR"
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

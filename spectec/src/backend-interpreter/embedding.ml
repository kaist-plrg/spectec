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
    let constructor = String.lowercase_ascii name in
    `Assoc (("constructor", `String constructor) :: fields)
  | OptV opt ->
    let jsons = opt |> Option.to_list |> List.map al2json in
    `List jsons
  | ListV vs ->
    let jsons = !vs |> Array.to_list |> List.map al2json in
    `List jsons
  | StrV fields ->
    let jsons =
      fields
      |> Util.Record.to_list
      |> List.map (fun (k, v) -> k, al2json v) in
    `Assoc jsons
  | TextV s -> `String s
  | NumV ((`Int z | `Nat z)) -> `Intlit (Z.to_string z)
  | v -> failwith ("todo: " ^ (Print.structured_string_of_value v))

let rec json2al: json -> Ast.value = function
  | `List l -> l |> List.map json2al |> listV_of_list
  | `Int i -> natV (Z.of_int i)
  | `Float f -> intV (Z.of_float f)
  | `Assoc fields when List.mem_assoc "constructor" fields ->
    let variant_name =
      fields
      |> List.assoc "constructor"
      |> json2string
      |> String.uppercase_ascii in
    let args =
      List.init
        (List.length fields - 1)
        (fun i ->
          fields
          |> List.assoc ("_" ^ string_of_int i)
          |> json2al
        ) in
    caseV (variant_name, args)
  | `Assoc fields ->
    fields
    |> List.map (fun (k, v) -> k, json2al v)
    |> Util.Record.of_list
    |> strV
  | `String s -> textV s
  | `Intlit s -> natV (Z.of_string s)
  | json ->
    json
    |> Yojson.Safe.show
    |> failwith

let parse_input input =
  let json = Yojson.Safe.from_string input in
  match json with
  | `Assoc fields ->
    (match List.assoc "name" fields, List.assoc "args" fields with
    | `String name, `List args -> name, args
    | _ -> failwith "todo"
    )
  | _ -> failwith "todo"


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
      let module_ = Construct.al_to_module al in
      try
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

let module_imports: embedding_function = function
  | [ json ] ->
    let al = json2al json in
    (* Module must be valid *)
    let module_ = Construct.al_to_module al in
    let ModuleT (importtypes, _) = Reference_interpreter.Valid.check_module module_ in
    importtypes
    |> List.map Construct.al_of_importtype
    |> List.map args_of_casev
    |> List.map (fun args -> caseV ("", args))
    |> listV_of_list
    |> al2json
  | args ->
    args
    |> List.map Yojson.Safe.show
    |> String.concat ", "
    |> Printf.sprintf "module_imports: wrong arity %s"
    |> failwith

let module_exports: embedding_function = function
  | [ json ] ->
    let al = json2al json in
    (* Module must be valid *)
    let module_ = Construct.al_to_module al in
    let ModuleT (_, exporttypes) = Reference_interpreter.Valid.check_module module_ in
    exporttypes
    |> List.map Construct.al_of_exporttype
    |> List.map args_of_casev
    |> List.map (fun args -> caseV ("", args))
    |> listV_of_list
    |> al2json
  | args ->
    args
    |> List.map Yojson.Safe.show
    |> String.concat ", "
    |> Printf.sprintf "module_exports: wrong arity %s"
    |> failwith

let module_instantiate: embedding_function = function
  | [ store_json; module_json; externaddrs_json ] ->
    let store = json2al store_json in
    let module_ = json2al module_json in
    let externaddrs = json2al externaddrs_json in

    Ds.Store.set store;
    let result =
      try
        Interpreter.instantiate [ module_; externaddrs ]
      with _ -> embedding_error in
    al2json (caseV ("", [ Ds.Store.get (); result ]))
  | args ->
    args
    |> List.map Yojson.Safe.show
    |> String.concat ", "
    |> Printf.sprintf "module_instantiate: wrong arity %s"
    |> failwith

let global_alloc: embedding_function = function
  | [ store_json; globaltype_json; val_json ] ->
    let store = json2al store_json in
    let globaltype = json2al globaltype_json in
    let val_ = json2al val_json in

    Ds.Store.set store;
    let globaladdr =
      match
        Interpreter.call_func "allocglobal" [ globaltype; val_ ]
      with
      | Some globaladdr -> globaladdr
      | None -> failwith "allocglobal return None" in

    al2json (caseV ("", [ Ds.Store.get (); globaladdr ]))
  | args ->
    args
    |> List.map Yojson.Safe.show
    |> String.concat ", "
    |> Printf.sprintf "global_alloc: wrong arity %s"
    |> failwith

let global_type: embedding_function = function
  | [ store_json; globaladdr_json ] ->
    let store = json2al store_json in
    let globaladdr = json2al globaladdr_json in

    let globaltype =
      store
      |> strv_access "GLOBALS"
      |> unwrap_listv_to_list
      |> Fun.flip List.nth (Z.to_int (unwrap_natv globaladdr))
      |> strv_access "TYPE" in

    al2json (caseV ("", [ Ds.Store.get (); globaltype ]))
  | args ->
    args
    |> List.map Yojson.Safe.show
    |> String.concat ", "
    |> Printf.sprintf "global_type: wrong arity %s"
    |> failwith

module EmbeddingFuncMap = Map.Make (String)
let embedding_func_map =
  EmbeddingFuncMap.empty
  |> EmbeddingFuncMap.add "module_decode" module_decode
  |> EmbeddingFuncMap.add "module_validate" module_validate
  |> EmbeddingFuncMap.add "module_imports" module_imports
  |> EmbeddingFuncMap.add "module_exports" module_exports
  |> EmbeddingFuncMap.add "module_instantiate" module_instantiate
  |> EmbeddingFuncMap.add "global_alloc" global_alloc
  |> EmbeddingFuncMap.add "global_type" global_type

let mem name = EmbeddingFuncMap.mem name embedding_func_map

let call_func name args =
  let func = EmbeddingFuncMap.find name embedding_func_map in
  func args

let run input =
  let funcname, args = parse_input input in
  (* print_endline (funcname ^ "(" ^ String.concat ", " (List.map Yojson.Safe.show args) ^ ")"); *)

  if EmbeddingFuncMap.mem funcname embedding_func_map then
    Ds.WasmContext.init_context ();
    let embedding_func = EmbeddingFuncMap.find funcname embedding_func_map in
    embedding_func args
    |> Yojson.Safe.to_string
    |> print_endline

open Al.Ast

(* AL value ↔ JSON (wire format matching wjmeta's ALValue codec) *)

let rec json_of_num : Xl.Num.num -> Yojson.Safe.t = function
  | `Nat n  -> `Assoc [("kind", `String "nat"); ("value", `String (Z.to_string n))]
  | `Int i  -> `Assoc [("kind", `String "int"); ("value", `String (Z.to_string i))]
  | `Rat q  -> `Assoc [("kind", `String "rat"); ("num", `String (Z.to_string (Q.num q))); ("den", `String (Z.to_string (Q.den q)))]
  | `Real r -> `Assoc [("kind", `String "real"); ("value", `Float r)]

and json_of_value : value -> Yojson.Safe.t = function
  | NumV n ->
    let num_fields = match json_of_num n with `Assoc fs -> fs | _ -> [] in
    `Assoc (("type", `String "num") :: num_fields)
  | BoolV b   -> `Assoc [("type", `String "bool"); ("value", `Bool b)]
  | TextV s   -> `Assoc [("type", `String "text"); ("value", `String s)]
  | ListV vs  -> `Assoc [("type", `String "list"); ("values", `List (Array.to_list !vs |> List.map json_of_value))]
  | StrV fs   -> `Assoc [("type", `String "str"); ("fields", `List (List.map (fun (k, v) -> `Assoc [("key", `String k); ("value", json_of_value !v)]) fs))]
  | CaseV (id, args) -> `Assoc [("type", `String "case"); ("id", `String id); ("args", `List (List.map json_of_value args))]
  | OptV None     -> `Assoc [("type", `String "opt"); ("value", `Null)]
  | OptV (Some v) -> `Assoc [("type", `String "opt"); ("value", json_of_value v)]
  | TupV vs   -> `Assoc [("type", `String "tup"); ("values", `List (List.map json_of_value vs))]
  | FnameV id -> `Assoc [("type", `String "fname"); ("id", `String id)]

let rec value_of_json (j : Yojson.Safe.t) : value =
  let open Yojson.Safe.Util in
  match j |> member "type" |> to_string with
  | "num" ->
    let num = match j |> member "kind" |> to_string with
      | "nat"  -> `Nat (Z.of_string (j |> member "value" |> to_string))
      | "int"  -> `Int (Z.of_string (j |> member "value" |> to_string))
      | "rat"  -> `Rat (Q.make (Z.of_string (j |> member "num" |> to_string)) (Z.of_string (j |> member "den" |> to_string)))
      | "real" -> `Real (j |> member "value" |> to_float)
      | k -> failwith ("unknown num kind: " ^ k)
    in
    NumV num
  | "bool"  -> BoolV (j |> member "value" |> to_bool)
  | "text"  -> TextV (j |> member "value" |> to_string)
  | "list"  -> ListV (ref (j |> member "values" |> to_list |> List.map value_of_json |> Array.of_list))
  | "str"   ->
    StrV (j |> member "fields" |> to_list |> List.map (fun f ->
      (f |> member "key" |> to_string, ref (f |> member "value" |> value_of_json))))
  | "case"  ->
    CaseV (j |> member "id" |> to_string,
           j |> member "args" |> to_list |> List.map value_of_json)
  | "opt"   ->
    (match j |> member "value" with
    | `Null -> OptV None
    | v     -> OptV (Some (value_of_json v)))
  | "tup"   -> TupV (j |> member "values" |> to_list |> List.map value_of_json)
  | "fname" -> FnameV (j |> member "id" |> to_string)
  | t -> failwith ("unknown AL value type: " ^ t)

(* JSON-RPC dispatcher *)

let handle_request (id : Yojson.Safe.t) (meth : string) (params : Yojson.Safe.t) : Yojson.Safe.t =
  let open Yojson.Safe.Util in
  let ok result =
    `Assoc [("jsonrpc", `String "2.0"); ("id", id); ("result", json_of_value result)]
  in
  let err code msg =
    `Assoc [("jsonrpc", `String "2.0"); ("id", id);
            ("error", `Assoc [("code", `Int code); ("message", `String msg)])]
  in
  (try
    match meth with
    | "module_decode" ->
      let bytes = params |> member "bytes" |> value_of_json in
      ok (Backend_interpreter.Embedding.module_decode bytes)
    | _ ->
      err (-32601) ("method not implemented: " ^ meth)
  with Failure msg | Invalid_argument msg ->
    err (-32603) msg)

let run () =
  try while true do
    let line = input_line stdin in
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let id     = json |> member "id" in
    let meth   = json |> member "method" |> to_string in
    let params = json |> member "params" in
    let response = handle_request id meth params in
    print_endline (Yojson.Safe.to_string response);
    flush stdout
  done with End_of_file -> ()

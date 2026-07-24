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
      | "real" ->
        (* XXX: it is hardcoding for byte, it might not work well for other floats *)
        `Nat (j |> member "value" |> to_float |> Z.of_float)
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

(* Ids for outbound requests we initiate (e.g. host_func_invoke). Independent
   from wjmeta's own id space: each side counts separately, see the
   JsonRpcConnection comment on the wjmeta side. *)
let next_request_id = ref 0
let fresh_id () = let i = !next_request_id in incr next_request_id; i

(* signed_31/signed_32/signed_64 are js-api's rendered names for Wasm Core
   spec's parametric `signed_(N)` numeric op; SpecTec itself only exposes a
   single numerics function "signed" taking the bit width as an explicit
   first argument (see backend-interpreter/numerics.ml's `signed`). This
   translation lives here rather than on the wjmeta/Scala side, since
   SpecTec is the one that knows its own rendering convention -- wjmeta just
   calls "signed_32" as if it were any other embedding function. *)
let call_signed (bits : int) (i : value) : value =
  match Backend_interpreter.Interpreter.call_func "signed" [ NumV (`Nat (Z.of_int bits)); i ] with
  | Some result -> result
  | None -> failwith (Printf.sprintf "signed_%d: no result" bits)

(* These four are mutually recursive: servicing a [func_invoke] runs the AL
   interpreter, which may perform [Host.Host_invoke] (a host function in the
   invoked code); the effect handler calls [host_func_invoke] -> [send_request];
   [send_request] pumps the channel and, on a nested inbound request, calls
   [serve_request] -> [handle_request] again. This is what makes
   host -> wasm -> host nesting work to arbitrary depth on a single channel. *)
let rec handle_request (id : Yojson.Safe.t) (meth : string) (params : Yojson.Safe.t) : Yojson.Safe.t =
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
    | "module_validate" ->
      let module_ = params |> member "module" |> value_of_json in
      ok (Backend_interpreter.Embedding.module_validate module_)
    | "module_imports" ->
      let module_ = params |> member "module" |> value_of_json in
      ok (Backend_interpreter.Embedding.module_imports module_)
    | "module_exports" ->
      let module_ = params |> member "module" |> value_of_json in
      ok (Backend_interpreter.Embedding.module_exports module_)
    | "instance_export" ->
      let moduleinst = params |> member "moduleinst" |> value_of_json in
      let name = params |> member "name" |> value_of_json in
      ok (Backend_interpreter.Embedding.instance_export moduleinst name)
    | "expand" ->
      let deftype = params |> member "deftype" |> value_of_json in
      ok (Backend_interpreter.Embedding.expand deftype)
    | "match_valtype" ->
      let valtype1 = params |> member "valtype1" |> value_of_json in
      let valtype2 = params |> member "valtype2" |> value_of_json in
      ok (Backend_interpreter.Embedding.match_valtype valtype1 valtype2)
    | "match_externtype" ->
      let externtype1 = params |> member "externtype1" |> value_of_json in
      let externtype2 = params |> member "externtype2" |> value_of_json in
      ok (Backend_interpreter.Embedding.match_externtype externtype1 externtype2)
    | "val_default" ->
      let valtype = params |> member "valtype" |> value_of_json in
      ok (Backend_interpreter.Embedding.val_default valtype)
    | "store_init" ->
      ok (Backend_interpreter.Embedding.store_init ())
    | "func_alloc" ->
      let store = params |> member "store" |> value_of_json in
      let deftype = params |> member "deftype" |> value_of_json in
      let hostfunc = params |> member "hostfunc" |> value_of_json in
      ok (Backend_interpreter.Embedding.func_alloc store deftype hostfunc)
    | "func_type" ->
      let store = params |> member "store" |> value_of_json in
      let funcaddr = params |> member "funcaddr" |> value_of_json in
      ok (Backend_interpreter.Embedding.func_type store funcaddr)
    | "func_invoke" ->
      let store = params |> member "store" |> value_of_json in
      let funcaddr = params |> member "funcaddr" |> value_of_json in
      let args = params |> member "args" |> value_of_json in
      (* Wasm execution may reenter wjmeta via the [Host_invoke] effect when the
         invoked code calls a host function. Handle it by issuing a matching
         [host_func_invoke] outbound request and resuming with its results. *)
      let result =
        Effect.Deep.try_with
          (fun () -> Backend_interpreter.Embedding.func_invoke store funcaddr args)
          ()
          { effc = (fun (type a) (eff : a Effect.t) ->
              match eff with
              | Backend_interpreter.Host.Host_invoke (hid, vals) ->
                Some (fun (k : (a, value) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (host_func_invoke hid vals))
              | _ -> None) }
      in
      ok result
    | "module_instantiate" ->
      let store = params |> member "store" |> value_of_json in
      let module_ = params |> member "module" |> value_of_json in
      let externvals = params |> member "externvals" |> value_of_json in
      (* Instantiation can run a start function, which may itself call a host
         function — same reentrancy concern (and handling) as [func_invoke]. *)
      let result =
        Effect.Deep.try_with
          (fun () -> Backend_interpreter.Embedding.module_instantiate store module_ externvals)
          ()
          { effc = (fun (type a) (eff : a Effect.t) ->
              match eff with
              | Backend_interpreter.Host.Host_invoke (hid, vals) ->
                Some (fun (k : (a, value) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (host_func_invoke hid vals))
              | _ -> None) }
      in
      ok result
    (* Wasm Core spec numerics referenced directly by js-api prose -- not
       part of the formal embedding.rst boundary, but simple enough (pure,
       no store) to expose here. See `call_signed` above. *)
    | "signed_31" ->
      let i = params |> member "i" |> value_of_json in
      ok (call_signed 31 i)
    | "signed_32" ->
      let i = params |> member "i" |> value_of_json in
      ok (call_signed 32 i)
    | "signed_64" ->
      let i = params |> member "i" |> value_of_json in
      ok (call_signed 64 i)
    | _ ->
      err (-32601) ("method not implemented: " ^ meth)
  with
  | Failure msg | Invalid_argument msg ->
    err (-32603) msg
  | Backend_interpreter.Exception.Error (at, msg, step) ->
    err (-32603) (msg ^ " (interpreting " ^ step ^ " at " ^ Util.Source.string_of_region at ^ ")")
  | Backend_interpreter.Exception.Invalid (e, backtrace) ->
    err (-32603) ("Invalid: " ^ Printexc.to_string e ^ "\n" ^ Printexc.raw_backtrace_to_string backtrace)
  | e ->
    err (-32603) (Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ()))

(* Reenter wjmeta to run host function [hid] with [vals], returning its [val*]. *)
and host_func_invoke (hid : string) (vals : value list) : value list =
  let params =
    `Assoc [("id", `String hid);
            ("args", `List (List.map json_of_value vals))] in
  match send_request "host_func_invoke" params with
  | ListV vs -> Array.to_list !vs
  | _ -> failwith "host_func_invoke: expected list result"

(* Read+handle+answer a single inbound request. *)
and serve_request (json : Yojson.Safe.t) : unit =
  let open Yojson.Safe.Util in
  let id     = json |> member "id" in
  let meth   = json |> member "method" |> to_string in
  let params = json |> member "params" in
  print_endline (Yojson.Safe.to_string (handle_request id meth params));
  flush stdout

(* Send an outbound request, then pump the channel until its response arrives,
   serving any inbound requests met in the meantime (reentrancy). *)
and send_request (meth : string) (params : Yojson.Safe.t) : value =
  let id = fresh_id () in
  let request =
    `Assoc [("jsonrpc", `String "2.0"); ("id", `Int id);
            ("method", `String meth); ("params", params)] in
  print_endline (Yojson.Safe.to_string request);
  flush stdout;
  let rec pump () =
    let json = Yojson.Safe.from_string (input_line stdin) in
    let open Yojson.Safe.Util in
    if (json |> member "method") <> `Null then
      (serve_request json; pump ())                 (* nested inbound request *)
    else if (json |> member "id") = `Int id then
      (match json |> member "result" with
       | `Null -> failwith (json |> member "error" |> member "message" |> to_string)
       | result -> value_of_json result)
    else
      failwith "send_request: response id mismatch"
  in
  pump ()

let run () =
  try while true do
    serve_request (Yojson.Safe.from_string (input_line stdin))
  done with End_of_file -> ()

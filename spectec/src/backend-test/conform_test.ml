open Unix

let env = environment ()

let has_substring str sub =
  let len_s = String.length str in
  let len_sub = String.length sub in
  let rec aux i =
    if i > len_s - len_sub then false
    else if String.sub str i len_sub = sub then true
    else aux (i + 1)
  in
  aux 0

let classify err =
  (* TODO: also look at the input program *)
  (* reference interpreter *)
  if has_substring err "runtime crash: type mismatch for element" then
    "reference interpreter: return_call_indirect"
  else if has_substring err "indirect calls must go through a table with type <= funcref" then
    "reference interpreter: return_call_indirect"
  else if has_substring err "Wasm.Data.Bounds" then
    "reference interpreter: data"
  (* wasmtime *)
  else if has_substring err "unrecoverable error when allocating" then
    "wasmtime: v128_array_ref_table_initializer"
  else if has_substring err "expected trap, got Core(" then
    "wasmtime: out_of_bound_table_none_reference"
  (* Unsupported *)
  else if has_substring err "exception handling featrue" then
    "wasmtime: unsupported exc"
  else if has_substring err "exceptions" then
    "wasmtime: unsupported exc"
  else if has_substring err "RefNull" then
    "wasmtime: unsupported RefNull"
  else if has_substring err "requested allocation's alignment of 16 is greater than max supported alignment of 8" then
    "wasmtime: unsupported alignment"
  (* west error *)
  else if has_substring err "expected `)`" then
    "Syntax Error"
  else if has_substring err "expected keyword" then
    "Syntax Error"
  else if has_substring err "unexpected token" then
    "Syntax Error"
  else
    err

let test_engine engine wast =
  let cmd = engine ^ " " ^ wast in
  Log.debug cmd;
  (* Execute shell command *)
  let (stdout, stdin, stderr) = open_process_full cmd env in
  let out = In_channel.input_all stdout in
  let err = In_channel.input_all stderr in
  Log.debug ("[Stdout]\n" ^ out);
  Log.debug ("[Stderr]\n " ^ err);
  let st = close_process_full (stdout, stdin, stderr) in
  match st with
  | WEXITED st when st > 0 -> Log.warn ("`" ^ cmd ^ "` failed: " ^ classify err)
  | WEXITED _ -> ()
  | WSIGNALED _ -> kill 0 Sys.sigint
  | WSTOPPED _ -> kill 0 Sys.sigstop

let conform_test seed =
  let wast = Printf.sprintf "out/%d.wast" seed in

  (* TODO: parallelize *)
  test_engine "../interpreter/wasm" wast;
  (* test_engine "wasmtime wast -W all-proposals=y" wast; *)

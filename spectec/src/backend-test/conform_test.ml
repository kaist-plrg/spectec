open Unix

let env = environment ()

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
  | WEXITED st when st > 0 -> Log.warn ("`" ^ cmd ^ "` failed")
  | WEXITED _ -> ()
  | WSIGNALED _ -> kill 0 Sys.sigint
  | WSTOPPED _ -> kill 0 Sys.sigstop

let conform_test seed =
  let wast = Printf.sprintf "out/%d.wast" seed in

  test_engine "../interpreter/wasm" wast;
  test_engine "wasmtime wast" wast;

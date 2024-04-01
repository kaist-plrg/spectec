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
  (match st with
  | WEXITED _ -> ()
  | WSIGNALED _ -> kill 0 Sys.sigint
  | WSTOPPED _ -> kill 0 Sys.sigstop
  );
  st

let conform_test seed =
  let wast = Printf.sprintf "out/%d.wast" seed in

  if (
    test_engine "/home/wasmtime/target/release/wasmtime wast" wast <>
    test_engine "wasmtime wast" wast
  ) then
    Log.warn (wast ^ " failed")
  else (
    system ("rm " ^ wast ^ " 2> /dev/null") |> ignore;
    system (Printf.sprintf "rm out/%d-* 2> /dev/null" seed) |> ignore
  )

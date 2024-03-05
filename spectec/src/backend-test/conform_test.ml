let test_engine engine wast =
  let cmd = engine ^ " " ^ wast in
  Log.debug cmd;
  (* Execute shell command *)
  let st = Sys.command (cmd ^ "> /dev/null 2> /dev/null") in
  Log.debug ("Status: " ^ (string_of_int st));
  if st > 0 then
    Log.warn ("`" ^ cmd ^ "` failed")

let conform_test seed =
  let wast = Printf.sprintf "out/%d.wast" seed in
  test_engine "bin/wasm" wast;
  test_engine "bin/wasmtime wast" wast;

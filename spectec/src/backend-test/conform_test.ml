let known_msgs = [
  "cranelift/codegen/src/machinst/lower.rs:766:21";
]

let is_known_bug msg =
  Sys.command ("grep " ^ msg ^ " stderr &> /dev/null") = 0

let test_engine engine wast =
  let cmd = engine ^ " " ^ wast in
  Log.debug cmd;
  (* Execute shell command *)
  let st =
    let res = Sys.command (cmd ^ "> /dev/null 2> stderr") in
    if res = 0 || List.exists is_known_bug known_msgs then 0
    else res
  in
  Log.debug ("Status: " ^ (string_of_int st));
  if st > 0 then
    Log.warn ("`" ^ cmd ^ "` failed")

let conform_test seed =
  let wast = Printf.sprintf "out/%d.wast" seed in
  test_engine "../interpreter/wasm" wast;
  test_engine "wasmtime wast" wast;
  test_engine "wasmer wast" wast;

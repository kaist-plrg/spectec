let known_msgs = [
  "cranelift/codegen/src/machinst/lower.rs:766:21";
]

let is_known_bug msg =
  Sys.command ("grep " ^ msg ^ " stderr > /dev/null") = 0

let test_engine engine wast =
  let cmd = engine ^ " " ^ wast in
  Log.debug cmd;
  (* Execute shell command *)
  let st =
    let res = Sys.command (cmd ^ "> /dev/null 2> stderr") in
    if res = 0 || List.exists is_known_bug known_msgs then
      0
    else res
  in
  if st = 255 then exit 130;
  Log.debug ("Status: " ^ (string_of_int st));
  st

let conform_test seed =
  let wast = Printf.sprintf "out/%d.wast" seed in
  let wt_12 = test_engine "../../wasmtime/target/release/wasmtime wast" wast in
  let wt_18 = test_engine "wasmtime wast" wast in
  if wt_12 <> wt_18 then Log.warn ("../../wasmtime/target/release/wasmtime wast " ^ wast)
  else Sys.command ("rm " ^ wast) |> ignore

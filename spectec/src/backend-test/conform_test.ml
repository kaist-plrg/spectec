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

let warn engine wast status =
  if status <> 0 then Log.warn ("`" ^ engine ^ " " ^ wast ^ "` failed")

let conform_test seed =
  let wast = Printf.sprintf "out/%d.wast" seed in
  let st_ref = test_engine "../interpreter/wasm" wast in
  let st_wt = test_engine "wasmtime wast" wast in
  let st_wr = test_engine "wasmer wast" wast in
  let st_we = test_engine "runtimes/wasmedge/wasmedge" wast in
  match st_ref, st_wt, st_wr, st_we with
  | 0, 0, 0, 0 -> Sys.command ("rm " ^ wast) |> ignore
  | 0, wt, wr, 0 when wt = wr ->
    Log.warn (wast ^ " may have nondeterministic behavior");
    Sys.command ("rm " ^ wast) |> ignore
  | ref, _, _, _ when ref != 0 ->
    Log.warn (wast ^ " may have wrong result")
  | _ ->
    warn "../interpreter/wasm" wast st_ref;
    warn "wasmtime wast" wast st_wt;
    warn "wasmer wast" wast st_wr;
    warn "runtimes/wasmedge/wasmedge" wast st_we

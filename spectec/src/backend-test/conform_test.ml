let test_engine engine wast = Sys.command (engine ^ " " ^ wast) |> ignore

let conform_test seed =
  let wast = Printf.sprintf "out/%d.wast" seed in
  test_engine "bin/wasm" wast;
  test_engine "bin/wasmtime wast" wast;

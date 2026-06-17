open Reference_interpreter
open Al.Ast
open Al.Al_util

let embedding_error = caseV ("error", [])

let module_decode (bytes_val : value) : value =
  match bytes_val with
  | ListV vs ->
    let buf = Buffer.create (Array.length !vs) in
    Array.iter (function
      | NumV (`Nat n) -> Buffer.add_char buf (Char.chr (Z.to_int n))
      | _ -> failwith "module_decode: expected list of nat bytes"
    ) !vs;
    (try Decode.decode "" (Buffer.contents buf) |> Construct.al_of_module
     with Decode.Code _ -> embedding_error)
  | _ -> failwith "module_decode: expected list value for bytes"

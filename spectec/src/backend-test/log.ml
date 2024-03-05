(* Logging system
 * 0: trace
 * 1: debug
 * 2: info
 * 3: warn
 * 4: error
 *)

let trace msg = if !Flag.log <= 0 then print_endline msg
let debug msg = if !Flag.log <= 1 then print_endline msg
let info msg  = if !Flag.log <= 2 then print_endline msg
let warn msg  = if !Flag.log <= 3 then print_endline msg
let error msg = if !Flag.log <= 4 then print_endline msg

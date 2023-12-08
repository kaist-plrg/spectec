open Al
open Ast
open Al_util

type valtype =
| T of value
| SubT of string * string
| TopT
| BotT
| SeqT of valtype
type restype = valtype list

let i32T = singleton "I32"
let i64T = singleton "I64"
let f32T = singleton "F32"
let f64T = singleton "F64"

(* Helper *)
let nt_matches_op nt op = match nt, op with
| CaseV ("I32", []), CaseV ("_I", _)
| CaseV ("I64", []), CaseV ("_I", _)
| CaseV ("F32", []), CaseV ("_F", _)
| CaseV ("F64", []), CaseV ("_F", _) -> true
| _ -> false

let is_inn nt = nt = i32T || nt = i64T

let default = function
| T (CaseV ("I32", [])) -> case_vv "CONST" (singleton "I32") zero
| T (CaseV ("I64", [])) -> case_vv "CONST" (singleton "I64") zero
| T (CaseV ("F32", [])) -> case_vv "CONST" (singleton "F32") zero
| T (CaseV ("F64", [])) -> case_vv "CONST" (singleton "F64") zero
| T (CaseV (("FUNCREF" | "EXTERNREF"), []) as rt) -> case_v "REF.NULL" rt
| _ -> singleton "UNREACHABLE"

(** Estimate if the given module is valid **)

(* Check if cvtop is syntactically correct *)
let correct_cvtop = function
| [ CaseV (t2, []); CaseV (cvtop, []); CaseV (t1, []); OptV (sx_opt) ] ->
  let x2 = String.sub t2 0 1 in
  let n2 = String.sub t2 1 2 in
  let x1 = String.sub t1 0 1 in
  let n1 = String.sub t1 1 2 in
  ( match (x2, n2, cvtop, x1, n1, sx_opt) with
  | "I", "32", "WRAP", "I", "64", None
  | "I", "64", "EXTEND", "I", "32", Some _
  | "I", _, "TRUNC", "F", _, Some _
  | "I", _, "TRUNC_SAT", "F", _, Some _
  | "F", "32", "DEMOTE", "F", "64", None
  | "F", "64", "PROMOTE", "F", "32", None
  | "F", _, "CONVERT", "I", _, Some _ -> true
  | _, _, "REINTERPRET", _, _, None -> x2 <> x1 && n2 = n1
  | _ -> false )
| _ -> false

(* Estimate if given instruction is valid with expected type, rt1* -> rt2* *)
(* TODO: Perhaps some of these can be automated? *)
let validate_instr case args const (_rt1, rt2) =
  match case with
  | "UNOP" | "BINOP" | "TESTOP" | "RELOP" -> ( match args, rt2 with
    | [ nt; op ], [ T t ] when nt_matches_op nt op && (not const || is_inn t) -> Some args
    | _ -> None )
  | "EXTEND" -> ( match args with
    | nt :: _ when is_inn nt -> Some args
    | _ -> None )
  | "CVTOP" -> if correct_cvtop args then Some args else None
  | "GLOBAL.GET" -> ( match rt2 with
    | [ T (CaseV ("I32", [])) ] -> Some [ NumV 0L ]
    | [ T (CaseV ("I64", [])) ] -> Some [ NumV 1L ]
    | [ T (CaseV ("F32", [])) ] -> Some [ NumV 2L ]
    | [ T (CaseV ("F64", [])) ] -> Some [ NumV 3L ]
    | _ -> None )
  | "GLOBAL.SET" -> ( match args with
    | [ gid ] -> Some [ add_num gid (NumV 4L) ]
    | _ -> None )
  | "CALL_REF" | "RETURN_CALL_REF" -> ( match args with
    | [ OptV (Some _) ] -> Some args
    | _ -> None )
  | "LOAD" | "STORE" -> (match args with
    | nt :: opt :: tl ->
      Some (if is_inn nt then nt :: opt :: tl else nt :: OptV None :: tl)
    | _ -> None)
  | _ -> Some args

open Al
open Ast
open Al_util

type valtype =
| T of value
| SubT of string * string
| TopT
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
let validate_instr case args const (rt1, rt2) =
  match case with
  | "UNOP" | "BINOP" | "TESTOP" | "RELOP" -> ( match args, rt1, rt2 with
    | [ _nt; op ], T nt1 :: _, [ T t2 ] when nt_matches_op nt1 op && (not const || is_inn t2) -> Some [ nt1; op ]
    | _ -> None )
  | "EXTEND" -> ( match args, rt1 with
    | _nt :: tl, [ T nt ] when is_inn nt -> Some (nt :: tl)
    | _ -> None )
  | "CVTOP" -> ( match args, rt1, rt2 with
    | [ _nt2; a1; _nt1; a2 ],  [ T nt1 ], [ T nt2 ] when correct_cvtop [ nt2; a1; nt1; a2 ] -> Some [ nt2; a1; nt1; a2 ]
    | _ -> None )
  | "CONST" -> ( match args, rt2 with
    | [ _nt; a ], [ T nt ] -> Some [ nt; a ]
    | _ -> None )
  | "REF.NULL" when !Backend_interpreter.Construct.version < 3 -> ( match args, rt2 with
    | [ _rt ], [ T rt ] -> Some [ rt ]
    | _ -> None )
  | "GLOBAL.GET" -> ( match rt2 with
    | [ T (CaseV ("I32", [])) ] -> Some [ NumV 0L ]
    | [ T (CaseV ("I64", [])) ] -> Some [ NumV 1L ]
    | [ T (CaseV ("F32", [])) ] -> Some [ NumV 2L ]
    | [ T (CaseV ("F64", [])) ] -> Some [ NumV 3L ]
    | _ -> None )
  | "CALL_REF" | "RETURN_CALL_REF" -> ( match args with
    | [ OptV (Some _) ] -> Some args
    | _ -> None )
  | "LOAD" -> (match args, rt2 with
    | _nt :: opt :: tl, [ T nt ] ->
      Some (if is_inn nt then nt :: opt :: tl else nt :: OptV None :: tl)
    | _ -> None)
  | "STORE" -> (match args, rt1 with
    | _nt :: opt :: tl, [ _; T nt ] ->
      Some (if is_inn nt then nt :: opt :: tl else nt :: OptV None :: tl)
    | _  -> None )
  | _ -> Some args

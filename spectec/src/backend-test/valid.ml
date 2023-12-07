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
| CaseV ("I32", []), CaseV ("_I", [])
| CaseV ("I64", []), CaseV ("_I", [])
| CaseV ("F32", []), CaseV ("_F", [])
| CaseV ("F64", []), CaseV ("_F", []) -> true
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
let valid_instr case args const (rt1, rt2) =
  match case with
  | "UNOP" | "BINOP" | "TESTOP" | "RELOP" -> ( match args, rt1 with
    | [ nt1; op ], [ T nt2 ] -> nt1 = nt2 && nt_matches_op nt1 op && (not const || is_inn nt1)
    | _ -> false )
  | "EXTEND" -> ( match args, rt1 with
    | [ nt1; _ ], [ T nt2 ] -> nt1 = nt2 && is_inn nt1
    | _ -> false )
  | "CVTOP" -> correct_cvtop args && ( match args, rt1, rt2 with
    | [ nt2; _; nt1; _ ],  [ T nt1' ], [ T nt2' ] -> nt1 = nt1' && nt2 = nt2'
    | _ -> false )
  | "CONST" -> ( match args, rt2 with
    | [ nt; _ ], [ T nt' ] -> nt = nt'
    | _ -> false )
  | "REF.NULL" when !Backend_interpreter.Construct.version < 3 -> ( match args, rt2 with
    | [ rt ], [ T rt' ] -> rt = rt'
    | _ -> false )
  | "GLOBAL.GET" -> ( match args with
    (* builtin globals *)
    | [ NumV 0L ] -> rt2 = [ T i32T ]
    | [ NumV 1L ] -> rt2 = [ T i64T ]
    | [ NumV 2L ] -> rt2 = [ T f32T ]
    | [ NumV 3L ] -> rt2 = [ T f64T ]
    | _ -> true (* Over-approximate *) )
  | "CALL_REF" | "RETURN_CALL_REF" -> ( match args with
    | [ OptV (Some _) ] -> true
    | _ -> false )
  | "LOAD" | "STORE" -> (match args with
    | nt :: OptV (Some _) :: _ -> is_inn nt
    | _  -> true )
  | _ -> true

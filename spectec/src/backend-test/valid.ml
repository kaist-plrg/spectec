open Al
open Ast
open Al_util
open Utils
open Util

type valtype =
| T of value
| SubT of string * string
| TopT
| BotT
| SeqT of valtype
type restype = valtype list

let i32T = nullary "I32"
let i64T = nullary "I64"
let f32T = nullary "F32"
let f64T = nullary "F64"

(* Helper *)
let nt_matches_op nt op = match nt, op with
| CaseV ("I32", []), CaseV ("_I", _)
| CaseV ("I64", []), CaseV ("_I", _)
| CaseV ("F32", []), CaseV ("_F", _)
| CaseV ("F64", []), CaseV ("_F", _) -> true
| _ -> false

let is_inn nt = nt = i32T || nt = i64T
let is_fnn nt = nt = f32T || nt = f64T

let default = function
| T (CaseV ("I32", [])) -> caseV ("CONST", [nullary "I32"; zero])
| T (CaseV ("I64", [])) -> caseV ("CONST", [nullary "I64"; zero])
| T (CaseV ("F32", [])) -> caseV ("CONST", [nullary "F32"; zero])
| T (CaseV ("F64", [])) -> caseV ("CONST", [nullary "F64"; zero])
| T (CaseV (("FUNCREF" | "EXTERNREF"), []) as rt) -> caseV ("REF.NULL", [rt])
| _ -> nullary "UNREACHABLE"

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
    | [ CaseV ("I32", []) as nt; NumV 32L ] -> Some [ nt; choose [ numV 8L; numV 16L ] ]
    | nt :: _ when is_inn nt -> Some args
    | _ -> None )
  | "CVTOP" -> if correct_cvtop args then Some args else None
  | "CALL_REF" | "RETURN_CALL_REF" -> ( match args with
    | [ OptV (Some _) ] -> Some args
    | _ -> None )
  | "LOAD" | "STORE" -> (match args with
    | [ nt; opt; memop ] ->
      let decompose_nt = function
      | CaseV ("I32", []) -> "I", 32
      | CaseV ("I64", []) -> "I", 64
      | CaseV ("F32", []) -> "F", 32
      | CaseV ("F64", []) -> "F", 64
      | _ -> failwith "Unreachable" in
      let (x, n) = decompose_nt nt in

      let decompose_opt = function
      | OptV None -> false, n, ""
      | OptV (Some (TupV [ m; s ])) -> true, unwrap_numv_to_int m, casev_of_case s
      | OptV (Some m) -> true, unwrap_numv_to_int m, ""
      | _ -> failwith "Unreachable" in
      let compose_opt is_some m s = match is_some, s with
      | true, "" -> OptV (Some ( numV_of_int m ))
      | true, _  -> OptV (Some ( TupV [ numV_of_int m; nullary s ]))
      | false, _ -> OptV None in
      let (is_some, m, s) = decompose_opt opt in

      let decompose_memop s = (
          s |> unwrap_strv |> Record.find "ALIGN" |> unwrap_numv_to_int,
          s |> unwrap_strv |> Record.find "OFFSET" |> unwrap_numv_to_int
        )in
      let compose_memop a o = StrV (Record.empty |> Record.add "ALIGN" (numV_of_int a) |> Record.add "OFFSET" (numV_of_int o)) in
      let (a, o) = decompose_memop memop in

      let is_some' = if x = "F" then false else is_some in

      let rec dec_m m =
        if not is_some' then m else
        if m < n then m else
        dec_m (m / 2)
      in
      let m' = dec_m m in

      let rec dec_align a =
        if 1 lsl a <= m'/8 then a else
        dec_align (a-1)
      in
      let a' = dec_align a in

      Some [ nt; compose_opt is_some' m' s; compose_memop a' o ]
    | _ -> None)
  | "SELECT" -> ( match args, rt2 with
    | [ OptV None ], [ T t ] -> if is_inn t || is_fnn t then Some args else None
    | [ OptV _ ], [ T t ] -> Some [ OptV (Some (singleton t)) ]
    | _ -> None )
  | _ -> Some args

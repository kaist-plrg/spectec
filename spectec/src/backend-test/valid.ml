open Al
open Ast
open Al_util
open Utils
open Util.Record

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
    | [ CaseV ("I32", []) as nt; NumV 32L ] -> Some [ nt; choose [ numV 8; numV 16 ] ]
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
      | OptV (Some (TupV [ m; s ])) -> true, al_to_int m, case_of_case s
      | OptV (Some m) -> true, al_to_int m, ""
      | _ -> failwith "Unreachable" in
      let compose_opt is_some m s = match is_some, s with
      | true, "" -> OptV (Some ( numV m ))
      | true, _  -> OptV (Some ( TupV [ numV m; singleton s ]))
      | false, _ -> OptV None in
      let (is_some, m, s) = decompose_opt opt in

      let decompose_memop s = field_of_str "ALIGN" s |> al_to_int, field_of_str "OFFSET" s |> al_to_int in
      let compose_memop a o = StrV (Record.empty |> Record.add "ALIGN" (numV a) |> Record.add "OFFSET" (numV o)) in
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
  | _ -> Some args

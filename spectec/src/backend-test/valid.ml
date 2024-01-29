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

let validate_shape = function
  | TupV [ CaseV ("I8", []); NumV _ ] -> tupV [ caseV ("I8", []); numV 16L ]
  | TupV [ CaseV ("I16", []); NumV _ ] -> tupV [ caseV ("I16", []); numV 8L ]
  | TupV [ CaseV ("I32", []); NumV _ ] -> tupV [ caseV ("I32", []); numV 4L ]
  | TupV [ CaseV ("I64", []); NumV _ ] -> tupV [ caseV ("I64", []); numV 2L ]
  | TupV [ CaseV ("F32", []); NumV _ ] -> tupV [ caseV ("F32", []); numV 4L ]
  | TupV [ CaseV ("F64", []); NumV _ ] -> tupV [ caseV ("F64", []); numV 2L ]
  | v -> failwith ("Invalid shape: " ^ Print.string_of_value v)

(* Estimate if given instruction is valid with expected type, rt1* -> rt2* *)
(* TODO: Perhaps some of these can be automated? *)
let validate_instr case args const (_rt1, rt2) =
  match case with
  | "UNOP" | "BINOP" | "TESTOP" | "RELOP" -> ( match args, rt2 with
    | [ nt; op ], [ T t ] when nt_matches_op nt op && (not const || is_inn t) -> Some args
    | _ -> None )
  | "VUNOP" | "VBINOP" | "VRELOP" ->
    (match List.hd (List.tl args) with
    | CaseV ("_VI", _) ->
      let i = choose [8; 16; 32; 64] in
      Some [ tupV [ nullary ("I"^(string_of_int i)); numV_of_int (128/i) ]; List.hd (List.rev args) ]
    | CaseV ("_VF", _) ->
      let i = choose [32; 64] in
      Some [ tupV [ nullary ("F"^(string_of_int i)); numV_of_int (128/i) ]; List.hd (List.rev args) ]
    | _ -> failwith "invalid op"
    )
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
  | "VEXTRACT_LANE" ->
    let ext = choose [nullary "S"; nullary "U"] in
    (match args with
    | [ TupV [ CaseV ("I8", []); NumV n ]; _; _ ] ->
      Some [ TupV [ CaseV ("I8", []); NumV n ]; OptV (Some ext); numV (Random.int64 n) ]
    | [ TupV [ CaseV ("I16", []); NumV n ]; _; _ ] ->
      Some [ TupV [ CaseV ("I16", []); NumV n ]; OptV (Some ext); numV (Random.int64 n) ]
    | [ TupV [ CaseV ("I32", []); NumV n ]; _; _ ] ->
      Some [ TupV [ CaseV ("I32", []); NumV n ]; OptV None; numV (Random.int64 n) ]
    | [ TupV [ CaseV ("I64", []); NumV n ]; _; _ ] ->
      Some [ TupV [ CaseV ("I64", []); NumV n ]; OptV None; numV (Random.int64 n) ]
    | [ TupV [ CaseV ("F32", []); NumV n ]; _; _ ] ->
      Some [ TupV [ CaseV ("F32", []); NumV n ]; OptV None; numV (Random.int64 n) ]
    | [ TupV [ CaseV ("F64", []); NumV n ]; _; _ ] ->
      Some [ TupV [ CaseV ("F64", []); NumV n ]; OptV None; numV (Random.int64 n) ]
    | v -> failwith ("Invalid vec extract lane op: " ^ Print.string_of_value (listV_of_list v))
    )
  | "VREPLACE_LANE" ->
    (match args with
    | [ TupV [ CaseV ("I8", []); NumV n ]; _ ] ->
      Some [ TupV [ CaseV ("I8", []); NumV n ]; numV (Random.int64 n) ]
    | [ TupV [ CaseV ("I16", []); NumV n ]; _ ] ->
      Some [ TupV [ CaseV ("I16", []); NumV n ]; numV (Random.int64 n) ]
    | [ TupV [ CaseV ("I32", []); NumV n ]; _ ] ->
      Some [ TupV [ CaseV ("I32", []); NumV n ]; numV (Random.int64 n) ]
    | [ TupV [ CaseV ("I64", []); NumV n ]; _ ] ->
      Some [ TupV [ CaseV ("I64", []); NumV n ]; numV (Random.int64 n) ]
    | [ TupV [ CaseV ("F32", []); NumV n ]; _ ] ->
      Some [ TupV [ CaseV ("F32", []); NumV n ]; numV (Random.int64 n) ]
    | [ TupV [ CaseV ("F64", []); NumV n ]; _ ] ->
      Some [ TupV [ CaseV ("F64", []); NumV n ]; numV (Random.int64 n) ]
    | v -> failwith ("Invalid vec replace lane op: " ^ Print.string_of_value (listV_of_list v))
    )
  (* cvtop *)
  | "VCVTOP" ->
    (match casev_of_case (List.hd (List.tl args)) with
    | "EXTEND" ->
      let i = choose [16; 32; 64] in
      let i2 = i/2 in
      let arg1 = tupV [ nullary ("I"^(string_of_int i)); numV_of_int (128/i) ] in
      let arg2 = tupV [ nullary ("I"^(string_of_int i2)); numV_of_int (128/i2) ] in
      let half = choose [nullary "HIGH"; nullary "LOW"] in
      let ext = choose [nullary "S"; nullary "U"] in
      Some ([arg1; nullary "EXTEND"; optV (Some half); arg2; optV (Some ext); none "ZERO"])
    | "TRUNC_SAT" ->
      let i = choose [32; 64] in
      let arg1 = tupV [ nullary "I32"; numV_of_int (128/32) ] in
      let arg2 = tupV [ nullary ("F"^(string_of_int i)); numV_of_int (128/i) ] in
      let ext = choose [nullary "S"; nullary "U"] in
      let zero = if i = 32 then none "ZERO" else some "ZERO" in
      Some ([arg1; nullary "TRUNC_SAT"; optV None; arg2; optV (Some ext); zero])
    | "CONVERT" ->
      let i = choose [32; 64] in
      let arg1 = tupV [ nullary ("F"^(string_of_int i)); numV_of_int (128/i) ] in
      let arg2 = tupV [ nullary "I32"; numV_of_int (128/32) ] in
      let half = choose [None; Some (nullary "LOW")] in
      let ext = choose [nullary "S"; nullary "U"] in
      Some ([arg1; nullary "CONVERT"; optV half; arg2; optV (Some ext); none "ZERO"])
    | "DEMOTE" ->
      let arg1 = tupV [ nullary "F32"; numV_of_int (128/32) ] in
      let arg2 = tupV [ nullary "F64"; numV_of_int (128/64) ] in
      Some ([arg1; nullary "DEMOTE"; optV None; arg2; optV None; some "ZERO"])
    | "PROMOTE" ->
      let arg1 = tupV [ nullary "F64"; numV_of_int (128/64) ] in
      let arg2 = tupV [ nullary "F32"; numV_of_int (128/32) ] in
      Some ([arg1; nullary "PROMOTE"; optV (Some (nullary "LOW")); arg2; optV None; none "ZERO"])
    | _ -> Some args
    )
  (* special vbinop *)
  | "VSWIZZLE" -> Some [ TupV [ CaseV ("I8", []); NumV 16L ]; ]
  | "VSHUFFLE" ->
    Some [ TupV [ CaseV ("I8", []); NumV 16L ]; List.hd (List.rev args) ]
  | "VNARROW" ->
    let i = choose [8; 16] in
    let i2 = i*2 in
    let arg1 = tupV [ caseV ("I"^(string_of_int i), []); numV_of_int (128/i) ] in
    let arg2 = tupV [ caseV ("I"^(string_of_int i2), []); numV_of_int (128/i2) ] in
    let ext = choose [nullary "S"; nullary "U"] in
    Some [ arg1; arg2; ext ]
  | "VEXTMUL" ->
    let i = choose [16; 32; 64] in
    let i2 = i/2 in
    let arg1 = tupV [ caseV ("I"^(string_of_int i), []); numV_of_int (128/i) ] in
    let arg2 = tupV [ caseV ("I"^(string_of_int i2), []); numV_of_int (128/i2) ] in
    let hl = choose [nullary "HIGH"; nullary "LOW"] in
    let ext = choose [nullary "S"; nullary "U"] in
    Some [ arg1; hl; arg2; ext ]
  | "VDOT" ->
    Some [ TupV [ CaseV ("I32", []); NumV 4L]; TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("S", []) ]
  | "VLOAD_LANE" | "VSTORE_LANE" ->
    (match args with
    | [ NumV n; memop; _ ] -> Some [ NumV n; memop; numV (Int64.div 128L n) ]
    | v -> failwith ("Invalid vec load/store lane op: " ^ Print.string_of_value (listV_of_list v))
    )
  (* special vcvtop *)
  | "VEXTADD_PAIRWISE" ->
    let i = choose [16; 32] in
    let i2 = i/2 in
    let arg1 = tupV [ caseV ("I"^(string_of_int i), []); numV_of_int (128/i) ] in
    let arg2 = tupV [ caseV ("I"^(string_of_int i2), []); numV_of_int (128/i2) ] in
    let ext = choose [nullary "S"; nullary "U"] in
    Some [ arg1; arg2; ext ]
  (* only ishape *)
  | "VISHIFTOP" ->
    let i = choose [8; 16; 32; 64] in
    Some [ tupV [ nullary ("I"^(string_of_int i)); numV_of_int (128/i) ]; List.hd (List.rev args) ]
  | "VBITMASK" | "VALL_TRUE" ->
    let i = choose [8; 16; 32; 64] in
    Some [ tupV [ nullary ("I"^(string_of_int i)); numV_of_int (128/i) ] ]
  | _ -> Some args

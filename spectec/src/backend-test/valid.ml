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
let make_ishape i = tupV [ nullary ("I"^(string_of_int i)); numV_of_int (128/i) ]
let make_fshape i = tupV [ nullary ("F"^(string_of_int i)); numV_of_int (128/i) ]

let rec string_of_vt = function
| T v -> Al.Print.string_of_value v
| SubT (x, sub) -> x ^ "<:" ^ sub
| TopT -> "_"
| BotT -> "⊥"
| SeqT t -> (string_of_vt t) ^ "*"
let string_of_rt vts = List.map string_of_vt vts |> String.concat " "

let decompose_vloadop = function
| OptV (Some (CaseV ("ZERO", [ NumV n ])))
| OptV (Some (CaseV ("SPLAT", [ NumV n ]))) -> n
| OptV (Some (CaseV ("SHAPE", [ TupV [NumV m; NumV n]; _ ]))) -> Int64.mul m n
| OptV (None) -> 128L
| v -> failwith ("Invalid vloadop: " ^ Print.string_of_value v)

let decompose_memop s = (
    s |> unwrap_strv |> Record.find "ALIGN" |> unwrap_numv_to_int,
    s |> unwrap_strv |> Record.find "OFFSET" |> unwrap_numv_to_int
  )
let compose_memop a o = StrV (Record.empty |> Record.add "ALIGN" (numV_of_int a) |> Record.add "OFFSET" (numV_of_int o))

let rec dec_align a n =
  if 1 lsl a <= n then a else
  dec_align (a-1) n

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
let validate_instr case args const (rt1, rt2) =
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

      let (a, o) = decompose_memop memop in

      let is_some' = if x = "F" then false else is_some in

      let rec dec_m m =
        if not is_some' then m else
        if m < n then m else
        dec_m (m / 2)
      in
      let m' = dec_m m in

      let a' = dec_align a (m'/8) in

      Some [ nt; compose_opt is_some' m' s; compose_memop a' o ]
    | _ -> None)
  | "SELECT" -> ( match args, rt2 with
    | [ OptV None ], [ T t ] -> if is_inn t || is_fnn t then Some args else None
    | [ OptV _ ], [ T t ] -> Some [ OptV (Some (singleton t)) ]
    | _ -> None )
  | "VLOAD" ->
  (match args with
    | [ vloadop; memop; ] ->
      let n = decompose_vloadop vloadop in
      let (a, o) = decompose_memop memop in
      let a' = dec_align a (Int64.to_int n / 8) in

      Some [ vloadop; compose_memop a' o; ]
    | v -> failwith ("Invalid vec load op: " ^ Print.string_of_value (listV_of_list v))
    )
| "VSTORE" ->
  (match args with
    | [ memop; ] ->
      let (a, o) = decompose_memop memop in
      let a' = dec_align a (128 / 8) in

      Some [ compose_memop a' o; ]
    | v -> failwith ("Invalid vec store op: " ^ Print.string_of_value (listV_of_list v))
    )
  | "VLOAD_LANE" | "VSTORE_LANE" ->
    (match args with
    | [ NumV n; memop; _ ] ->
      let (a, o) = decompose_memop memop in
      let a' = dec_align a (Int64.to_int n / 8) in

      Some [ NumV n; compose_memop a' o; numV (Random.int64 (Int64.div 128L n)) ]
    | v -> failwith ("Invalid vec load/store lane op: " ^ Print.string_of_value (listV_of_list v))
    )
  | "VUNOP" | "VBINOP" | "VRELOP"->
    let op = List.hd (List.tl args) in
    (match op with
    | CaseV ("_VI", [CaseV (opname, _)]) ->
      (* popcnt *)
      (match opname with
      | "POPCNT" -> Some [ make_ishape 8; op ]
      (* q15mulr_sat_s *)
      | "Q15MULR_SAT_S" -> Some [ make_ishape 16; op ]
      (* visatbinop & avgr_u *)
      | "ADD_SAT" | "SUB_SAT" | "AVGR_U" -> Some [ make_ishape (choose [8; 16]); op ]
      (* minmaxop *)
      | "MIN" | "MAX" -> Some [ make_ishape (choose [8; 16; 32]); op ]
      (* mul *)
      | "MUL" -> Some [ make_ishape (choose [16; 32; 64]); op ]
      (* lt_s, gt_s, le_s, ge_s *)
      | "LT" | "GT" | "LE" | "GE" ->
        let i = choose [8; 16; 32; 64] in
        Some [ make_ishape i; caseV ("_VI", [caseV (opname, [nullary "S"])]) ]
      | _ -> Some [ make_ishape (choose [8; 16; 32; 64]); op ]
      )
    | CaseV ("_VF", _) -> Some [ make_fshape (choose [32; 64]); op ]
    | _ -> failwith "invalid op"
    )
  (* special vbinop *)
  | "VSWIZZLE" -> Some [ make_ishape 8; ]
  | "VSHUFFLE" -> Some [ make_ishape 8; listV_of_list (List.init 16 (fun _ -> numV_of_int (Random.int 32))) ]
  (* dynamic typing *)
  | "VSPLAT" ->
    (match List.hd rt1 with
    | T (CaseV ("I32", [])) ->
      let i = choose [8; 16; 32] in
      Some [ make_ishape i ]
    | T (CaseV ("I64", [])) ->
      Some [ make_ishape 64 ]
    | T (CaseV ("F32", [])) ->
      Some [ make_fshape 32 ]
    | T (CaseV ("F64", [])) ->
      Some [ make_fshape 64 ]
    | _ -> None
    )
  | "VEXTRACT_LANE" ->
    let ext = choose [nullary "S"; nullary "U"] in
    (match List.hd rt2 with
    | T (CaseV ("I32", [])) ->
      let i = choose [8; 16; 32] in
      if i = 32 then
        Some [ make_ishape 32; optV None; numV_of_int (Random.int (128/32)) ]
      else
        Some [ make_ishape i; OptV (Some ext); numV_of_int (Random.int (128/i)) ]
    | T (CaseV ("I64", [])) ->
      Some [ make_ishape 64; OptV None; numV_of_int (Random.int (128/64)) ]
    | T (CaseV ("F32", [])) ->
      Some [ make_fshape 32; OptV None; numV_of_int (Random.int (128/32)) ]
    | T (CaseV ("F64", [])) ->
      Some [ make_fshape 64; OptV None; numV_of_int (Random.int (128/64)) ]
    | _ -> None
    )
  | "VREPLACE_LANE" ->
    (match List.hd (List.tl rt1) with
    | T (CaseV ("I32", [])) ->
      let i = choose [8; 16; 32] in
      Some [ make_ishape i; numV_of_int (Random.int (128/i)) ]
    | T (CaseV ("I64", [])) ->
      Some [ make_ishape 64; numV_of_int (Random.int (128/64)) ]
    | T (CaseV ("F32", [])) ->
      Some [ make_fshape 32; numV_of_int (Random.int (128/32)) ]
    | T (CaseV ("F64", [])) ->
      Some [ make_fshape 64; numV_of_int (Random.int (128/64)) ]
    | _ -> None
    )
  | "VDOT" -> Some [ make_ishape 32; make_ishape 16; nullary "S" ]
  | "VBITMASK" | "VALL_TRUE" -> Some [ make_ishape (choose [8; 16; 32; 64]) ]
  | "VNARROW" ->
    let i = choose [8; 16] in
    let arg1 = make_ishape i in
    let arg2 = make_ishape (i*2) in
    let ext = choose [nullary "S"; nullary "U"] in
    Some [ arg1; arg2; ext ]
  (* cvtop *)
  | "VCVTOP" ->
    (match casev_of_case (List.hd (List.tl args)) with
    | "EXTEND" ->
      let i = choose [16; 32; 64] in
      let arg1 = make_ishape i in
      let arg2 = make_ishape (i/2) in
      let half = choose [nullary "HIGH"; nullary "LOW"] in
      let ext = choose [nullary "S"; nullary "U"] in
      Some ([arg1; nullary "EXTEND"; optV (Some half); arg2; optV (Some ext); none "ZERO"])
    | "TRUNC_SAT" ->
      let i = choose [32; 64] in
      let arg1 = make_ishape 32 in
      let arg2 = make_fshape i in
      let ext = choose [nullary "S"; nullary "U"] in
      let zero = if i = 32 then none "ZERO" else some "ZERO" in
      Some ([arg1; nullary "TRUNC_SAT"; optV None; arg2; optV (Some ext); zero])
    | "CONVERT" ->
      let i = choose [32; 64] in
      let arg1 = make_fshape i in
      let arg2 = make_ishape 32 in
      let half = if i = 32 then None else Some (nullary "LOW") in
      let ext = choose [nullary "S"; nullary "U"] in
      Some ([arg1; nullary "CONVERT"; optV half; arg2; optV (Some ext); none "ZERO"])
    | "DEMOTE" ->
      let arg1 = make_fshape 32 in
      let arg2 = make_fshape 64 in
      Some ([arg1; nullary "DEMOTE"; optV None; arg2; optV None; some "ZERO"])
    | "PROMOTE" ->
      let arg1 = make_fshape 64 in
      let arg2 = make_fshape 32 in
      Some ([arg1; nullary "PROMOTE"; optV (Some (nullary "LOW")); arg2; optV None; none "ZERO"])
    | _ -> Some args
    )
  | "VISHIFTOP" ->
    let i = choose [8; 16; 32; 64] in
    Some [ make_ishape i; List.hd (List.rev args) ]
  | "VEXTMUL" ->
    let i = choose [16; 32; 64] in
    let arg1 = make_ishape i in
    let arg2 = make_ishape (i/2) in
    let hl = choose [nullary "HIGH"; nullary "LOW"] in
    let ext = choose [nullary "S"; nullary "U"] in
    Some [ arg1; hl; arg2; ext ]
  (* special vcvtop *)
  | "VEXTADD_PAIRWISE" ->
    let i = choose [16; 32] in
    let arg1 = make_ishape i in
    let arg2 = make_ishape (i/2) in
    let ext = choose [nullary "S"; nullary "U"] in
    Some [ arg1; arg2; ext ]
  | _ -> Some args

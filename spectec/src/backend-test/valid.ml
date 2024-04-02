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
let v128T = nullary "V128"

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

let validate_vloadop vloadop =
  let mk_vloadop name args = OptV (Some (CaseV (name, args))) in
  match vloadop with
  | OptV (Some (CaseV ("ZERO", _))) ->
    let n = choose [32; 64] in
    mk_vloadop "ZERO" [ numV_of_int n ], n
  | OptV (Some (CaseV ("SPLAT", _))) ->
    let n = choose [8; 16; 32; 64] in
    mk_vloadop "SPLAT" [ numV_of_int n ], n
  | OptV (Some (CaseV ("SHAPE", [_; _; sx]))) ->
    let n1 = choose [8; 16; 32] in
    let n2 = 64 / n1 in
    mk_vloadop "SHAPE" [ numV_of_int n1; numV_of_int n2; sx ], 64
  | OptV None -> OptV None, 128
  | v -> failwith ("Invalid vloadop: " ^ Print.string_of_value v)

let decompose_memop s = (
    s |> unwrap_strv |> Record.find "ALIGN" |> unwrap_numv_to_int,
    s |> unwrap_strv |> Record.find "OFFSET" |> unwrap_numv_to_int
  )
let compose_memop a o = StrV (Record.empty |> Record.add "ALIGN" (numV_of_int a) |> Record.add "OFFSET" (numV_of_int o))

let rec dec_align a n =
  if 1 lsl a <= n then a else
  dec_align (a-1) n

let get_vunop_shape vop =
  let name = casev_get_case vop in
  match name with
  | "ABS"
  | "NEG"    -> make_ishape 8 // make_fshape (32 // 64)
  | "POPCNT" -> make_ishape 8
  | _        -> make_fshape (32 // 64)
let get_vbinop_shape orig_shape vop =
  let name = casev_get_case vop in
  let has_arg = List.length (casev_get_args vop) > 0 in
  match name with
  | "ADD"
  | "SUB"           -> orig_shape
  | "MUL"           -> make_ishape (choose [16; 32; 64]) // make_fshape (32 // 64)
  | "ADD_SAT"
  | "SUB_SAT"
  | "AVGR_U"        -> make_ishape (8 // 16)
  | "Q15MULR_SAT_S" -> make_ishape 16
  | ("MIN" | "MAX")
    when has_arg    -> make_ishape (choose [8; 16; 32])
  | _               -> make_fshape (32 // 64)
let get_vrelop_shape orig_shape vop =
  if List.length (casev_get_args vop) > 0 then
    match casev_nth_arg 0 vop |> casev_get_case with
    | "S" -> make_ishape (choose [8; 16; 32; 64])
    | _   -> make_ishape (choose [8; 16; 32])
  else
    match casev_get_case vop with
    | "EQ" | "NE" -> orig_shape
    | _           -> make_fshape (32 // 64)

let is_inn nt = nt = i32T || nt = i64T
let is_fnn nt = nt = f32T || nt = f64T
let is_vnn nt = nt = v128T

let default = function
| T (CaseV ("I32", [])) -> caseV ("CONST", [nullary "I32"; zero])
| T (CaseV ("I64", [])) -> caseV ("CONST", [nullary "I64"; zero])
| T (CaseV ("F32", [])) -> caseV ("CONST", [nullary "F32"; fzero])
| T (CaseV ("F64", [])) -> caseV ("CONST", [nullary "F64"; fzero])
| T (CaseV ("V128", [])) -> caseV ("VCONST", [nullary "V128"; zero])
| T (CaseV (("FUNCREF" | "EXTERNREF"), []) as rt) -> caseV ("REF.NULL", [rt])
| v -> failwith ("Default value for " ^ (string_of_vt v) ^ " is not implented")

(** Estimate if the given module is valid **)

(* Check if cvtop is syntactically correct *)
let correct_cvtop = function
| [ CaseV (t2, []); CaseV (cvtop, []); CaseV (t1, []); OptV (sx_opt) ] when t1 <> t2 ->
  let x2 = String.sub t2 0 1 in
  let n2 = String.sub t2 1 2 in
  let x1 = String.sub t1 0 1 in
  let n1 = String.sub t1 1 2 in
  let sx = Option.is_some sx_opt in
  ( match cvtop with
  | "CONVERT" ->
    if x1 <> x2 then sx else
    sx = (t1 = "I32")
  | "CONVERT_SAT" -> (
    match (x1, x2) with
    | "F", "I" -> sx
    | _ -> false )
  | "REINTERPRET" -> n1 = n2 && not sx
  | _ -> failwith "Unreachable" )
| _ -> false

let validate_if_extend = function
| [ CaseV ("I32", []) as nt; CaseV ("EXTEND", _) ] -> [ nt; CaseV ("EXTEND", [ numV_of_int (choose ([8; 16])) ]) ];
| [ CaseV ("I64", []) as nt; CaseV ("EXTEND", _) ] -> [ nt; CaseV ("EXTEND", [ numV_of_int (choose ([8; 16; 32])) ]) ];
| args -> args

let validate_shape = function
  | TupV [ CaseV ("I8", []); NumV _ ] -> tupV [ caseV ("I8", []); numV_of_int 16 ]
  | TupV [ CaseV ("I16", []); NumV _ ] -> tupV [ caseV ("I16", []); numV_of_int 8 ]
  | TupV [ CaseV ("I32", []); NumV _ ] -> tupV [ caseV ("I32", []); numV_of_int 4 ]
  | TupV [ CaseV ("I64", []); NumV _ ] -> tupV [ caseV ("I64", []); numV_of_int 2 ]
  | TupV [ CaseV ("F32", []); NumV _ ] -> tupV [ caseV ("F32", []); numV_of_int 4 ]
  | TupV [ CaseV ("F64", []); NumV _ ] -> tupV [ caseV ("F64", []); numV_of_int 2 ]
  | v -> failwith ("Invalid shape: " ^ Print.string_of_value v)

(* Estimate if given instruction is valid with expected type, rt1* -> rt2* *)
(* TODO: Perhaps some of these can be automated? *)
let validate_instr case args const (rt1, rt2) =
  match case with
  | "UNOP" | "BINOP" | "TESTOP" | "RELOP" -> ( match rt2 with
    | [ T t ] when (not const || is_inn t) ->
      let args' = validate_if_extend args in
      Some args'
    | _ -> None )
  | "CVTOP" -> if correct_cvtop args then Some args else None
  | "CALL_REF" | "RETURN_CALL_REF" -> ( match args with
    | [ OptV (Some _) ] -> Some args
    | _ -> None )
  (* special vbinop *)
  | "VSWIZZLE" -> Some [ make_ishape 8; ]
  | "VSHUFFLE" -> Some [ make_ishape 8; listV_of_list (List.init 16 (fun _ -> numV_of_int (Random.int 32))) ]
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
      | OptV (Some (TupV [ m; s ])) -> true, unwrap_numv_to_int m, casev_get_case s
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
    | [ OptV None ], [ T t ] -> if is_inn t || is_fnn t || is_vnn t then Some args else None
    | [ OptV _ ], [ T t ] -> Some [ OptV (Some (singleton t)) ]
    | _ -> None )
  | "VLOAD" -> ( match args with
    | [ vloadop; memop; ] ->
      let vloadop', n = validate_vloadop vloadop in
      let (a, o) = decompose_memop memop in
      let a' = dec_align a (n / 8) in

      Some [ vloadop'; compose_memop a' o; ]
    | v -> failwith ("Invalid vec load op: " ^ Print.string_of_value (listV_of_list v))
    )
  | "VSTORE" -> ( match args with
      | [ memop; ] ->
        let (a, o) = decompose_memop memop in
        let a' = dec_align a (128 / 8) in

        Some [ compose_memop a' o; ]
      | v -> failwith ("Invalid vec store op: " ^ Print.string_of_value (listV_of_list v))
      )
  | "VLOAD_LANE" | "VSTORE_LANE" ->
    (match args with
    | [ n; memop; _ ] ->
      let i = unwrap_numv_to_int n in
      let (a, o) = decompose_memop memop in
      let a' = dec_align a (i / 8) in
      let j = Random.int (128 / i) in

      Some [ n; compose_memop a' o; numV_of_int j ]
    | v -> failwith ("Invalid vec load/store lane op: " ^ Print.string_of_value (listV_of_list v))
    )
  | "VUNOP"   -> let op = List.nth args 1 in Some [ get_vunop_shape op; op ]
  | "VBINOP"  -> let op = List.nth args 1 in Some [ get_vbinop_shape (List.hd args) op; op ]
  | "VTESTOP" -> let op = List.nth args 1 in Some [ make_ishape (choose [8; 16; 32; 64]); op ]
  | "VRELOP"  -> let op = List.nth args 1 in Some [ get_vrelop_shape (List.hd args) op; op ]
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
  | "VNARROW" ->
    let i = choose [8; 16] in
    let arg1 = make_ishape i in
    let arg2 = make_ishape (i*2) in
    let ext = choose [nullary "S"; nullary "U"] in
    Some [ arg1; arg2; ext ]
  (* cvtop *)
  | "VCVTOP" ->
    (match casev_get_case (List.nth args 1) with
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
  | "VEXTUNOP" ->
    let i = 16 // 32 in
    let arg1 = make_ishape i in
    let arg2 = make_ishape (i/2) in
    let arg3 = List.nth args 2 in
    let arg4 = List.nth args 3 in
    Some [ arg1; arg2; arg3; arg4 ]
  | "VEXTBINOP" ->
    let is_dot = List.nth args 2 |> casev_get_case = "DOT" in
    let i = if is_dot then 32 else choose [16; 32; 64] in
    let arg1 = make_ishape i in
    let arg2 = make_ishape (i/2) in
    let arg3 = List.nth args 2 in
    let arg4 = if is_dot then nullary "S" else List.nth args 3 in
    Some [ arg1; arg2; arg3; arg4 ]
  (* HACKS *)
  | "CONST" -> (
    match args with
    | [ ty; _ ] when is_inn ty ->
      let mask =
        ty
        |> get_bitwidth
        |> Z.shift_left Z.one
        |> Z.pred
      in

      let n =
        interesting_value_map
        |> Interesting.find (get_bitwidth ty)
        |> IntSet.elements
        |> choose
        |> Z.logand mask
        |> numV
      in

      Some [ ty; n ]
    | [ ty; CaseV (("POS"|"NEG"), _) ] when is_fnn ty -> Some args
    | _ -> None
  )
  | _ -> Some args

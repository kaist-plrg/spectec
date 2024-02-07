open Reference_interpreter2
open Ast
open Types
open Values
open Al.Ast
open Al.Al_util
open Source
open Util

(* Constant *)

let default_table_max = 4294967295L
let default_memory_max = 65536L
let version = ref 3


(* Failure *)

let fail ty v =
  Al.Print.string_of_value v
  |> Printf.sprintf "Invalid %s: %s" ty
  |> failwith

let fail_list ty l = listV_of_list l |> fail ty


(* Destruct *)

(* Destruct data structure *)

let al_to_opt (f: value -> 'a) (v: value): 'a option = unwrap_optv v |> Option.map f
let al_to_list (f: value -> 'a) (v: value): 'a list =
  unwrap_listv v |> (!) |> Array.to_list |> List.map f
let al_to_seq f s = al_to_list f s |> List.to_seq
let al_to_phrase (f: value -> 'a) (v: value): 'a phrase = f v @@ no_region


(* Destruct minor *)

let al_to_int64: value -> int64 = unwrap_numv
let al_to_int (v: value): int = al_to_int64 v |> Int64.to_int
let al_to_int32 (v: value): int32 = al_to_int64 v |> Int64.to_int32
let al_to_float32 (v: value): F32.t = al_to_int32 v |> F32.of_bits
let al_to_float64 (v: value): F64.t = al_to_int64 v |> F64.of_bits
let al_to_var: value -> var = al_to_phrase al_to_int32
let al_to_byte (v: value): Char.t = al_to_int v |> Char.chr
let al_to_bytes (v: value): string = al_to_seq al_to_byte v |> String.of_seq
let al_to_string = function
  | TextV str -> str
  | v -> fail "text" v
let al_to_name name = name |> al_to_string |> Utf8.decode
let al_to_vector = unwrap_vecv
let al_to_bool = unwrap_boolv


(* Destruct type *)

let al_to_mut: value -> mutability = function
  | CaseV ("MUT", [ OptV None ]) -> Immutable
  | CaseV ("MUT", [ OptV _ ]) -> Mutable
  | v -> fail "mut" v

let rec al_to_result_type (v: value): result_type = al_to_list al_to_val_type v

and al_to_ref_type: value -> ref_type = function
  | CaseV ("FUNCREF", []) -> FuncRefType
  | CaseV ("EXTERNREF", []) -> ExternRefType
  | v -> fail "ref type" v

and al_to_num_type: value -> num_type = function
  | CaseV ("I32", []) -> I32Type
  | CaseV ("I64", []) -> I64Type
  | CaseV ("F32", []) -> F32Type
  | CaseV ("F64", []) -> F64Type
  | v -> fail "num type" v

and al_to_val_type: value -> value_type = function
  | CaseV ("I32", _) | CaseV ("I64", _)
  | CaseV ("F32", _) | CaseV ("F64", _) as v -> NumType (al_to_num_type v)
  | CaseV ("V128", []) -> VecType V128Type
  | CaseV (reftype, _) as v when String.ends_with ~suffix:"REF" reftype -> RefType (al_to_ref_type v)
  | v -> fail "val type" v

let al_to_block_type: value -> block_type = function
  | CaseV ("_IDX", [ var ]) -> VarBlockType (al_to_var var)
  | CaseV ("_RESULT", [ vt_opt ]) -> ValBlockType (al_to_opt al_to_val_type vt_opt)
  | v -> fail "block type" v

let al_to_limits (default: int64): value -> int32 limits = function
  | TupV [ min; max ] ->
    let max' =
      match al_to_int64 max with
      | i64 when default = i64 -> None
      | _ -> Some (al_to_int32 max)
    in
    { min = al_to_int32 min; max = max' }
  | v -> fail "limits" v

let al_to_func_type: value -> func_type = function
  | TupV [ rt1; rt2 ] -> FuncType (al_to_result_type rt1, al_to_result_type rt2)
  | v -> fail "function type" v

let al_to_type: value -> type_ = function
  | CaseV ("TYPE", [ft]) -> al_to_phrase al_to_func_type ft
  | v -> fail "type" v

let al_to_global_type: value -> global_type = function
  | TupV [ mut; vt ] -> GlobalType (al_to_val_type vt, al_to_mut mut)
  | v -> fail "global type" v

let al_to_table_type: value -> table_type = function
  | TupV [ limits; rt ] -> TableType (al_to_limits default_table_max limits, al_to_ref_type rt)
  | v -> fail "table type" v

let al_to_memory_type: value -> memory_type = function
  | CaseV ("I8", [ limits ]) -> MemoryType (al_to_limits default_memory_max limits)
  | v -> fail "memory type" v


(* Destruct value *)

let rec al_to_num: value -> num = function
  | CaseV ("CONST", [ CaseV ("I32", []); i32 ]) -> I32 (al_to_int32 i32)
  | CaseV ("CONST", [ CaseV ("I64", []); i64 ]) -> I64 (al_to_int64 i64)
  | CaseV ("CONST", [ CaseV ("F32", []); f32 ]) -> F32 (al_to_float32 f32)
  | CaseV ("CONST", [ CaseV ("F64", []); f64 ]) -> F64 (al_to_float64 f64)
  | v -> fail "num" v

and al_to_vec: value -> vec = function
  | CaseV ("VVCONST", [ CaseV ("V128", []); VecV (v128)]) -> V128 (V128.of_bits v128)
  | v -> fail "vec" v

and al_to_ref: value -> ref_ = function
  | CaseV ("REF.NULL", [ rt ]) -> NullRef (al_to_ref_type rt)
  (*| CaseV ("REF.HOST_ADDR", [ i32 ]) -> Script.HostRef (al_to_int32 i32)*)
  | CaseV ("REF.FUNC_ADDR", [ a ]) -> Script.ExternRef (al_to_int32 a)
  | v -> fail "ref" v

and al_to_value: value -> Values.value = function
  | CaseV ("CONST", _) as v -> Num (al_to_num v)
  | CaseV (ref_, _) as v when String.sub ref_ 0 4 = "REF." -> Ref (al_to_ref v)
  | CaseV ("VVCONST", _) as v -> Vec (al_to_vec v)
  | v -> fail "value" v


(* Destruct operator *)

let al_to_op f1 f2 = function
  | [ CaseV ("I32", []); CaseV ("_I", [op]) ] -> I32 (f1 op)
  | [ CaseV ("I64", []); CaseV ("_I", [op]) ] -> I64 (f1 op)
  | [ CaseV ("F32", []); CaseV ("_F", [op]) ] -> F32 (f2 op)
  | [ CaseV ("F64", []); CaseV ("_F", [op]) ] -> F64 (f2 op)
  | l -> fail_list "op" l

let al_to_int_unop: value -> IntOp.unop = function
  | CaseV ("CLZ", []) -> IntOp.Clz
  | CaseV ("CTZ", []) -> IntOp.Ctz
  | CaseV ("POPCNT", []) -> IntOp.Popcnt
  | CaseV ("EXTEND8S", []) -> IntOp.ExtendS Types.Pack8
  | CaseV ("EXTEND16S", []) -> IntOp.ExtendS Types.Pack16
  | CaseV ("EXTEND32S", []) -> IntOp.ExtendS Types.Pack32
  | CaseV ("EXTEND64S", []) -> IntOp.ExtendS Types.Pack64
  | v -> fail "integer unop" v
let al_to_float_unop: value -> FloatOp.unop = function
  | CaseV ("NEG", []) -> FloatOp.Neg
  | CaseV ("ABS", []) -> FloatOp.Abs
  | CaseV ("CEIL", []) -> FloatOp.Ceil
  | CaseV ("FLOOR", []) -> FloatOp.Floor
  | CaseV ("TRUNC", []) -> FloatOp.Trunc
  | CaseV ("NEAREST", []) -> FloatOp.Nearest
  | CaseV ("SQRT", []) -> FloatOp.Sqrt
  | v -> fail "float unop" v
let al_to_unop: value list -> Ast.unop = al_to_op al_to_int_unop al_to_float_unop

let al_to_int_binop: value -> IntOp.binop = function
  | CaseV ("ADD", []) -> IntOp.Add
  | CaseV ("SUB", []) -> IntOp.Sub
  | CaseV ("MUL", []) -> IntOp.Mul
  | CaseV ("DIV", [CaseV ("S", [])]) -> IntOp.DivS
  | CaseV ("DIV", [CaseV ("U", [])]) -> IntOp.DivU
  | CaseV ("REM", [CaseV ("S", [])]) -> IntOp.RemS
  | CaseV ("REM", [CaseV ("U", [])]) -> IntOp.RemU
  | CaseV ("AND", []) -> IntOp.And
  | CaseV ("OR", []) -> IntOp.Or
  | CaseV ("XOR", []) -> IntOp.Xor
  | CaseV ("SHL", []) -> IntOp.Shl
  | CaseV ("SHR", [CaseV ("S", [])]) -> IntOp.ShrS
  | CaseV ("SHR", [CaseV ("U", [])]) -> IntOp.ShrU
  | CaseV ("ROTL", []) -> IntOp.Rotl
  | CaseV ("ROTR", []) -> IntOp.Rotr
  | v -> fail "integer binop" v
let al_to_float_binop: value -> FloatOp.binop = function
  | CaseV ("ADD", []) -> FloatOp.Add
  | CaseV ("SUB", []) -> FloatOp.Sub
  | CaseV ("MUL", []) -> FloatOp.Mul
  | CaseV ("DIV", []) -> FloatOp.Div
  | CaseV ("MIN", []) -> FloatOp.Min
  | CaseV ("MAX", []) -> FloatOp.Max
  | CaseV ("COPYSIGN", []) -> FloatOp.CopySign
  | v -> fail "float binop" v
let al_to_binop: value list -> Ast.binop = al_to_op al_to_int_binop al_to_float_binop

let al_to_int_testop: value -> IntOp.testop = function
  | CaseV ("EQZ", []) -> IntOp.Eqz
  | v -> fail "integer testop" v
let al_to_float_testop: value -> FloatOp.testop = function
  | v -> fail "float testop" v
let al_to_testop: value list -> Ast.testop = al_to_op al_to_int_testop al_to_float_testop

let al_to_int_relop: value -> IntOp.relop = function
  | CaseV ("EQ", []) -> IntOp.Eq
  | CaseV ("NE", []) -> IntOp.Ne
  | CaseV ("LT", [CaseV ("S", [])]) -> IntOp.LtS
  | CaseV ("LT", [CaseV ("U", [])]) -> IntOp.LtU
  | CaseV ("GT", [CaseV ("S", [])]) -> IntOp.GtS
  | CaseV ("GT", [CaseV ("U", [])]) -> IntOp.GtU
  | CaseV ("LE", [CaseV ("S", [])]) -> IntOp.LeS
  | CaseV ("LE", [CaseV ("U", [])]) -> IntOp.LeU
  | CaseV ("GE", [CaseV ("S", [])]) -> IntOp.GeS
  | CaseV ("GE", [CaseV ("U", [])]) -> IntOp.GeU
  | v -> fail "integer relop" v
let al_to_float_relop: value -> FloatOp.relop = function
  | CaseV ("EQ", []) -> FloatOp.Eq
  | CaseV ("NE", []) -> FloatOp.Ne
  | CaseV ("LT", []) -> FloatOp.Lt
  | CaseV ("GT", []) -> FloatOp.Gt
  | CaseV ("LE", []) -> FloatOp.Le
  | CaseV ("GE", []) -> FloatOp.Ge
  | v -> fail "float relop" v
let al_to_relop: value list -> relop = al_to_op al_to_int_relop al_to_float_relop

let al_to_pack_size : value -> Types.pack_size = function
  | NumV 8L -> Types.Pack8
  | NumV 16L -> Types.Pack16
  | NumV 32L -> Types.Pack32
  | NumV 64L -> Types.Pack64
  | v -> fail "pack_size" v

let al_to_extendop = function
  | [ CaseV ("I32", []); n ] -> I32 (IntOp.ExtendS (al_to_pack_size n))
  | [ CaseV ("I64", []); n ] -> I64 (IntOp.ExtendS (al_to_pack_size n))
  | l -> fail_list "extendop" l

let al_to_int_cvtop: value list -> IntOp.cvtop = function
  | CaseV ("EXTEND", []) :: args ->
    (match args with
    | [ CaseV ("I32", []); OptV (Some (CaseV ("S", []))) ] -> IntOp.ExtendSI32
    | [ CaseV ("I32", []); OptV (Some (CaseV ("U", []))) ] -> IntOp.ExtendUI32
    | l -> fail_list "extend" l)
    | [ CaseV ("Wrap", []); CaseV ("I64", []); OptV None ] -> IntOp.WrapI64
  | CaseV ("TRUNC", []) :: args ->
    (match args with
    | [ CaseV ("F32", []); OptV (Some (CaseV ("S", []))) ] -> IntOp.TruncSF32
    | [ CaseV ("F32", []); OptV (Some (CaseV ("U", []))) ] -> IntOp.TruncUF32
    | [ CaseV ("F64", []); OptV (Some (CaseV ("S", []))) ] -> IntOp.TruncSF64
    | [ CaseV ("F64", []); OptV (Some (CaseV ("U", []))) ] -> IntOp.TruncUF64
    | l -> fail_list "trunc" l)
  | CaseV ("TRUNCSAT", []) :: args ->
    (match args with
    | [ CaseV ("F32", []); OptV (Some (CaseV ("S", []))) ] -> IntOp.TruncSatSF32
    | [ CaseV ("F32", []); OptV (Some (CaseV ("U", []))) ] -> IntOp.TruncSatUF32
    | [ CaseV ("F64", []); OptV (Some (CaseV ("S", []))) ] -> IntOp.TruncSatSF64
    | [ CaseV ("F64", []); OptV (Some (CaseV ("U", []))) ] -> IntOp.TruncSatUF64
    | l -> fail_list "truncsat" l)
  | [ CaseV ("REINTERPRET", []); _; OptV None ] -> IntOp.ReinterpretFloat
  | l -> fail_list "integer cvtop" l
let al_to_float_cvtop : value list -> FloatOp.cvtop = function
  | CaseV ("CONVERT", []) :: args ->
    (match args with
    | [ CaseV ("I32", []); OptV (Some (CaseV (("S", [])))) ] -> FloatOp.ConvertSI32
    | [ CaseV ("I32", []); OptV (Some (CaseV (("U", [])))) ] -> FloatOp.ConvertUI32
    | [ CaseV ("I64", []); OptV (Some (CaseV (("S", [])))) ] -> FloatOp.ConvertSI64
    | [ CaseV ("I64", []); OptV (Some (CaseV (("U", [])))) ] -> FloatOp.ConvertUI64
    | l -> fail_list "convert" l)
  | [ CaseV ("PROMOTE", []); CaseV ("F32", []); OptV None ] -> FloatOp.PromoteF32
  | [ CaseV ("DEMOTE", []); CaseV ("F64", []); OptV None ] -> FloatOp.DemoteF64
  | [ CaseV ("REINTERPRET", []); _; OptV None ] -> FloatOp.ReinterpretInt
  | l -> fail_list "float cvtop" l
let al_to_cvtop: value list -> cvtop = function
  | CaseV ("I32", []) :: op -> I32 (al_to_int_cvtop op)
  | CaseV ("I64", []) :: op -> I64 (al_to_int_cvtop op)
  | CaseV ("F32", []) :: op -> F32 (al_to_float_cvtop op)
  | CaseV ("F64", []) :: op -> F64 (al_to_float_cvtop op)
  | l -> fail_list "cvtop" l

(* Vector operator *)

let al_to_extension : value -> Types.extension = function
  | CaseV ("S", []) -> Types.SX
  | CaseV ("U", []) -> Types.ZX
  | v -> fail "extension" v

let al_to_vop f1 f2 = function
  | [ TupV [ CaseV ("I8", []); NumV 16L ]; CaseV ("_VI", [vop]) ] -> V128 (V128.I8x16 (f1 vop))
  | [ TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("_VI", [vop]) ] -> V128 (V128.I16x8 (f1 vop))
  | [ TupV [ CaseV ("I32", []); NumV 4L ]; CaseV ("_VI", [vop]) ] -> V128 (V128.I32x4 (f1 vop))
  | [ TupV [ CaseV ("I64", []); NumV 2L ]; CaseV ("_VI", [vop]) ] -> V128 (V128.I64x2 (f1 vop))
  | [ TupV [ CaseV ("F32", []); NumV 4L ]; CaseV ("_VF", [vop]) ] -> V128 (V128.F32x4 (f2 vop))
  | [ TupV [ CaseV ("F64", []); NumV 2L ]; CaseV ("_VF", [vop]) ] -> V128 (V128.F64x2 (f2 vop))
  | l -> fail_list "vop" l

let al_to_vvop f = function
  | [ CaseV ("V128", []); CaseV ("_VV", [ vop ]) ] -> V128 (f vop)
  | l -> fail_list "vvop" l

let al_to_vtestop : value list -> vec_testop = function
  | [ TupV [ CaseV ("I8", []); NumV 16L ] ] -> V128 (V128.I8x16 (V128Op.AllTrue))
  | [ TupV [ CaseV ("I16", []); NumV 8L ] ] -> V128 (V128.I16x8 (V128Op.AllTrue))
  | [ TupV [ CaseV ("I32", []); NumV 4L ] ] -> V128 (V128.I32x4 (V128Op.AllTrue))
  | [ TupV [ CaseV ("I64", []); NumV 2L ] ] -> V128 (V128.I64x2 (V128Op.AllTrue))
  | l -> fail_list "vtestop" l

let al_to_vbitmaskop : value list -> vec_bitmaskop = function
  | [ TupV [ CaseV ("I8", []); NumV 16L ] ] -> V128 (V128.I8x16 (V128Op.Bitmask))
  | [ TupV [ CaseV ("I16", []); NumV 8L ] ] -> V128 (V128.I16x8 (V128Op.Bitmask))
  | [ TupV [ CaseV ("I32", []); NumV 4L ] ] -> V128 (V128.I32x4 (V128Op.Bitmask))
  | [ TupV [ CaseV ("I64", []); NumV 2L ] ] -> V128 (V128.I64x2 (V128Op.Bitmask))
  | l -> fail_list "vbitmaskop" l

let al_to_int_vrelop : value -> V128Op.irelop = function
  | CaseV ("EQ", []) -> V128Op.Eq
  | CaseV ("NE", []) -> V128Op.Ne
  | CaseV ("LT", [CaseV ("S", [])]) -> V128Op.LtS
  | CaseV ("LT", [CaseV ("U", [])]) -> V128Op.LtU
  | CaseV ("LE", [CaseV ("S", [])]) -> V128Op.LeS
  | CaseV ("LE", [CaseV ("U", [])]) -> V128Op.LeU
  | CaseV ("GT", [CaseV ("S", [])]) -> V128Op.GtS
  | CaseV ("GT", [CaseV ("U", [])]) -> V128Op.GtU
  | CaseV ("GE", [CaseV ("S", [])]) -> V128Op.GeS
  | CaseV ("GE", [CaseV ("U", [])]) -> V128Op.GeU
  | v -> fail "integer vrelop" v

let al_to_float_vrelop : value -> V128Op.frelop = function
  | CaseV ("EQ", []) -> V128Op.Eq
  | CaseV ("NE", []) -> V128Op.Ne
  | CaseV ("LT", []) -> V128Op.Lt
  | CaseV ("LE", []) -> V128Op.Le
  | CaseV ("GT", []) -> V128Op.Gt
  | CaseV ("GE", []) -> V128Op.Ge
  | v -> fail "float vrelop" v

let al_to_vrelop : value list -> vec_relop =
  al_to_vop al_to_int_vrelop al_to_float_vrelop

let al_to_int_vunop : value -> V128Op.iunop = function
  | CaseV ("ABS", []) -> V128Op.Abs
  | CaseV ("NEG", []) -> V128Op.Neg
  | CaseV ("POPCNT", []) -> V128Op.Popcnt
  | v -> fail "integer vunop" v

let al_to_float_vunop : value -> V128Op.funop = function
  | CaseV ("ABS", []) -> V128Op.Abs
  | CaseV ("NEG", []) -> V128Op.Neg
  | CaseV ("SQRT", []) -> V128Op.Sqrt
  | CaseV ("CEIL", []) -> V128Op.Ceil
  | CaseV ("FLOOR", []) -> V128Op.Floor
  | CaseV ("TRUNC", []) -> V128Op.Trunc
  | CaseV ("NEAREST", []) -> V128Op.Nearest
  | v -> fail "float vunop" v

let al_to_vunop : value list -> vec_unop =
  al_to_vop al_to_int_vunop al_to_float_vunop

let al_to_int_vbinop : value -> V128Op.ibinop = function
  | CaseV ("ADD", []) -> V128Op.Add
  | CaseV ("SUB", []) -> V128Op.Sub
  | CaseV ("MUL", []) -> V128Op.Mul
  | CaseV ("MIN", [CaseV ("S", [])]) -> V128Op.MinS
  | CaseV ("MIN", [CaseV ("U", [])]) -> V128Op.MinU
  | CaseV ("MAX", [CaseV ("S", [])]) -> V128Op.MaxS
  | CaseV ("MAX", [CaseV ("U", [])]) -> V128Op.MaxU
  | CaseV ("AVGR_U", []) -> V128Op.AvgrU
  | CaseV ("ADD_SAT", [CaseV ("S", [])]) -> V128Op.AddSatS
  | CaseV ("ADD_SAT", [CaseV ("U", [])]) -> V128Op.AddSatU
  | CaseV ("SUB_SAT", [CaseV ("S", [])]) -> V128Op.SubSatS
  | CaseV ("SUB_SAT", [CaseV ("U", [])]) -> V128Op.SubSatU
  | CaseV ("DOTS", []) -> V128Op.DotS
  | CaseV ("Q15MULR_SAT_S", []) -> V128Op.Q15MulRSatS
  | CaseV ("SWIZZLE", []) -> V128Op.Swizzle
  (*TODO *)
  | CaseV ("Shuffle", [ l ]) -> V128Op.Shuffle (al_to_list al_to_int l)
  | v -> fail "integer vbinop" v

let al_to_float_vbinop : value -> V128Op.fbinop = function
  | CaseV ("ADD", []) -> V128Op.Add
  | CaseV ("SUB", []) -> V128Op.Sub
  | CaseV ("MUL", []) -> V128Op.Mul
  | CaseV ("DIV", []) -> V128Op.Div
  | CaseV ("MIN", []) -> V128Op.Min
  | CaseV ("MAX", []) -> V128Op.Max
  | CaseV ("PMIN", []) -> V128Op.Pmin
  | CaseV ("PMAX", []) -> V128Op.Pmax
  | v -> fail "float vbinop" v

let al_to_vbinop : value list -> vec_binop = al_to_vop al_to_int_vbinop al_to_float_vbinop

let al_to_special_vbinop = function
  | CaseV ("VSWIZZLE", [ TupV [ CaseV ("I8", []); NumV 16L ]; ]) -> V128 (V128.I8x16 (V128Op.Swizzle))
  | CaseV ("VSHUFFLE", [ TupV [ CaseV ("I8", []); NumV 16L ]; l ]) -> V128 (V128.I8x16 (V128Op.Shuffle (al_to_list al_to_int l)))
  | CaseV ("VNARROW", [ TupV [ CaseV ("I8", []); NumV 16L ]; TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("S", []) ]) -> V128 (V128.I8x16 (V128Op.NarrowS))
  | CaseV ("VNARROW", [ TupV [ CaseV ("I16", []); NumV 8L ]; TupV [ CaseV ("I32", []); NumV 4L ]; CaseV ("S", []) ]) -> V128 (V128.I16x8 (V128Op.NarrowS))
  | CaseV ("VNARROW", [ TupV [ CaseV ("I8", []); NumV 16L ]; TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("U", []) ]) -> V128 (V128.I8x16 (V128Op.NarrowU))
  | CaseV ("VNARROW", [ TupV [ CaseV ("I16", []); NumV 8L]; TupV [ CaseV ("I32", []); NumV 4L ]; CaseV ("U", []) ]) -> V128 (V128.I16x8 (V128Op.NarrowU))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("HIGH", []); TupV [ CaseV ("I8", []); NumV 16L ]; CaseV ("S", []) ]) -> V128 (V128.I16x8 (V128Op.ExtMulHighS))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("HIGH", []); TupV [ CaseV ("I8", []); NumV 16L ]; CaseV ("U", []) ]) -> V128 (V128.I16x8 (V128Op.ExtMulHighU))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("LOW", []); TupV [ CaseV ("I8", []); NumV 16L ]; CaseV ("S", []) ]) -> V128 (V128.I16x8 (V128Op.ExtMulLowS))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("LOW", []); TupV [ CaseV ("I8", []); NumV 16L ]; CaseV ("U", []) ] ) -> V128 (V128.I16x8 (V128Op.ExtMulLowU))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I32", []); NumV 4L ]; CaseV ("HIGH", []); TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("S", []) ]) -> V128 (V128.I32x4 (V128Op.ExtMulHighS))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I32", []); NumV 4L ]; CaseV ("HIGH", []); TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("U", []) ]) -> V128 (V128.I32x4 (V128Op.ExtMulHighU))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I32", []); NumV 4L ]; CaseV ("LOW", []); TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("S", []) ]) -> V128 (V128.I32x4 (V128Op.ExtMulLowS))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I32", []); NumV 4L ]; CaseV ("LOW", []); TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("U", []) ] ) -> V128 (V128.I32x4 (V128Op.ExtMulLowU))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I64", []); NumV 2L ]; CaseV ("HIGH", []); TupV [ CaseV ("I32", []); NumV 4L ]; CaseV ("S", []) ]) -> V128 (V128.I64x2 (V128Op.ExtMulHighS))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I64", []); NumV 2L ]; CaseV ("HIGH", []); TupV [ CaseV ("I32", []); NumV 4L ]; CaseV ("U", []) ]) -> V128 (V128.I64x2 (V128Op.ExtMulHighU))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I64", []); NumV 2L ]; CaseV ("LOW", []); TupV [ CaseV ("I32", []); NumV 4L ]; CaseV ("S", []) ]) -> V128 (V128.I64x2 (V128Op.ExtMulLowS))
  | CaseV ("VEXTMUL", [ TupV [ CaseV ("I64", []); NumV 2L ]; CaseV ("LOW", []); TupV [ CaseV ("I32", []); NumV 4L ]; CaseV ("U", []) ] ) -> V128 (V128.I64x2 (V128Op.ExtMulLowU))
  | CaseV ("VDOT", [ TupV [ CaseV ("I32", []); NumV 4L]; TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("S", []) ]) -> V128 (V128.I32x4 (V128Op.DotS))
  | v -> fail "special vbinop" v


let al_to_int_vcvtop : value list -> V128Op.icvtop = function
  | [ CaseV (op, []); OptV half; sh; OptV ext; CaseV ("ZERO", [OptV _]) ] as l -> (
    match op with
    | "EXTEND" -> (
      match half, ext with
      | Some (CaseV ("LOW", [])), Some (CaseV ("S", [])) -> V128Op.ExtendLowS
      | Some (CaseV ("LOW", [])), Some (CaseV ("U", [])) -> V128Op.ExtendLowU
      | Some (CaseV ("HIGH", [])), Some (CaseV ("S", [])) -> V128Op.ExtendHighS
      | Some (CaseV ("HIGH", [])), Some (CaseV ("U", [])) -> V128Op.ExtendHighU
      | _ -> fail_list "integer vcvtop" l
    )
    | "TRUNC_SAT" -> (
      match sh, ext with
      | TupV [ CaseV ("F32", []); NumV 4L ], Some (CaseV ("S", [])) -> V128Op.TruncSatSF32x4
      | TupV [ CaseV ("F32", []); NumV 4L ], Some (CaseV ("U", [])) -> V128Op.TruncSatUF32x4
      | TupV [ CaseV ("F64", []); NumV 2L ], Some (CaseV ("S", [])) -> V128Op.TruncSatSZeroF64x2
      | TupV [ CaseV ("F64", []); NumV 2L ], Some (CaseV ("U", [])) -> V128Op.TruncSatUZeroF64x2
      | _ -> fail_list "integer vcvtop" l
    )
    | _ -> fail_list "integer vcvtop" l
  )
  | l -> fail_list "integer vcvtop" l

let al_to_float_vcvtop : value list -> V128Op.fcvtop = function
  | [ CaseV (op, []); OptV _; _; OptV ext; CaseV ("ZERO", [OptV _]) ] as l -> (
    match op with
    | "DEMOTE" -> V128Op.DemoteZeroF64x2
    | "CONVERT" -> (
      match ext with
      | Some (CaseV ("S", [])) -> V128Op.ConvertSI32x4
      | Some (CaseV ("U", [])) -> V128Op.ConvertUI32x4
      | _ -> fail_list "float vcvtop" l
    )
    | "PROMOTE" -> V128Op.PromoteLowF32x4
    | _ -> fail_list "float vcvtop" l
  )
  | l -> fail_list "float vcvtop" l

let al_to_vcvtop : value list -> vec_cvtop = function
  | TupV [ CaseV ("I8", []); NumV 16L ] :: op -> V128 (V128.I8x16 (al_to_int_vcvtop op))
  | TupV [ CaseV ("I16", []); NumV 8L ] :: op -> V128 (V128.I16x8 (al_to_int_vcvtop op))
  | TupV [ CaseV ("I32", []); NumV 4L ] :: op -> V128 (V128.I32x4 (al_to_int_vcvtop op))
  | TupV [ CaseV ("I64", []); NumV 2L ] :: op -> V128 (V128.I64x2 (al_to_int_vcvtop op))
  | TupV [ CaseV ("F32", []); NumV 4L ] :: op -> V128 (V128.F32x4 (al_to_float_vcvtop op))
  | TupV [ CaseV ("F64", []); NumV 2L ] :: op -> V128 (V128.F64x2 (al_to_float_vcvtop op))
  | l -> fail_list "vcvtop" l

let al_to_special_vcvtop = function
  | CaseV ("VEXTADD_PAIRWISE", [ TupV [ CaseV ("I16", []); NumV 8L]; TupV [ CaseV ("I8", []); NumV 16L ]; CaseV ("S", []) ]) -> V128 (V128.I16x8 (V128Op.ExtAddPairwiseS))
  | CaseV ("VEXTADD_PAIRWISE", [ TupV [ CaseV ("I16", []); NumV 8L]; TupV [ CaseV ("I8", []); NumV 16L ]; CaseV ("U", []) ]) -> V128 (V128.I16x8 (V128Op.ExtAddPairwiseU))
  | CaseV ("VEXTADD_PAIRWISE", [ TupV [ CaseV ("I32", []); NumV 4L]; TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("S", []) ]) -> V128 (V128.I32x4 (V128Op.ExtAddPairwiseS))
  | CaseV ("VEXTADD_PAIRWISE", [ TupV [ CaseV ("I32", []); NumV 4L]; TupV [ CaseV ("I16", []); NumV 8L ]; CaseV ("U", []) ]) -> V128 (V128.I32x4 (V128Op.ExtAddPairwiseU))
  | v -> fail "special vcvtop" v


let al_to_int_vshiftop : value -> V128Op.ishiftop = function
  | CaseV ("SHL", []) -> V128Op.Shl
  | CaseV ("SHR", [CaseV ("S", [])]) -> V128Op.ShrS
  | CaseV ("SHR", [CaseV ("U", [])]) -> V128Op.ShrU
  | v -> fail "integer vshiftop" v
let al_to_float_vshiftop : value -> void = fail "float vshiftop"
let al_to_vshiftop : value list -> vec_shiftop = al_to_vop al_to_int_vshiftop al_to_float_vshiftop

let al_to_vvtestop' : value -> V128Op.vtestop = function
  | CaseV ("ANY_TRUE", []) -> V128Op.AnyTrue
  | v -> fail "vvtestop" v
let al_to_vvtestop : value list -> vec_vtestop = al_to_vvop al_to_vvtestop'

let al_to_vvunop' : value -> V128Op.vunop = function
  | CaseV ("NOT", []) -> V128Op.Not
  | v -> fail "vvunop" v
let al_to_vvunop : value list -> vec_vunop = al_to_vvop al_to_vvunop'

let al_to_vvbinop' = function
  | CaseV ("AND", []) -> V128Op.And
  | CaseV ("OR", []) -> V128Op.Or
  | CaseV ("XOR", []) -> V128Op.Xor
  | CaseV ("ANDNOT", []) -> V128Op.AndNot
  | v -> fail "vvbinop" v
let al_to_vvbinop : value list -> vec_vbinop = al_to_vvop al_to_vvbinop'

let al_to_vvternop' : value -> V128Op.vternop = function
  | CaseV ("BITSELECT", []) -> V128Op.Bitselect
  | v -> fail "vvternop" v
let al_to_vvternop : value list -> vec_vternop = al_to_vvop al_to_vvternop'

let al_to_vsplatop : value list -> vec_splatop = function
  | [ TupV [ CaseV ("I8", []); NumV 16L ] ] -> V128 (V128.I8x16 Splat)
  | [ TupV [ CaseV ("I16", []); NumV 8L ] ] -> V128 (V128.I16x8 Splat)
  | [ TupV [ CaseV ("I32", []); NumV 4L ] ] -> V128 (V128.I32x4 Splat)
  | [ TupV [ CaseV ("I64", []); NumV 2L ] ] -> V128 (V128.I64x2 Splat)
  | [ TupV [ CaseV ("F32", []); NumV 4L ] ] -> V128 (V128.F32x4 Splat)
  | [ TupV [ CaseV ("F64", []); NumV 2L ] ] -> V128 (V128.F64x2 Splat)
  | vl -> fail_list "vsplatop" vl

let al_to_vextractop : value list -> vec_extractop = function
  | [ TupV [ CaseV ("I8", []); NumV 16L ]; OptV (Some ext); n ] ->
    V128 (V128.I8x16 (Extract (al_to_int n, al_to_extension ext)))
  | [ TupV [ CaseV ("I16", []); NumV 8L ]; OptV (Some ext); n ] ->
    V128 (V128.I16x8 (Extract (al_to_int n, al_to_extension ext)))
  | [ TupV [ CaseV ("I32", []); NumV 4L ]; OptV None; n ] ->
    V128 (V128.I32x4 (Extract (al_to_int n, ())))
  | [ TupV [ CaseV ("I64", []); NumV 2L ]; OptV None; n ] ->
    V128 (V128.I64x2 (Extract (al_to_int n, ())))
  | [ TupV [ CaseV ("F32", []); NumV 4L ]; OptV None; n ] ->
    V128 (V128.F32x4 (Extract (al_to_int n, ())))
  | [ TupV [ CaseV ("F64", []); NumV 2L ]; OptV None; n ] ->
    V128 (V128.F64x2 (Extract (al_to_int n, ())))
  | vl -> fail_list "vextractop" vl

let al_to_vreplaceop : value list -> vec_replaceop = function
  | [ TupV [ CaseV ("I8", []); NumV 16L ]; n ] -> V128 (V128.I8x16 (Replace (al_to_int n)))
  | [ TupV [ CaseV ("I16", []); NumV 8L ]; n ] -> V128 (V128.I16x8 (Replace (al_to_int n)))
  | [ TupV [ CaseV ("I32", []); NumV 4L ]; n ] -> V128 (V128.I32x4 (Replace (al_to_int n)))
  | [ TupV [ CaseV ("I64", []); NumV 2L ]; n ] -> V128 (V128.I64x2 (Replace (al_to_int n)))
  | [ TupV [ CaseV ("F32", []); NumV 4L ]; n ] -> V128 (V128.F32x4 (Replace (al_to_int n)))
  | [ TupV [ CaseV ("F64", []); NumV 2L ]; n ] -> V128 (V128.F64x2 (Replace (al_to_int n)))
  | vl -> fail_list "vreplaceop" vl

let al_to_pack_size : value -> Types.pack_size = function
  | NumV 8L -> Types.Pack8
  | NumV 16L -> Types.Pack16
  | NumV 32L -> Types.Pack32
  | NumV 64L -> Types.Pack64
  | v -> fail "pack_size" v

let al_to_extension: value -> Types.extension = function
  | CaseV ("S", []) -> Types.SX
  | CaseV ("U", []) -> Types.ZX
  | v -> fail "extension" v

let al_to_memop (f: value -> 'p) : value list -> (num_type, 'p) memop = function
  | [ nt; p; StrV str ] ->
    {
      ty = al_to_num_type nt;
      align = Record.find "ALIGN" str |> al_to_int;
      offset = Record.find "OFFSET" str |> al_to_int32;
      pack = f p;
    }
  | v -> fail_list "memop" v

let al_to_pack_size_extension: value -> Types.pack_size * Types.extension = function
  | TupV [ p; ext ] -> al_to_pack_size p, al_to_extension ext
  | v -> fail "pack size, extension" v

let al_to_loadop: value list -> loadop = al_to_opt al_to_pack_size_extension |> al_to_memop

let al_to_storeop: value list -> storeop = al_to_opt al_to_pack_size |> al_to_memop

let al_to_vmemop (f: value -> 'p): value list -> (vec_type, 'p) memop = function
  | [ StrV str ] ->
    {
      ty = V128Type;
      align = Record.find "ALIGN" str |> al_to_int;
      offset = Record.find "OFFSET" str |> al_to_int32;
      pack = f (numV 0L);
    }
  | [ p; StrV str ] ->
    {
      ty = V128Type;
      align = Record.find "ALIGN" str |> al_to_int;
      offset = Record.find "OFFSET" str |> al_to_int32;
      pack = f p;
    }
  | v -> fail_list "vmemop" v

let al_to_pack_shape = function
  | TupV [NumV 8L; NumV 8L] -> Types.Pack8x8
  | TupV [NumV 16L; NumV 4L] -> Types.Pack16x4
  | TupV [NumV 32L; NumV 2L] -> Types.Pack32x2
  | v -> fail "pack shape" v

let pack_shape_to_pack_size = function
  | Types.Pack8x8 -> Types.Pack8
  | Types.Pack16x4 -> Types.Pack16
  | Types.Pack32x2 -> Types.Pack32

let al_to_vloadop': value -> pack_size * vec_extension = function
  | CaseV ("SHAPE", [ pack_shape; ext ]) ->
    (
      pack_shape_to_pack_size (al_to_pack_shape pack_shape),
      Types.ExtLane (al_to_pack_shape pack_shape, al_to_extension ext)
    )
  | CaseV ("SPLAT", [ pack_size ]) -> al_to_pack_size pack_size, Types.ExtSplat
  | CaseV ("ZERO", [ pack_size ]) -> al_to_pack_size pack_size, Types.ExtZero
  | v -> fail "vloadop" v

let al_to_vloadop: value list -> vec_loadop = al_to_vmemop (al_to_opt al_to_vloadop')

let al_to_vstoreop = al_to_vmemop (fun _ -> ())

let al_to_vlaneop (vl: value list): vec_laneop =
  let h, t = Util.Lib.List.split_last vl in
  al_to_vmemop al_to_pack_size h, al_to_int t


let rec al_to_instr (v: value): Ast.instr = al_to_phrase al_to_instr' v
and al_to_instr': value -> Ast.instr' = function
  (* wasm values *)
  | CaseV ("CONST", _) as v -> Const (al_to_phrase al_to_num v)
  | CaseV ("VVCONST", _) as v -> VecConst (al_to_phrase al_to_vec v)
  | CaseV ("REF.NULL", [ rt ]) -> RefNull (al_to_ref_type rt)
  (* wasm instructions *)
  | CaseV ("UNREACHABLE", []) -> Unreachable
  | CaseV ("NOP", []) -> Nop
  | CaseV ("DROP", []) -> Drop
  | CaseV ("UNOP", op) -> Unary (al_to_unop op)
  | CaseV ("BINOP", op) -> Binary (al_to_binop op)
  | CaseV ("TESTOP", op) -> Test (al_to_testop op)
  | CaseV ("RELOP", op) -> Compare (al_to_relop op)
  | CaseV ("EXTEND", op) -> Unary (al_to_extendop op)
  | CaseV ("CVTOP", op) -> Convert (al_to_cvtop op)
  | CaseV ("VALL_TRUE", vop) -> VecTest (al_to_vtestop vop)
  | CaseV ("VRELOP", vop) -> VecCompare (al_to_vrelop vop)
  | CaseV ("VUNOP", vop) -> VecUnary (al_to_vunop vop)
  | CaseV ("VBINOP", vop) -> VecBinary (al_to_vbinop vop)
  | CaseV (("VSWIZZLE" | "VSHUFFLE" | "VNARROW" | "VEXTMUL" | "VDOT"), _) as v ->
    VecBinary (al_to_special_vbinop v)
  | CaseV ("VCVTOP", vop) -> VecConvert (al_to_vcvtop vop)
  | CaseV ("VEXTADD_PAIRWISE", _) as v -> VecConvert (al_to_special_vcvtop v)
  | CaseV ("VISHIFTOP", vop) -> VecShift (al_to_vshiftop vop)
  | CaseV ("VBITMASK", vop) -> VecBitmask (al_to_vbitmaskop vop)
  | CaseV ("VVTESTOP", vop) -> VecTestBits (al_to_vvtestop vop)
  | CaseV ("VVUNOP", vop) -> VecUnaryBits (al_to_vvunop vop)
  | CaseV ("VVBINOP", vop) -> VecBinaryBits (al_to_vvbinop vop)
  | CaseV ("VVTERNOP", vop) -> VecTernaryBits (al_to_vvternop vop)
  | CaseV ("VSPLAT", vop) -> VecSplat (al_to_vsplatop vop)
  | CaseV ("VEXTRACT_LANE", vop) -> VecExtract (al_to_vextractop vop)
  | CaseV ("VREPLACE_LANE", vop) -> VecReplace (al_to_vreplaceop vop)
  | CaseV ("REF.IS_NULL", []) -> RefIsNull
  | CaseV ("REF.FUNC", [ var ]) -> RefFunc (al_to_var var)
  | CaseV ("SELECT", [ vtl_opt ]) -> Select (al_to_opt (al_to_list al_to_val_type) vtl_opt)
  | CaseV ("LOCAL.GET", [ var ]) -> LocalGet (al_to_var var)
  | CaseV ("LOCAL.SET", [ var ]) -> LocalSet (al_to_var var)
  | CaseV ("LOCAL.TEE", [ var ]) -> LocalTee (al_to_var var)
  | CaseV ("GLOBAL.GET", [ var ]) -> GlobalGet (al_to_var var)
  | CaseV ("GLOBAL.SET", [ var ]) -> GlobalSet (al_to_var var)
  | CaseV ("TABLE.GET", [ var ]) -> TableGet (al_to_var var)
  | CaseV ("TABLE.SET", [ var ]) -> TableSet (al_to_var var)
  | CaseV ("TABLE.SIZE", [ var ]) -> TableSize (al_to_var var)
  | CaseV ("TABLE.GROW", [ var ]) -> TableGrow (al_to_var var)
  | CaseV ("TABLE.FILL", [ var ]) -> TableFill (al_to_var var)
  | CaseV ("TABLE.COPY", [ var1; var2 ]) -> TableCopy (al_to_var var1, al_to_var var2)
  | CaseV ("TABLE.INIT", [ var1; var2 ]) -> TableInit (al_to_var var1, al_to_var var2)
  | CaseV ("ELEM.DROP", [ var ]) -> ElemDrop (al_to_var var)
  | CaseV ("BLOCK", [ bt; instrs ]) ->
    Block (al_to_block_type bt, al_to_list al_to_instr instrs)
  | CaseV ("LOOP", [ bt; instrs ]) ->
    Loop (al_to_block_type bt, al_to_list al_to_instr instrs)
  | CaseV ("IF", [ bt; instrs1; instrs2 ]) ->
    If (al_to_block_type bt, al_to_list al_to_instr instrs1, al_to_list al_to_instr instrs2)
  | CaseV ("BR", [ var ]) -> Br (al_to_var var)
  | CaseV ("BR_IF", [ var ]) -> BrIf (al_to_var var)
  | CaseV ("BR_TABLE", [ vars; var ]) -> BrTable (al_to_list al_to_var vars, al_to_var var)
  | CaseV ("RETURN", []) -> Return
  | CaseV ("CALL", [ var ]) -> Call (al_to_var var)
  | CaseV ("CALL_INDIRECT", [ var1; var2 ]) ->
    CallIndirect (al_to_var var1, al_to_var var2)
  | CaseV ("LOAD", loadop) -> Load (al_to_loadop loadop)
  | CaseV ("STORE", storeop) -> Store (al_to_storeop storeop)
  | CaseV ("VLOAD", vloadop) -> VecLoad (al_to_vloadop vloadop)
  | CaseV ("VLOAD_LANE", vlaneop) -> VecLoadLane (al_to_vlaneop vlaneop)
  | CaseV ("VSTORE", vstoreop) -> VecStore (al_to_vstoreop vstoreop)
  | CaseV ("VSTORE_LANE", vlaneop) -> VecStoreLane (al_to_vlaneop vlaneop)
  | CaseV ("MEMORY.SIZE", []) -> MemorySize
  | CaseV ("MEMORY.GROW", []) -> MemoryGrow
  | CaseV ("MEMORY.FILL", []) -> MemoryFill
  | CaseV ("MEMORY.COPY", []) -> MemoryCopy
  | CaseV ("MEMORY.INIT", [ var ]) -> MemoryInit (al_to_var var)
  | CaseV ("DATA.DROP", [ var ]) -> DataDrop (al_to_var var)
  | v -> fail "instrunction" v

let al_to_const: value -> const = al_to_list al_to_instr |> al_to_phrase


(* Deconstruct module *)

let al_to_local: value -> value_type = function
  | CaseV ("LOCAL", [ vt ]) -> al_to_val_type vt
  | v -> fail "local" v

let al_to_func': value -> func' = function
  | CaseV ("FUNC", [ var; locals; instrs ]) ->
    {
      ftype = al_to_var var;
      locals = al_to_list al_to_local locals;
      body = al_to_list al_to_instr instrs;
    }
  | v -> fail "func" v
let al_to_func: value -> func = al_to_phrase al_to_func'

let al_to_global': value -> global' = function
  | CaseV ("GLOBAL", [ gt; const ]) ->
    { gtype = al_to_global_type gt; ginit = al_to_const const }
  | v -> fail "global" v
let al_to_global: value -> global = al_to_phrase al_to_global'

let al_to_table': value -> table' = function
  | CaseV ("TABLE", [ tt ]) ->
    { ttype = al_to_table_type tt }
  | v -> fail "table" v
let al_to_table: value -> table = al_to_phrase al_to_table'

let al_to_memory': value -> memory' = function
  | CaseV ("MEMORY", [ mt ]) -> { mtype = al_to_memory_type mt }
  | v -> fail "memory" v
let al_to_memory: value -> memory = al_to_phrase al_to_memory'

let al_to_segment': value -> segment_mode' = function
  | CaseV ("PASSIVE", []) -> Passive
  | CaseV ("ACTIVE", [ var; const ]) ->
    Active { index = al_to_var var; offset = al_to_const const }
  | CaseV ("DECLARE", []) -> Declarative
  | v -> fail "segment mode" v
let al_to_segment: value -> segment_mode = al_to_phrase al_to_segment'

let al_to_elem': value -> elem_segment' = function
  | CaseV ("ELEM", [ rt; consts; seg ]) ->
    {
      etype = al_to_ref_type rt;
      einit = al_to_list al_to_const consts;
      emode = al_to_segment seg
    }
  | v -> fail "elem segment" v
let al_to_elem: value -> elem_segment = al_to_phrase al_to_elem'

let al_to_data': value -> data_segment' = function
  | CaseV ("DATA", [ bytes_; seg ]) ->
    { dinit = al_to_bytes bytes_; dmode = al_to_segment seg }
  | v -> fail "data segment" v
let al_to_data: value -> data_segment = al_to_phrase al_to_data'

let al_to_import_desc _module_ = function
  | CaseV ("FUNC", [ x ]) -> FuncImport (al_to_phrase (fun _ -> 0l) x) (* TODO *)
  | CaseV ("TABLE", [ tt ]) -> TableImport (al_to_table_type tt)
  | CaseV ("MEM", [ mt ]) -> MemoryImport (al_to_memory_type mt)
  | CaseV ("GLOBAL", [ gt ]) -> GlobalImport (al_to_global_type gt)
  | v -> fail "import desc" v

let al_to_import' module_ = function
  | CaseV ("IMPORT", [ module_name; item_name; import_desc ]) -> {
    module_name = al_to_name module_name;
    item_name = al_to_name item_name;
    idesc = al_to_phrase (al_to_import_desc module_) import_desc;
  }
  | v -> fail "import" v
let al_to_import module_: value -> import = al_to_phrase (al_to_import' module_)

let al_to_export_desc': value -> export_desc' = function
  | CaseV ("FUNC", [ var ]) -> FuncExport (al_to_var var)
  | CaseV ("TABLE", [ var ]) -> TableExport (al_to_var var)
  | CaseV ("MEM", [ var ]) -> MemoryExport (al_to_var var)
  | CaseV ("GLOBAL", [ var ]) -> GlobalExport (al_to_var var)
  | v -> fail "export desc" v
let al_to_export_desc: value -> export_desc = al_to_phrase al_to_export_desc'

let al_to_start': value -> start' = function
  | CaseV ("START", [ var ]) -> { sfunc = al_to_var var }
  | v -> fail "start" v
let al_to_start: value -> start = al_to_phrase al_to_start'

let al_to_export': value -> export' = function
  | CaseV ("EXPORT", [ name; ed ]) ->
    { name = al_to_name name; edesc = al_to_export_desc ed }
  | v -> fail "export" v
let al_to_export: value -> export = al_to_phrase al_to_export'

let al_to_module': value -> module_' = function
  | CaseV ("MODULE", [
    types; imports; funcs; globals; tables; memories; elems; datas; start; exports
  ]) as module_ ->
    {
      types = al_to_list al_to_type types;
      imports = al_to_list (al_to_import module_) imports;
      funcs = al_to_list al_to_func funcs;
      globals = al_to_list al_to_global globals;
      tables = al_to_list al_to_table tables;
      memories = al_to_list al_to_memory memories;
      elems = al_to_list al_to_elem elems;
      datas = al_to_list al_to_data datas;
      start = al_to_opt al_to_start start;
      exports = al_to_list al_to_export exports;
    }
  | v -> fail "module" v
let al_to_module: value -> module_ = al_to_phrase al_to_module'


(* Construct *)

(* Construct data structure *)

let al_of_list f l = List.map f l |> listV_of_list
let al_of_seq f s = List.of_seq s |> al_of_list f
let al_of_opt f opt = Option.map f opt |> optV


(* Construct minor *)

let al_of_int64 i64 = numV i64
let al_of_int i = Int64.of_int i |> al_of_int64
let al_of_int8 i8 =
  (* NOTE: int8 is considered to be unsigned *)
  Int64.of_int32 i8 |> Int64.logand 0x0000_0000_0000_00ffL |> al_of_int64
let al_of_int16 i16 =
  (* NOTE: int32 is considered to be unsigned *)
  Int64.of_int32 i16 |> Int64.logand 0x0000_0000_0000_ffffL |> al_of_int64
let al_of_int32 i32 =
  (* NOTE: int32 is considered to be unsigned *)
  Int64.of_int32 i32 |> Int64.logand 0x0000_0000_ffff_ffffL |> al_of_int64
let al_of_float32 f32 = F32.to_bits f32 |> al_of_int32
let al_of_float64 f64 = F64.to_bits f64 |> al_of_int64
let al_of_vector vec = V128.to_bits vec |> vecV
let al_of_bool b = Bool.to_int b |> al_of_int
let al_of_var var = al_of_int32 var.it
let al_of_byte byte = Char.code byte |> al_of_int
let al_of_bytes bytes_ = String.to_seq bytes_ |> al_of_seq al_of_byte
let al_of_name name = TextV (Utf8.encode name)
let al_with_version vs f a = if (List.mem !version vs) then [ f a ] else []

(* Helper *)

let arg_of_case case i = function
| CaseV (case', args) when case = case' -> List.nth args i
| _ -> failwith "invalid arg_of_case"
let arg_of_tup i = function
| TupV args -> List.nth args i
| _ -> failwith "invalid arg_of_tup"

(* Construct type *)

let al_of_mut = function
  | Immutable -> none "MUT"
  | Mutable -> some "MUT"

let rec al_of_result_type rt = al_of_list al_of_val_type rt

and al_of_ref_type = function
  | FuncRefType -> nullary "FUNCREF"
  | ExternRefType -> nullary "EXTERNREF"

and al_of_num_type nt = nullary (string_of_num_type nt)

and al_of_vec_type vt = nullary (string_of_vec_type vt)

and al_of_val_type = function
  | RefType rt -> al_of_ref_type rt
  | NumType nt -> al_of_num_type nt
  | VecType vt -> al_of_vec_type vt

let al_of_blocktype = function
  | VarBlockType var -> CaseV ("_IDX", [ al_of_var var ])
  | ValBlockType vt_opt ->
    if !version = 1 then
      al_of_opt al_of_val_type vt_opt
    else
      CaseV ("_RESULT", [ al_of_opt al_of_val_type vt_opt ])

let al_of_limits default limits =
  let max =
    match limits.max with
    | Some v -> al_of_int32 v
    | None -> al_of_int64 default
  in

  tupV [ al_of_int32 limits.min; max ]

let al_of_global_type = function
  | GlobalType (vt, mut) -> tupV [ al_of_mut mut; al_of_val_type vt ]

let al_of_table_type = function
  | TableType (limits, rt) -> tupV [ al_of_limits default_table_max limits; al_of_ref_type rt ]

let al_of_memory_type = function
  | MemoryType limits -> CaseV ("I8", [ al_of_limits default_memory_max limits ])

(* Construct value *)

let al_of_num = function
  | I32 i32 -> CaseV ("CONST", [ nullary "I32"; al_of_int32 i32 ])
  | I64 i64 -> CaseV ("CONST", [ nullary "I64"; al_of_int64 i64 ])
  | F32 f32 -> CaseV ("CONST", [ nullary "F32"; al_of_float32 f32 ])
  | F64 f64 -> CaseV ("CONST", [ nullary "F64"; al_of_float64 f64 ])

let al_of_vec = function
  | V128 v128 -> CaseV ("VVCONST", [ nullary "V128"; VecV (V128.to_bits v128) ])

let al_of_vec_shape shape (lanes: int64 list) =
  al_of_vec (V128  (
    match shape with
    | V128.I8x16() -> V128.I8x16.of_lanes (List.map Int64.to_int32 lanes)
    | V128.I16x8() -> V128.I16x8.of_lanes (List.map Int64.to_int32 lanes)
    | V128.I32x4() -> V128.I32x4.of_lanes (List.map Int64.to_int32 lanes)
    | V128.I64x2() -> V128.I64x2.of_lanes lanes
    | V128.F32x4() -> V128.F32x4.of_lanes (List.map (fun i -> i |> Int64.to_int32 |> F32.of_bits) lanes)
    | V128.F64x2() -> V128.F64x2.of_lanes (List.map F64.of_bits lanes)
  ))

let al_of_ref = function
  | NullRef rt -> CaseV ("REF.NULL", [ al_of_ref_type rt ])
  (*
  | I31.I31Ref i ->
    CaseV ("REF.I31_NUM", [ NumV (Int64.of_int i) ])
  | Aggr.StructRef a ->
    CaseV ("REF.STRUCT_ADDR", [ NumV (int64_of_int32_u a) ])
  | Aggr.ArrayRef a ->
    CaseV ("REF.ARRAY_ADDR", [ NumV (int64_of_int32_u a) ])
  | Instance.FuncRef a ->
    CaseV ("REF.FUNC_ADDR", [ NumV (int64_of_int32_u a) ])
  | Script.HostRef i32 -> CaseV ("REF.HOST_ADDR", [ al_of_int32 i32 ])
  *)
  | Script.ExternRef a -> CaseV ("REF.EXTERN", [ al_of_int32 a ])
  | r -> string_of_ref r |> failwith

let al_of_value = function
  | Num n -> al_of_num n
  | Vec v -> al_of_vec v
  | Ref r -> al_of_ref r


(* Construct operation *)

let al_of_op f1 f2 = function
  | I32 op -> [ nullary "I32"; caseV ("_I", [f1 op]) ]
  | I64 op -> [ nullary "I64"; caseV ("_I", [f1 op]) ]
  | F32 op -> [ nullary "F32"; caseV ("_F", [f2 op]) ]
  | F64 op -> [ nullary "F64"; caseV ("_F", [f2 op]) ]

let al_of_int_unop = function
  | IntOp.Clz -> CaseV ("CLZ", [])
  | IntOp.Ctz -> CaseV ("CTZ", [])
  | IntOp.Popcnt -> CaseV ("POPCNT", [])
  | IntOp.ExtendS Types.Pack8 -> CaseV ("EXTEND8S", [])
  | IntOp.ExtendS Types.Pack16 -> CaseV ("EXTEND16S", [])
  | IntOp.ExtendS Types.Pack32 -> CaseV ("EXTEND32S", [])
  | IntOp.ExtendS Types.Pack64 -> CaseV ("EXTEND64S", [])
let al_of_float_unop = function
  | FloatOp.Neg -> CaseV ("NEG", [])
  | FloatOp.Abs -> CaseV ("ABS", [])
  | FloatOp.Ceil -> CaseV ("CEIL", [])
  | FloatOp.Floor -> CaseV ("FLOOR", [])
  | FloatOp.Trunc -> CaseV ("TRUNC", [])
  | FloatOp.Nearest -> CaseV ("NEAREST", [])
  | FloatOp.Sqrt -> CaseV ("SQRT", [])
let al_of_unop = al_of_op al_of_int_unop al_of_float_unop

let al_of_int_binop = function
  | IntOp.Add -> CaseV ("ADD", [])
  | IntOp.Sub -> CaseV ("SUB", [])
  | IntOp.Mul -> CaseV ("MUL", [])
  | IntOp.DivS -> CaseV ("DIV", [CaseV ("S", [])])
  | IntOp.DivU -> CaseV ("DIV", [CaseV ("U", [])])
  | IntOp.RemS -> CaseV ("REM", [CaseV ("S", [])])
  | IntOp.RemU -> CaseV ("REM", [CaseV ("U", [])])
  | IntOp.And -> CaseV ("AND", [])
  | IntOp.Or -> CaseV ("OR", [])
  | IntOp.Xor -> CaseV ("XOR", [])
  | IntOp.Shl -> CaseV ("SHL", [])
  | IntOp.ShrS -> CaseV ("SHR", [CaseV ("S", [])])
  | IntOp.ShrU -> CaseV ("SHR", [CaseV ("U", [])])
  | IntOp.Rotl -> CaseV ("ROTL", [])
  | IntOp.Rotr -> CaseV ("ROTR", [])
let al_of_float_binop = function
  | FloatOp.Add -> CaseV ("ADD", [])
  | FloatOp.Sub -> CaseV ("SUB", [])
  | FloatOp.Mul -> CaseV ("MUL", [])
  | FloatOp.Div -> CaseV ("DIV", [])
  | FloatOp.Min -> CaseV ("MIN", [])
  | FloatOp.Max -> CaseV ("MAX", [])
  | FloatOp.CopySign -> CaseV ("COPYSIGN", [])
let al_of_binop = al_of_op al_of_int_binop al_of_float_binop

let al_of_int_testop: IntOp.testop -> value = function
  | IntOp.Eqz -> CaseV ("EQZ", [])
let al_of_float_testop: FloatOp.testop -> value = function
  | _ -> .
let al_of_testop: testop -> value list = al_of_op al_of_int_testop al_of_float_testop

let al_of_int_relop = function
  | IntOp.Eq -> CaseV ("EQ", [])
  | IntOp.Ne -> CaseV ("NE", [])
  | IntOp.LtS -> CaseV ("LT", [CaseV ("S", [])])
  | IntOp.LtU -> CaseV ("LT", [CaseV ("U", [])])
  | IntOp.GtS -> CaseV ("GT", [CaseV ("S", [])])
  | IntOp.GtU -> CaseV ("GT", [CaseV ("U", [])])
  | IntOp.LeS -> CaseV ("LE", [CaseV ("S", [])])
  | IntOp.LeU -> CaseV ("LE", [CaseV ("U", [])])
  | IntOp.GeS -> CaseV ("GE", [CaseV ("S", [])])
  | IntOp.GeU -> CaseV ("GE", [CaseV ("U", [])])
let al_of_float_relop = function
  | FloatOp.Eq -> CaseV ("EQ", [])
  | FloatOp.Ne -> CaseV ("NE", [])
  | FloatOp.Lt -> CaseV ("LT", [])
  | FloatOp.Gt -> CaseV ("GT", [])
  | FloatOp.Le -> CaseV ("LE", [])
  | FloatOp.Ge -> CaseV ("GE", [])
let al_of_relop = al_of_op al_of_int_relop al_of_float_relop

let al_of_int_cvtop num_bits = function
  | IntOp.ExtendSI32 -> "Extend", "I32", Some (nullary "S")
  | IntOp.ExtendUI32 -> "Extend", "I32", Some (nullary "U")
  | IntOp.WrapI64 -> "Wrap", "I64", None
  | IntOp.TruncSF32 -> "Trunc", "F32", Some (nullary "S")
  | IntOp.TruncUF32 -> "Trunc", "F32", Some (nullary "U")
  | IntOp.TruncSF64 -> "Trunc", "F64", Some (nullary "S")
  | IntOp.TruncUF64 -> "Trunc", "F64", Some (nullary "U")
  | IntOp.TruncSatSF32 -> "TruncSat", "F32", Some (nullary "S")
  | IntOp.TruncSatUF32 -> "TruncSat", "F32", Some (nullary "U")
  | IntOp.TruncSatSF64 -> "TruncSat", "F64", Some (nullary "S")
  | IntOp.TruncSatUF64 -> "TruncSat", "F64", Some (nullary "U")
  | IntOp.ReinterpretFloat -> "Reinterpret", "F" ^ num_bits, None
let al_of_float_cvtop num_bits = function
  | FloatOp.ConvertSI32 -> "Convert", "I32", Some (nullary ("S"))
  | FloatOp.ConvertUI32 -> "Convert", "I32", Some (nullary ("U"))
  | FloatOp.ConvertSI64 -> "Convert", "I64", Some (nullary ("S"))
  | FloatOp.ConvertUI64 -> "Convert", "I64", Some (nullary ("U"))
  | FloatOp.PromoteF32 -> "Promote", "F32", None
  | FloatOp.DemoteF64 -> "Demote", "F64", None
  | FloatOp.ReinterpretInt -> "Reinterpret", "I" ^ num_bits, None
let al_of_cvtop = function
  | I32 op ->
    let op', to_, ext = al_of_int_cvtop "32" op in
    [ nullary "I32"; nullary op'; nullary to_; optV ext ]
  | I64 op ->
    let op', to_, ext = al_of_int_cvtop "64" op in
    [ nullary "I64"; nullary op'; nullary to_; optV ext ]
  | F32 op ->
    let op', to_, ext = al_of_float_cvtop "32" op in
    [ nullary "F32"; nullary op'; nullary to_; optV ext ]
  | F64 op ->
    let op', to_, ext = al_of_float_cvtop "64" op in
    [ nullary "F64"; nullary op'; nullary to_; optV ext ]

(* Vector operator *)

let al_of_extension = function
  | Types.SX -> nullary "S"
  | Types.ZX -> nullary "U"

let al_of_vop f1 f2 = function
  | V128 vop -> (
    match vop with
    | V128.I8x16 op -> [ TupV [ nullary "I8"; numV 16L ]; f1 op ]
    | V128.I16x8 op -> [ TupV [ nullary "I16"; numV 8L ]; f1 op ]
    | V128.I32x4 op -> [ TupV [ nullary "I32"; numV 4L ]; f1 op ]
    | V128.I64x2 op -> [ TupV [ nullary "I64"; numV 2L ]; f1 op ]
    | V128.F32x4 op -> [ TupV [ nullary "F32"; numV 4L ]; f2 op ]
    | V128.F64x2 op -> [ TupV [ nullary "F64"; numV 2L ]; f2 op ]
  )

let al_of_viop f1:
    ('a, 'a, 'a, 'a, void, void) V128.laneop vecop -> value list =
  function
  | V128 vop -> (
    match vop with
    | V128.I8x16 op -> [ TupV [ nullary "I8"; numV 16L ]; f1 op ]
    | V128.I16x8 op -> [ TupV [ nullary "I16"; numV 8L ]; f1 op ]
    | V128.I32x4 op -> [ TupV [ nullary "I32"; numV 4L ]; f1 op ]
    | V128.I64x2 op -> [ TupV [ nullary "I64"; numV 2L ]; f1 op ]
    | _ -> .
  )

let al_of_vtestop = function
  | V128 vop -> (
    match vop with
    | V128.I8x16 _ -> [ TupV [ nullary "I8"; numV 16L ] ]
    | V128.I16x8 _ -> [ TupV [ nullary "I16"; numV 8L ] ]
    | V128.I32x4 _ -> [ TupV [ nullary "I32"; numV 4L ] ]
    | V128.I64x2 _ -> [ TupV [ nullary "I64"; numV 2L ] ]
    | _ -> failwith "Invalid shape"
  )

let al_of_int_vrelop : V128Op.irelop -> value = function
  | V128Op.Eq -> CaseV ("_VI", [ nullary "EQ" ])
  | V128Op.Ne -> CaseV ("_VI", [ nullary "NE" ])
  | V128Op.LtS -> CaseV ("_VI", [ CaseV ("LT", [ nullary "S" ]) ])
  | V128Op.LtU -> CaseV ("_VI", [ CaseV ("LT", [ nullary "U" ]) ])
  | V128Op.LeS -> CaseV ("_VI", [ CaseV ("LE", [ nullary "S" ]) ])
  | V128Op.LeU -> CaseV ("_VI", [ CaseV ("LE", [ nullary "U" ]) ])
  | V128Op.GtS -> CaseV ("_VI", [ CaseV ("GT", [ nullary "S" ]) ])
  | V128Op.GtU -> CaseV ("_VI", [ CaseV ("GT", [ nullary "U" ]) ])
  | V128Op.GeS -> CaseV ("_VI", [ CaseV ("GE", [ nullary "S" ]) ])
  | V128Op.GeU -> CaseV ("_VI", [ CaseV ("GE", [ nullary "U" ]) ])

let al_of_float_vrelop : V128Op.frelop -> value = function
  | V128Op.Eq -> CaseV ("_VF", [ nullary "EQ" ])
  | V128Op.Ne -> CaseV ("_VF", [ nullary "NE" ])
  | V128Op.Lt -> CaseV ("_VF", [ nullary "LT" ])
  | V128Op.Le -> CaseV ("_VF", [ nullary "LE" ])
  | V128Op.Gt -> CaseV ("_VF", [ nullary "GT" ])
  | V128Op.Ge -> CaseV ("_VF", [ nullary "GE" ])

let al_of_vrelop = al_of_vop al_of_int_vrelop al_of_float_vrelop

let al_of_int_vunop : V128Op.iunop -> value = function
  | V128Op.Abs -> CaseV ("_VI", [ nullary "ABS" ])
  | V128Op.Neg -> CaseV ("_VI", [ nullary "NEG" ])
  | V128Op.Popcnt -> CaseV ("_VI", [ nullary "POPCNT" ])

let al_of_float_vunop : V128Op.funop -> value = function
  | V128Op.Abs -> CaseV ("_VF", [ nullary "ABS" ])
  | V128Op.Neg -> CaseV ("_VF", [ nullary "NEG" ])
  | V128Op.Sqrt -> CaseV ("_VF", [ nullary "SQRT" ])
  | V128Op.Ceil -> CaseV ("_VF", [ nullary "CEIL" ])
  | V128Op.Floor -> CaseV ("_VF", [ nullary "FLOOR" ])
  | V128Op.Trunc -> CaseV ("_VF", [ nullary "TRUNC" ])
  | V128Op.Nearest -> CaseV ("_VF", [ nullary "NEAREST" ])

let al_of_vunop = al_of_vop al_of_int_vunop al_of_float_vunop

let al_of_int_vbinop : V128Op.ibinop -> value option = function
  | V128Op.Add -> Some (CaseV ("_VI", [ nullary "ADD" ]))
  | V128Op.Sub -> Some (CaseV ("_VI", [ nullary "SUB" ]))
  | V128Op.Mul -> Some (CaseV ("_VI", [ nullary "MUL" ]))
  | V128Op.MinS -> Some (CaseV ("_VI", [ caseV ("MIN", [nullary "S"]) ]))
  | V128Op.MinU -> Some (CaseV ("_VI", [ caseV ("MIN", [nullary "U"]) ]))
  | V128Op.MaxS -> Some (CaseV ("_VI", [ caseV ("MAX", [nullary "S"]) ]))
  | V128Op.MaxU -> Some (CaseV ("_VI", [ caseV ("MAX", [nullary "U"]) ]))
  | V128Op.AvgrU -> Some (CaseV ("_VI", [ nullary "AVGR_U" ]))
  | V128Op.AddSatS -> Some (CaseV ("_VI", [ CaseV ("ADD_SAT", [nullary "S"]) ]))
  | V128Op.AddSatU -> Some (CaseV ("_VI", [ CaseV ("ADD_SAT", [nullary "U"]) ]))
  | V128Op.SubSatS -> Some (CaseV ("_VI", [ CaseV ("SUB_SAT", [nullary "S"])]))
  | V128Op.SubSatU -> Some (CaseV ("_VI", [ CaseV ("SUB_SAT", [nullary "U"])]))
  | V128Op.Q15MulRSatS -> Some (CaseV ("_VI", [ nullary "Q15MULR_SAT_S" ]))
  | _ -> None

let al_of_float_vbinop : V128Op.fbinop -> value = function
  | V128Op.Add -> CaseV ("_VF", [ nullary "ADD" ])
  | V128Op.Sub -> CaseV ("_VF", [ nullary "SUB" ])
  | V128Op.Mul -> CaseV ("_VF", [ nullary "MUL" ])
  | V128Op.Div -> CaseV ("_VF", [ nullary "DIV" ])
  | V128Op.Min -> CaseV ("_VF", [ nullary "MIN" ])
  | V128Op.Max -> CaseV ("_VF", [ nullary "MAX" ])
  | V128Op.Pmin -> CaseV ("_VF", [ nullary "PMIN" ])
  | V128Op.Pmax -> CaseV ("_VF", [ nullary "PMAX" ])

let al_of_vbinop = function
  | V128 vop -> (
    match vop with
    | V128.I8x16 op -> Option.map (fun v -> [ TupV [ nullary "I8"; numV 16L ]; v ]) (al_of_int_vbinop op)
    | V128.I16x8 op -> Option.map (fun v -> [ TupV [ nullary "I16"; numV 8L ]; v ]) (al_of_int_vbinop op)
    | V128.I32x4 op -> Option.map (fun v -> [ TupV [ nullary "I32"; numV 4L ]; v ]) (al_of_int_vbinop op)
    | V128.I64x2 op -> Option.map (fun v -> [ TupV [ nullary "I64"; numV 2L ]; v ]) (al_of_int_vbinop op)
    | V128.F32x4 op -> Some ([ TupV [ nullary "F32"; numV 4L ]; al_of_float_vbinop op ])
    | V128.F64x2 op -> Some ([ TupV [ nullary "F64"; numV 2L ]; al_of_float_vbinop op ])
  )

let al_of_special_vbinop = function
  | V128 (V128.I8x16 (V128Op.Swizzle)) -> CaseV ("VSWIZZLE", [ TupV [ nullary "I8"; numV 16L ]; ])
  | V128 (V128.I8x16 (V128Op.Shuffle l)) -> CaseV ("VSHUFFLE", [ TupV [ nullary "I8"; numV 16L ]; al_of_list al_of_int l ])
  | V128 (V128.I8x16 (V128Op.NarrowS)) -> CaseV ("VNARROW", [ TupV [ nullary "I8"; numV 16L ]; TupV [ nullary "I16"; numV 8L ]; al_of_extension Types.SX ])
  | V128 (V128.I16x8 (V128Op.NarrowS)) -> CaseV ("VNARROW", [ TupV [ nullary "I16"; numV 8L ]; TupV [ nullary "I32"; numV 4L ]; al_of_extension Types.SX ])
  | V128 (V128.I8x16 (V128Op.NarrowU)) -> CaseV ("VNARROW", [ TupV [ nullary "I8"; numV 16L ]; TupV [ nullary "I16"; numV 8L ]; al_of_extension Types.ZX ])
  | V128 (V128.I16x8 (V128Op.NarrowU)) -> CaseV ("VNARROW", [ TupV [ nullary "I16"; numV 8L]; TupV [ nullary "I32"; numV 4L ]; al_of_extension Types.ZX ])
  | V128 (V128.I16x8 (V128Op.ExtMulHighS)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I16"; numV 8L ]; nullary "HIGH"; TupV [ nullary "I8"; numV 16L ]; al_of_extension Types.SX ])
  | V128 (V128.I16x8 (V128Op.ExtMulHighU)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I16"; numV 8L ]; nullary "HIGH"; TupV [ nullary "I8"; numV 16L ]; al_of_extension Types.ZX ])
  | V128 (V128.I16x8 (V128Op.ExtMulLowS)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I16"; numV 8L ]; nullary "LOW"; TupV [ nullary "I8"; numV 16L ]; al_of_extension Types.SX ])
  | V128 (V128.I16x8 (V128Op.ExtMulLowU)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I16"; numV 8L ]; nullary "LOW"; TupV [ nullary "I8"; numV 16L ]; al_of_extension Types.ZX ] )
  | V128 (V128.I32x4 (V128Op.ExtMulHighS)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I32"; numV 4L ]; nullary "HIGH"; TupV [ nullary "I16"; numV 8L ]; al_of_extension Types.SX ])
  | V128 (V128.I32x4 (V128Op.ExtMulHighU)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I32"; numV 4L ]; nullary "HIGH"; TupV [ nullary "I16"; numV 8L ]; al_of_extension Types.ZX ])
  | V128 (V128.I32x4 (V128Op.ExtMulLowS)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I32"; numV 4L ]; nullary "LOW"; TupV [ nullary "I16"; numV 8L ]; al_of_extension Types.SX ])
  | V128 (V128.I32x4 (V128Op.ExtMulLowU)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I32"; numV 4L ]; nullary "LOW"; TupV [ nullary "I16"; numV 8L ]; al_of_extension Types.ZX ] )
  | V128 (V128.I64x2 (V128Op.ExtMulHighS)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I64"; numV 2L ]; nullary "HIGH"; TupV [ nullary "I32"; numV 4L ]; al_of_extension Types.SX ])
  | V128 (V128.I64x2 (V128Op.ExtMulHighU)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I64"; numV 2L ]; nullary "HIGH"; TupV [ nullary "I32"; numV 4L ]; al_of_extension Types.ZX ])
  | V128 (V128.I64x2 (V128Op.ExtMulLowS)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I64"; numV 2L ]; nullary "LOW"; TupV [ nullary "I32"; numV 4L ]; al_of_extension Types.SX ])
  | V128 (V128.I64x2 (V128Op.ExtMulLowU)) -> CaseV ("EXTMUL_HALF", [ TupV [ nullary "I64"; numV 2L ]; nullary "LOW"; TupV [ nullary "I32"; numV 4L ]; al_of_extension Types.ZX ] )
  | V128 (V128.I32x4 (V128Op.DotS)) -> CaseV ("DOT", [ TupV [ nullary "I32"; numV 4L]; TupV [ nullary "I16"; numV 8L ]; al_of_extension Types.SX ])
  | _ -> failwith "invalid special vbinop"

let al_of_int_vcvtop = function
  | V128Op.ExtendLowS -> Some (nullary "EXTEND", Some (nullary "LOW"), None, Some (nullary "S"), None)
  | V128Op.ExtendLowU -> Some (nullary "EXTEND", Some (nullary "LOW"), None, Some (nullary "U"), None)
  | V128Op.ExtendHighS -> Some (nullary "EXTEND", Some (nullary "HIGH"), None, Some (nullary "S"), None)
  | V128Op.ExtendHighU -> Some (nullary "EXTEND", Some (nullary "HIGH"), None, Some (nullary "U"), None)
  | V128Op.TruncSatSF32x4 -> Some (nullary "TRUNC_SAT", None, Some (TupV [ nullary "F32"; numV 4L ]), Some (nullary "S"), None)
  | V128Op.TruncSatUF32x4 -> Some (nullary "TRUNC_SAT", None, Some (TupV [ nullary "F32"; numV 4L ]), Some (nullary "U"), None)
  | V128Op.TruncSatSZeroF64x2 -> Some (nullary "TRUNC_SAT", None, Some (TupV [ nullary "F64"; numV 2L ]), Some (nullary "S"), Some (tupV []))
  | V128Op.TruncSatUZeroF64x2 -> Some (nullary "TRUNC_SAT", None, Some (TupV [ nullary "F64"; numV 2L ]), Some (nullary "U"), Some (tupV []))
  | _ -> None

let al_of_float32_vcvtop = function
  | V128Op.DemoteZeroF64x2 -> Some (nullary "DEMOTE", None, Some (TupV [ nullary "F64"; numV 2L ]), None, Some (tupV []))
  | V128Op.ConvertSI32x4 -> Some (nullary "CONVERT", None, Some (TupV [ nullary "I32"; numV 4L ]), Some (nullary "S"), None)
  | V128Op.ConvertUI32x4 -> Some (nullary "CONVERT", None, Some (TupV [ nullary "I32"; numV 4L ]), Some (nullary "U"), None)
  | _ -> None

let al_of_float64_vcvtop = function
  | V128Op.PromoteLowF32x4 -> Some (nullary "PROMOTE", Some (nullary "LOW"), Some (TupV [ nullary "F32"; numV 4L ]), None, None)
  | V128Op.ConvertSI32x4 -> Some (nullary "CONVERT", Some (nullary "LOW"), Some (TupV [ nullary "I32"; numV 4L ]), Some (nullary "S"), None)
  | V128Op.ConvertUI32x4 -> Some (nullary "CONVERT", Some (nullary "LOW"), Some (TupV [ nullary "I32"; numV 4L ]), Some (nullary "U"), None)
  | _ -> None

let al_of_vcvtop = function
  | V128 vop -> (
    match vop with
    | V128.I8x16 op -> (
      Option.map (fun (op', half, to_, ext, zero) ->
        let sh = match to_ with Some sh -> sh | None -> (
          match half with
          | Some _ -> failwith "invalid vcvtop"
          | None -> TupV [ nullary "I8"; numV 16L ]
        ) in
        [ TupV [ nullary "I8"; numV 16L ]; op'; optV half; sh; optV ext; CaseV ("ZERO", [OptV zero]) ]
      ) (al_of_int_vcvtop op)
    )
    | V128.I16x8 op -> (
      Option.map (fun (op', half, to_, ext, zero) ->
        let sh = match to_ with Some sh -> sh | None -> (
          match half with
          | Some _ -> TupV [ nullary "I8"; numV 16L ]
          | None -> TupV [ nullary "I16"; numV 8L ]
        ) in
        [ TupV [ nullary "I16"; numV 8L ]; op'; optV half; sh; optV ext; CaseV ("ZERO", [OptV zero]) ]
      ) (al_of_int_vcvtop op)
    )
    | V128.I32x4 op -> (
      Option.map (fun (op', half, to_, ext, zero) ->
        let sh = match to_ with Some sh -> sh | None -> (
          match half with
          | Some _ -> TupV [ nullary "I16"; numV 8L ]
          | None -> TupV [ nullary "I32"; numV 4L ]
        ) in
        [ TupV [ nullary "I32"; numV 4L ]; op'; optV half; sh; optV ext; CaseV ("ZERO", [OptV zero]) ]
      ) (al_of_int_vcvtop op)
    )
    | V128.I64x2 op -> (
      Option.map (fun (op', half, to_, ext, zero) ->
        let sh = match to_ with Some sh -> sh | None -> (
          match half with
          | Some _ -> TupV [ nullary "I32"; numV 4L ]
          | None -> TupV [ nullary "I64"; numV 2L ]
        ) in
        [ TupV [ nullary "I64"; numV 2L ]; op'; optV half; sh; optV ext; CaseV ("ZERO", [OptV zero]) ]
      ) (al_of_int_vcvtop op)
    )
    | V128.F32x4 op -> (
      Option.map (fun (op', half, to_, ext, zero) ->
        let sh = match to_ with Some sh -> sh | None -> (
          match half with
          | Some _ -> failwith "invalid vcvtop"
          | None -> TupV [ nullary "F32"; numV 4L ]
        ) in
        [ TupV [ nullary "F32"; numV 4L ]; op'; optV half; sh; optV ext; CaseV ("ZERO", [OptV zero]) ]
      ) (al_of_float32_vcvtop op)
    )
    | V128.F64x2 op -> (
      Option.map (fun (op', half, to_, ext, zero) ->
        let sh = match to_ with Some sh -> sh | None -> (
          match half with
          | Some _ -> TupV [ nullary "F32"; numV 4L ]
          | None -> TupV [ nullary "F64"; numV 2L ]
        ) in
        [ TupV [ nullary "F64"; numV 2L ]; op'; optV half; sh; optV ext; CaseV ("ZERO", [OptV zero]) ]
      ) (al_of_float64_vcvtop op)
    )
  )


let al_of_special_vcvtop = function
  | V128 (V128.I16x8 (V128Op.ExtAddPairwiseS)) -> CaseV ("EXTADD_PAIRWISE", [ TupV [ nullary "I16"; numV 8L]; TupV [ nullary "I8"; numV 16L ]; al_of_extension Types.SX ])
  | V128 (V128.I16x8 (V128Op.ExtAddPairwiseU)) -> CaseV ("EXTADD_PAIRWISE", [ TupV [ nullary "I16"; numV 8L]; TupV [ nullary "I8"; numV 16L ]; al_of_extension Types.ZX ])
  | V128 (V128.I32x4 (V128Op.ExtAddPairwiseS)) -> CaseV ("EXTADD_PAIRWISE", [ TupV [ nullary "I32"; numV 4L]; TupV [ nullary "I16"; numV 8L ]; al_of_extension Types.SX ])
  | V128 (V128.I32x4 (V128Op.ExtAddPairwiseU)) -> CaseV ("EXTADD_PAIRWISE", [ TupV [ nullary "I32"; numV 4L]; TupV [ nullary "I16"; numV 8L ]; al_of_extension Types.ZX ])
  | _ -> failwith "invalid special vcvtop"

let al_of_int_vshiftop : V128Op.ishiftop -> value = function
  | V128Op.Shl -> CaseV ("_VI", [ nullary "SHL" ])
  | V128Op.ShrS -> CaseV ("_VI", [ caseV ("SHR", [nullary "S"]) ])
  | V128Op.ShrU -> CaseV ("_VI", [ caseV ("SHR", [nullary "U"]) ])

let al_of_vshiftop = al_of_viop al_of_int_vshiftop

let al_of_vvtestop : vec_vtestop -> value list = function
  | V128 vop -> (
    match vop with
    | V128Op.AnyTrue ->
      [ nullary "V128"; CaseV ("_VV", [ nullary "ANY_TRUE" ]) ]
  )

let al_of_vvunop : vec_vunop -> value list = function
  | V128 vop -> (
    match vop with
    | V128Op.Not -> [ nullary "V128"; CaseV ("_VV", [ nullary "NOT" ]) ]
  )

let al_of_vvbinop : vec_vbinop -> value list = function
  | V128 vop -> (
    match vop with
    | V128Op.And -> [ nullary "V128"; CaseV ("_VV", [ nullary "AND" ]) ]
    | V128Op.Or -> [ nullary "V128"; CaseV ("_VV", [ nullary "OR" ]) ]
    | V128Op.Xor -> [ nullary "V128"; CaseV ("_VV", [ nullary "XOR" ]) ]
    | V128Op.AndNot -> [ nullary "V128"; CaseV ("_VV", [ nullary "ANDNOT" ]) ]
  )

let al_of_vvternop : vec_vternop -> value list = function
  | V128 vop -> (
    match vop with
    | V128Op.Bitselect ->
      [ nullary "V128"; CaseV ("_VV", [ nullary "BITSELECT" ]) ]
  )

let al_of_vsplatop : vec_splatop -> value list = function
  | V128 vop -> (
    match vop with
    | V128.I8x16 _ -> [ TupV [ nullary "I8"; numV 16L ] ]
    | V128.I16x8 _ -> [ TupV [ nullary "I16"; numV 8L ] ]
    | V128.I32x4 _ -> [ TupV [ nullary "I32"; numV 4L ] ]
    | V128.I64x2 _ -> [ TupV [ nullary "I64"; numV 2L ] ]
    | V128.F32x4 _ -> [ TupV [ nullary "F32"; numV 4L ] ]
    | V128.F64x2 _ -> [ TupV [ nullary "F64"; numV 2L ] ]
  )

let al_of_vextractop : vec_extractop -> value list = function
  | V128 vop -> (
    match vop with
    | V128.I8x16 vop' -> (
      match vop' with
      | Extract (n, ext) ->
        [ TupV [ nullary "I8"; numV 16L ]; optV (Some (al_of_extension ext)); al_of_int n; ]
    )
    | V128.I16x8 vop' -> (
      match vop' with
      | Extract (n, ext) ->
        [ TupV [ nullary "I16"; numV 8L ]; optV (Some (al_of_extension ext)); al_of_int n; ]
    )
    | V128.I32x4 vop' -> (
      match vop' with
      | Extract (n, _) -> [ TupV [ nullary "I32"; numV 4L ]; optV None; al_of_int n ]
    )
    | V128.I64x2 vop' -> (
      match vop' with
      | Extract (n, _) -> [ TupV [ nullary "I64"; numV 2L ]; optV None; al_of_int n ]
    )
    | V128.F32x4 vop' -> (
      match vop' with
      | Extract (n, _) -> [ TupV [ nullary "F32"; numV 4L ]; optV None; al_of_int n ]
    )
    | V128.F64x2 vop' -> (
      match vop' with
      | Extract (n, _) -> [ TupV [ nullary "F64"; numV 2L ]; optV None; al_of_int n ]
    )
  )

let al_of_vreplaceop : vec_replaceop -> value list = function
  | V128 vop -> (
    match vop with
    | V128.I8x16 (Replace n) -> [ TupV [ nullary "I8"; numV 16L ]; al_of_int n ]
    | V128.I16x8 (Replace n) -> [ TupV [ nullary "I16"; numV 8L ]; al_of_int n ]
    | V128.I32x4 (Replace n) -> [ TupV [ nullary "I32"; numV 4L ]; al_of_int n ]
    | V128.I64x2 (Replace n) -> [ TupV [ nullary "I64"; numV 2L ]; al_of_int n ]
    | V128.F32x4 (Replace n) -> [ TupV [ nullary "F32"; numV 4L ]; al_of_int n ]
    | V128.F64x2 (Replace n) -> [ TupV [ nullary "F64"; numV 2L ]; al_of_int n ]
  )

let al_of_pack_size = function
  | Types.Pack8 -> al_of_int 8
  | Types.Pack16 -> al_of_int 16
  | Types.Pack32 -> al_of_int 32
  | Types.Pack64 -> al_of_int 64

let al_of_pack_shape = function
  | Types.Pack8x8 -> CaseV ("PACKSHAPE", [NumV 8L; NumV 8L])
  | Types.Pack16x4 -> CaseV ("PACKSHAPE", [NumV 16L; NumV 4L])
  | Types.Pack32x2 -> CaseV ("PACKSHAPE", [NumV 32L; NumV 2L])

let al_of_extension = function
  | Types.SX -> nullary "S"
  | Types.ZX -> nullary "U"

let al_of_memop f memop =
  let str =
    Record.empty
    |> Record.add "ALIGN" (al_of_int memop.align)
    |> Record.add "OFFSET" (al_of_int32 memop.offset)
  in
  [ al_of_num_type memop.ty; f memop.pack ] @ [ StrV str ]

let al_of_pack_size_extension (p, s) = tupV [ al_of_pack_size p; al_of_extension s ]

let al_of_loadop = al_of_opt al_of_pack_size_extension |> al_of_memop

let al_of_storeop = al_of_opt al_of_pack_size |> al_of_memop

let al_of_vloadop vloadop =

  let str =
    Record.empty
    |> Record.add "ALIGN" (al_of_int vloadop.align)
    |> Record.add "OFFSET" (al_of_int32 vloadop.offset)
  in

  let vmemop = match vloadop.pack with
  | Option.Some (pack_size, vextension) -> (
    match vextension with
    | Types.ExtLane (pack_shape, extension) -> CaseV ("SHAPE", [ al_of_pack_shape pack_shape; al_of_extension extension; StrV str ])
    | Types.ExtSplat -> CaseV ("SPLAT", [ al_of_pack_size pack_size; StrV str ])
    | Types.ExtZero -> CaseV ("ZERO", [ al_of_pack_size pack_size; StrV str ])
  )
  | None -> CaseV ("LOAD", [ StrV str ]) in

  [ vmemop ] @ [zero](* TODO: update wasm2.0 spec@ al_of_memvar ()*)

let al_of_vstoreop vstoreop =
  let str =
    Record.empty
    |> Record.add "ALIGN" (al_of_int vstoreop.align)
    |> Record.add "OFFSET" (al_of_int32 vstoreop.offset)
  in

  (* TODO: update wasm2.0 @ al_of_memvar () *) zero :: [ StrV str; ]

let al_of_vlaneop vlaneop =
  let (vmemop, lanevar) = vlaneop in
  let pack_size = vmemop.pack in

  let str =
    Record.empty
    |> Record.add "ALIGN" (al_of_int vmemop.align)
    |> Record.add "OFFSET" (al_of_int32 vmemop.offset)
  in

  [ al_of_pack_size pack_size; ] @ [zero] (* TODO: ukpdate wasm2.0 al_of_memvar () *) @ [ StrV str; al_of_int lanevar ]

(* Construct instruction *)

let rec al_of_instr instr =
  match instr.it with
  (* wasm values *)
  | Const num -> al_of_num num.it
  | VecConst vec -> al_of_vec vec.it
  | RefNull rt -> CaseV ("REF.NULL", [ al_of_ref_type rt ])
  (* wasm instructions *)
  | Unreachable -> nullary "UNREACHABLE"
  | Nop -> nullary "NOP"
  | Drop -> nullary "DROP"
  | Unary op -> CaseV ("UNOP", al_of_unop op)
  | Binary op -> CaseV ("BINOP", al_of_binop op)
  | Test op -> CaseV ("TESTOP", al_of_testop op)
  | Compare op -> CaseV ("RELOP", al_of_relop op)
  | Convert op -> CaseV ("CVTOP", al_of_cvtop op)
  | VecTest vop -> CaseV ("VALL_TRUE", al_of_vtestop vop)
  | VecCompare vop -> CaseV ("VRELOP", al_of_vrelop vop)
  | VecUnary vop -> CaseV ("VUNOP", al_of_vunop vop)
  | VecBinary vop -> (match al_of_vbinop vop with Some l -> CaseV ("VBINOP", l) | None -> al_of_special_vbinop vop)
  | VecConvert vop -> (match al_of_vcvtop vop with Some l -> CaseV ("VCVTOP", l) | None -> al_of_special_vcvtop vop)
  | VecShift vop -> CaseV ("VISHIFTOP", al_of_vshiftop vop)
  | VecBitmask vop -> CaseV ("VBITMASK", al_of_vtestop vop)
  | VecTestBits vop -> CaseV ("VVTESTOP", al_of_vvtestop vop)
  | VecUnaryBits vop -> CaseV ("VVUNOP", al_of_vvunop vop)
  | VecBinaryBits vop -> CaseV ("VVBINOP", al_of_vvbinop vop)
  | VecTernaryBits vop -> CaseV ("VVTERNOP", al_of_vvternop vop)
  | VecSplat vop -> CaseV ("VSPLAT", al_of_vsplatop vop)
  | VecExtract vop -> CaseV ("VEXTRACT_LANE", al_of_vextractop vop)
  | VecReplace vop -> CaseV ("VREPLACE_LANE", al_of_vreplaceop vop)
  | RefIsNull -> nullary "REF.IS_NULL"
  | RefFunc var -> CaseV ("REF.FUNC", [ al_of_var var ])
  | Select vtl_opt -> CaseV ("SELECT", [ al_of_opt (al_of_list al_of_val_type) vtl_opt ])
  | LocalGet var -> CaseV ("LOCAL.GET", [ al_of_var var ])
  | LocalSet var -> CaseV ("LOCAL.SET", [ al_of_var var ])
  | LocalTee var -> CaseV ("LOCAL.TEE", [ al_of_var var ])
  | GlobalGet var -> CaseV ("GLOBAL.GET", [ al_of_var var ])
  | GlobalSet var -> CaseV ("GLOBAL.SET", [ al_of_var var ])
  | TableGet var -> CaseV ("TABLE.GET", [ al_of_var var ])
  | TableSet var -> CaseV ("TABLE.SET", [ al_of_var var ])
  | TableSize var -> CaseV ("TABLE.SIZE", [ al_of_var var ])
  | TableGrow var -> CaseV ("TABLE.GROW", [ al_of_var var ])
  | TableFill var -> CaseV ("TABLE.FILL", [ al_of_var var ])
  | TableCopy (var1, var2) -> CaseV ("TABLE.COPY", [ al_of_var var1; al_of_var var2 ])
  | TableInit (var1, var2) -> CaseV ("TABLE.INIT", [ al_of_var var1; al_of_var var2 ])
  | ElemDrop var -> CaseV ("ELEM.DROP", [ al_of_var var ])
  | Block (bt, instrs) ->
    CaseV ("BLOCK", [ al_of_blocktype bt; al_of_list al_of_instr instrs ])
  | Loop (bt, instrs) ->
    CaseV ("LOOP", [ al_of_blocktype bt; al_of_list al_of_instr instrs ])
  | If (bt, instrs1, instrs2) ->
    CaseV ("IF", [
      al_of_blocktype bt;
      al_of_list al_of_instr instrs1;
      al_of_list al_of_instr instrs2;
    ])
  | Br var -> CaseV ("BR", [ al_of_var var ])
  | BrIf var -> CaseV ("BR_IF", [ al_of_var var ])
  | BrTable (vars, var) ->
    CaseV ("BR_TABLE", [ al_of_list al_of_var vars; al_of_var var ])
  | Return -> nullary "RETURN"
  | Call var -> CaseV ("CALL", [ al_of_var var ])
  | CallIndirect (var1, var2) ->
    let args = al_with_version [ 2; 3 ] al_of_var var1 @ [ al_of_var var2 ] in
    CaseV ("CALL_INDIRECT", args)
  | Load loadop -> CaseV ("LOAD", al_of_loadop loadop)
  | Store storeop -> CaseV ("STORE", al_of_storeop storeop)
  | VecLoad vloadop -> CaseV ("VLOAD", al_of_vloadop vloadop)
  | VecLoadLane vlaneop -> CaseV ("VLOAD_LANE", al_of_vlaneop vlaneop)
  | VecStore vstoreop -> CaseV ("VSTORE", al_of_vstoreop vstoreop)
  | VecStoreLane vlaneop -> CaseV ("VSTORE_LANE", al_of_vlaneop vlaneop)
  | MemorySize -> CaseV ("MEMORY.SIZE", [])
  | MemoryGrow -> CaseV ("MEMORY.GROW", [])
  | MemoryFill -> CaseV ("MEMORY.FILL", [])
  | MemoryCopy -> CaseV ("MEMORY.COPY", [])
  | MemoryInit i32 -> CaseV ("MEMORY.INIT", [ al_of_var i32 ])
  | DataDrop var -> CaseV ("DATA.DROP", [ al_of_var var ])
  (* | _ -> CaseV ("TODO: Unconstructed Wasm instruction (al_of_instr)", []) *)

let al_of_const const = al_of_list al_of_instr const.it


(* Construct module *)

let al_of_type ty =
  let FuncType (rt1, rt2) = ty.it in
  TupV [al_of_result_type rt1; al_of_result_type rt2]

let al_of_func func =
  CaseV ("FUNC", [
    al_of_var func.it.ftype;
    al_of_list al_of_val_type func.it.locals;
    al_of_list al_of_instr func.it.body;
  ])

let al_of_global global =
  CaseV ("GLOBAL", [
    al_of_global_type global.it.gtype;
    al_of_const global.it.ginit;
  ])

let al_of_table table = CaseV ("TABLE", [ al_of_table_type table.it.ttype ])

let al_of_memory memory =
  let arg = al_of_memory_type memory.it.mtype in
  let arg' =
    if !version = 1 then
      arg_of_case "I8" 0 arg
    else arg
  in
  CaseV ("MEMORY", [ arg' ])

let al_of_segment segment =
  match segment.it with
  | Passive -> nullary "PASSIVE"
  | Active { index; offset } ->
    CaseV ("ACTIVE", [ al_of_var index; al_of_const offset ])
  | Declarative -> nullary "DECLARE"

let al_of_elem elem =
  if !version = 1 then
    CaseV ("ELEM", [
      al_of_segment elem.it.emode |> arg_of_case "ACTIVE" 1;
      al_of_list al_of_const elem.it.einit
      |> unwrap_listv_to_list
      |> List.map (fun expr -> expr |> unwrap_listv_to_list |> List.hd |> (arg_of_case "REF.FUNC" 0))
      |> listV_of_list;
    ])
  else
    CaseV ("ELEM", [
      al_of_ref_type elem.it.etype;
      al_of_list al_of_const elem.it.einit;
      al_of_segment elem.it.emode;
    ])

let al_of_data data =
  let seg = al_of_segment data.it.dmode in
  let bytes_ = al_of_bytes data.it.dinit in
  if !version = 1 then
    CaseV ("DATA", [ arg_of_case "ACTIVE" 1 seg; bytes_ ])
  else
    CaseV ("DATA", [ bytes_; seg ])

let al_of_import_desc idesc =
  match idesc.it with
  | FuncImport x -> CaseV ("FUNC", [ al_of_var x ])
  | TableImport tt -> CaseV ("TABLE", [ al_of_table_type tt ])
  | MemoryImport mt -> CaseV ("MEM", [ al_of_memory_type mt ])
  | GlobalImport gt -> CaseV ("GLOBAL", [ al_of_global_type gt ])

let al_of_import import =
  CaseV ("IMPORT", [
    al_of_name import.it.module_name;
    al_of_name import.it.item_name;
    al_of_import_desc import.it.idesc;
  ])

let al_of_export_desc export_desc = match export_desc.it with
  | FuncExport var -> CaseV ("FUNC", [ al_of_var var ])
  | TableExport var -> CaseV ("TABLE", [ al_of_var var ])
  | MemoryExport var -> CaseV ("MEM", [ al_of_var var ])
  | GlobalExport var -> CaseV ("GLOBAL", [ al_of_var var ])

let al_of_start start = CaseV ("START", [ al_of_var start.it.sfunc ])

let al_of_export export =
  CaseV ("EXPORT", [ al_of_name export.it.name; al_of_export_desc export.it.edesc ])

let al_of_module module_ =
  CaseV ("MODULE", [
    al_of_list al_of_type module_.it.types;
    al_of_list al_of_import module_.it.imports;
    al_of_list al_of_func module_.it.funcs;
    al_of_list al_of_global module_.it.globals;
    al_of_list al_of_table module_.it.tables;
    al_of_list al_of_memory module_.it.memories;
    al_of_list al_of_elem module_.it.elems;
    al_of_list al_of_data module_.it.datas;
    al_of_opt al_of_start module_.it.start;
    al_of_list al_of_export module_.it.exports;
  ])

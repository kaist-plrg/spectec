open Al
open Ast
open Al_util
open Backend_interpreter
open Utils

let types   = ref (listV [])
let imports = ref (listV [])
let funcs   = ref (listV [])
let globals = ref (listV [])
let tables  = ref (listV [])
let mems    = ref (listV [])
let elems   = ref (listV [])
let datas   = ref (listV [])
let starts  = ref (listV [])
let exports = ref (listV [])

(** Hardcoded patches to make module valid **)
(* TODO: Remove these hardcodenesses *)

(* type *)
let patch_types types =
  let arrow = ArrowV (listV [], listV []) in
  let dt =
    case_vv "REC" ([
      case_vvv "SUBD"
        (non_empty "FINAL")
        (listV [])
        arrow
    ] |> listV) zero
    |> case_v "DEF" in
  let t = case_v "TYPE" (if (!Construct.version = 3) then dt else arrow) in
  t :: (al_to_list types) |> listV

(* import *)
let patch_imports _imports =
  let mk_import name kind t = Al.Ast.CaseV ("IMPORT", [ TextV "spectest"; TextV name; case_v kind t ]) in
  listV [
    mk_import "print" "FUNC" zero;
    mk_import "global_i32" "GLOBAL" (TupV [empty "MUT"; singleton "I32"]);
    mk_import "global_i64" "GLOBAL" (TupV [empty "MUT"; singleton "I64"]);
    mk_import "global_f32" "GLOBAL" (TupV [empty "MUT"; singleton "F32"]);
    mk_import "global_f64" "GLOBAL" (TupV [empty "MUT"; singleton "F64"]);
    (* mk_import "table" "TABLE" (TupV [ TupV [ NumV 10L; NumV 20L ]; singleton "FUNCREF" ]); *)
    (* mk_import "memory" "MEM" (CaseV ("I8", [ TupV [ NumV 1L; NumV 2L ] ])); *)
  ]

(* func *)
let fix_local_idx = function
  | CaseV ("FUNC", [ tid; locals; expr ]) ->
    let n =
      (  arg_of_list (al_to_int tid) !types
      |> arg_of_case "TYPE" 0
      |> arg_of_tup 0
      |> al_to_list
      |> List.length )
    + (  locals
      |> al_to_list
      |> List.length )
    in
    let fix_local_id' = walk_value (function
    | CaseV (("LOCAL.GET" | "LOCAL.SET" | "LOCAL.TEE") as case, [ lid ]) ->
      let i = al_to_int lid in
      let i' = if i < n then i else if n > 0 then Random.int n else 0 in
      CaseV (case, [ numV i' ])
    | v -> v) in
    CaseV ("FUNC", [ tid; locals; fix_local_id' expr ])
  | v -> v
let inc_type_idx = function (* Due to manually added 0th type, [] -> [] *)
  | CaseV ("FUNC", tid :: args) -> CaseV ("FUNC", (add_num tid one) :: args)
  | v -> v
let patch_func func = func
  |> fix_local_idx
  |> inc_type_idx
let patch_funcs funcs = listv_map patch_func funcs

(* global *)
let patch_global global = global
let patch_globals globals = listv_map patch_global globals

(* table *)
let patch_table table = table
let patch_tables tables = listv_map patch_table tables

(* mem *)
let patch_mem mem = mem
let patch_mems mems = listV ( match al_to_list mems with
| [] -> []
| mem :: _ -> [ patch_mem mem ] )

(* elem *)
let patch_mode rt = function
  | CaseV ("ACTIVE", tid :: _args) as v ->
    let tid' = al_to_int tid in
    let rt' =
      arg_of_list tid' !tables
      |> arg_of_case "TABLE" 0
      |> arg_of_tup 1 in
    if (rt = rt') then v else choose [ singleton "PASSIVE"; singleton "DECLARE" ]
  | v -> v
let patch_elem = function
  | CaseV ("ELEM", [ rt; e; mode ]) -> CaseV ("ELEM", [ rt; e; patch_mode rt mode ])
  | v -> v
let patch_elems elems = listv_map patch_elem elems

(* data *)
let patch_data data = data
let patch_datas datas = listv_map patch_data datas

(* start *)
let patch_start start = start
let patch_starts starts = starts

(* export *)
let dedup_names exports =
  let rec dedup_names' names es = match es with
  | [] -> []
  | e :: es' ->
    let name = arg_of_case "EXPORT" 0 e |> al_to_string in
    if contains name names then
      let name' = TextV (name ^ "'") in
      let e' = replace_case "EXPORT" 0 name' e in
      dedup_names' names (e' :: es')
    else
      let names' = name :: names in
      e :: dedup_names' names' es'
  in
  dedup_names' [] (al_to_list exports) |> listV
let patch_export export = export
let patch_exports exports = listv_map patch_export exports
  |> dedup_names

(* module *)
let patch_module module_ =
  let nth_arg n = arg_of_case "MODULE" n module_ in

  types   := nth_arg 0;
  imports := nth_arg 1;
  funcs   := nth_arg 2;
  globals := nth_arg 3;
  tables  := nth_arg 4;
  mems    := nth_arg 5;
  elems   := nth_arg 6;
  datas   := nth_arg 7;
  starts  := nth_arg 8;
  exports := nth_arg 9;

  let types'  = patch_types !types in
  let imports'= patch_imports !imports in
  let funcs'  = patch_funcs !funcs in
  let globals'= patch_globals !globals in
  let tables' = patch_tables !tables in
  let mems'   = patch_mems !mems in
  let elems'  = patch_elems !elems in
  let datas'  = patch_datas !datas in
  let starts' = patch_starts !starts in
  let exports'= patch_exports !exports in

  CaseV ("MODULE", [
    types';
    imports';
    funcs';
    globals';
    tables';
    mems';
    elems';
    datas';
    starts';
    exports';
  ])

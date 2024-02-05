open Al
open Ast
open Al_util
open Utils

let types   = ref empty_list
let imports = ref empty_list
let funcs   = ref empty_list
let globals = ref empty_list
let tables  = ref empty_list
let mems    = ref empty_list
let elems   = ref empty_list
let datas   = ref empty_list
let starts  = ref empty_list
let exports = ref empty_list

(** Hardcoded patches to make module valid **)
(* TODO: Remove these hardcodenesses *)

(* type *)
let patch_types types =
  let type_ = caseV ("TYPE", [tupV [empty_list; empty_list]]) in
  listV (Array.append [|type_|] (unwrap_listv_to_array types))

(* import *)
let patch_imports _imports =
  let mk_import name kind t = caseV ("IMPORT", [ TextV "spectest"; TextV name; caseV (kind, [t])]) in
  listV [|
    (* mk_import "print" "FUNC" zero; *)
    mk_import "global_i32" "GLOBAL" (TupV [some "MUT"; nullary "I32"]);
    mk_import "global_i64" "GLOBAL" (TupV [some "MUT"; nullary "I64"]);
    mk_import "global_f32" "GLOBAL" (TupV [some "MUT"; nullary "F32"]);
    mk_import "global_f64" "GLOBAL" (TupV [some "MUT"; nullary "F64"]);
    (* mk_import "table" "TABLE" (TupV [ TupV [ NumV 10L; NumV 20L ]; nullary "FUNCREF" ]); *)
    (* mk_import "memory" "MEM" (CaseV ("I8", [ TupV [ NumV 1L; NumV 2L ] ])); *)
  |]

(* func *)
let inc_type_idx = function (* Due to manually added 0th type, [] -> [] *)
  | CaseV ("FUNC", tid :: args) ->
    let add = map2 unwrap_numv numV Int64.add in
    let inc_type_idx' =
      walk_value (function
        | CaseV ("_IDX", [tid]) -> caseV ("_IDX", [add tid one])
        | v -> v
      )
    in
    CaseV ("FUNC", (add tid one) :: List.map inc_type_idx' args)
  | v -> v
let patch_func func = func
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
let patch_mems mems =
  match unwrap_listv_to_list mems with
  | [] -> empty_list
  | mem :: _ -> singleton (patch_mem mem)

(* elem *)
let patch_mode rt = function
  | CaseV ("ACTIVE", tid :: _args) as v ->
    let tid' = unwrap_numv_to_int tid in
    let table = List.nth (unwrap_listv_to_list !tables) tid' in
    let rt' = List.nth (unwrap_tupv (casev_nth_arg 0 table)) 1 in
    if (rt = rt') then v else choose [ nullary "PASSIVE"; nullary "DECLARE" ]
  | v -> v
let patch_elem = function
  | CaseV ("ELEM", [ rt; e; mode ]) -> CaseV ("ELEM", [ rt; e; patch_mode rt mode ])
  | v -> v
let patch_elems elems = listv_map patch_elem elems

(* data *)
let patch_data data = data
let patch_datas datas = listv_map patch_data datas

(* start *)
let patch_start start =
  let fid = start |> casev_nth_arg 0 |> unwrap_numv_to_int in
  let fs = !funcs |> unwrap_listv_to_list in
  let is_ok f =
    let tid = casev_nth_arg 0 f |> unwrap_numv_to_int in
    let t = (try List.nth (!types |> unwrap_listv_to_list) tid with _ -> failwith (string_of_int tid)) |> casev_nth_arg 0 in
    t = tupV [empty_list; empty_list] in
  let f = List.nth fs fid in
  if is_ok f then Some start else
  let candidates = find_index_all is_ok fs in
  if candidates = [] then None else
    Some (caseV ("START", [numV_of_int (choose candidates)]))
let patch_starts starts = match starts with
| OptV None -> starts
| OptV Some start -> OptV (patch_start start)
| _ -> starts

(* export *)
let dedup_names exports =
  let rec dedup_names' names es = match es with
  | [] -> []
  | e :: es' ->
    let name = casev_nth_arg 0 e |> unwrap_textv in
    if List.mem name names then
      let name' = TextV (name ^ "'") in
      let e' = casev_replace_nth_arg 0 name' e in
      dedup_names' names (e' :: es')
    else
      let names' = name :: names in
      e :: dedup_names' names' es'
  in
  dedup_names' [] (unwrap_listv_to_list exports) |> listV_of_list
let patch_export export = export
let patch_exports exports = listv_map patch_export exports
  |> dedup_names

(* module *)
let patch_module module_ =
  let nth_arg n = casev_nth_arg n module_ in

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

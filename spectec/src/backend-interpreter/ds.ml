open Al
open Ast
open Al_util
open Print
open Util
open Source


(* Map *)

module InfoMap = Map.Make (Int)
module Env = Map.Make (String)
module Map = Map.Make (String)


(* Program *)

type rule_map = algorithm Map.t
type func_map = algorithm Map.t

let rule_map: rule_map ref = ref Map.empty
let func_map: func_map ref = ref Map.empty

let to_map algos =
  let f acc algo =
    let rmap, fmap = acc in
    match algo with
    | RuleA ((name, _), _, _) -> Map.add name algo rmap, fmap
    | FuncA (name, _, _) -> rmap, Map.add name algo fmap
  in
  List.fold_left f (Map.empty, Map.empty) algos

let bound_rule name = Map.mem name !rule_map
let bound_func name = Map.mem name !func_map

let lookup_algo name =
  if bound_rule name then
    Map.find name !rule_map
  else if bound_func name then
    Map.find name !func_map
  else failwith ("Algorithm not found: " ^ name)


(* Store *)

let _store : store ref = ref Record.empty
let get_store () = !_store


(* Environment *)

type env = value Env.t

let string_of_env env =
  "\n{" ^
  Print.string_of_list
    (fun (k, v) ->
      k ^ ": " ^ Print.string_of_value v)
    ",\n  "
    (Env.bindings env) ^
  "\n}"

let lookup_env key env =
  try Env.find key env
  with Not_found ->
    Printf.sprintf "The key '%s' is not in the map: %s."
      key (string_of_env env)
    |> prerr_endline;
    raise Not_found

let add_store = Env.add "s" (Ast.StoreV _store)


(* Info *)

module Info = struct
  type info = { algo_name: string; instr: instr; mutable covered: bool }
  let info_map : info InfoMap.t ref = ref InfoMap.empty

  let make_info algo_name instr =
    { algo_name; instr; covered = false }

  let rec partition_by_algo info_map =
    match InfoMap.choose_opt info_map with
    | None -> []
    | Some (_, info) ->
      let f _ v = v.algo_name = info.algo_name in
      let im1, im2 = InfoMap.partition f info_map in
      im1 :: partition_by_algo im2

  let print () =
    partition_by_algo !info_map
    |> List.iter
      (fun im ->
        let _, v = InfoMap.choose im in
        Printf.printf "\n[%s]\n" v.algo_name;
        InfoMap.iter
          (fun _ v' ->
            Al.Print.string_of_instr v'.instr
            |> print_endline)
          im)

  let add k i = info_map := InfoMap.add k i !info_map

  let find k = InfoMap.find k !info_map
end


(* Register *)

module Register = struct
  let _register : value Map.t ref = ref Map.empty
  let _latest = ""

  let add name moduleinst = _register := Map.add name moduleinst !_register

  let add_with_var var moduleinst =
    let open Reference_interpreter.Source in
    add _latest moduleinst;
    match var with
    | Some name -> add name.it moduleinst
    | _ -> ()

  let find name = Map.find name !_register

  let get_module_name var =
    let open Reference_interpreter.Source in
    match var with
    | Some name -> name.it
    | None -> _latest
end


(* AL Context *)

module AlContext = struct
  (* TODO: Change name *)
  type return_value =
    | Bot
    | None
    | Some of value

  type t = string * env * return_value * int

  let context_stack: t list ref = ref []
  let context_stack_length = ref 0

  let create_context name = name, Env.empty, Bot, 0

  let init_context () =
    context_stack := [];
    context_stack_length := 0

  let push_context ctx =
    context_stack := ctx :: !context_stack;
    context_stack_length := 1 + !context_stack_length

  let pop_context () =
    context_stack_length := !context_stack_length - 1;
    match !context_stack with
    | h :: t -> context_stack := t; h
    | _ -> failwith "AL context stack underflow"

  let get_context () =
    match !context_stack with
    | h :: _ -> h
    | _ -> failwith "AL context stack underflow"

  let get_name () =
    let name, _, _, _ = get_context () in
    name

  (* Print *)

  let string_of_return_value = function
    | Bot -> "⊥"
    | None -> "None"
    | Some v -> string_of_value v

  let string_of_context ctx =
    let name, _, return_value, depth = ctx in
    Printf.sprintf "(%s, %s, %s)"
      name
      (string_of_return_value return_value)
      (string_of_int depth)

  let string_of_context_stack () =
    List.fold_left
      (fun acc ctx -> (string_of_context ctx) ^ " :: " ^ acc)
      "[]" !context_stack

  (* Env *)

  let set_env env =
    let name, _, return_value, depth = pop_context () in
    push_context (name, env, return_value, depth)

  let update_env n v =
    let name, env, return_value, depth = pop_context () in
    push_context (name, Env.add n v env, return_value, depth)

  let get_env () =
    let _, env, _, _ = get_context () in
    env

  (* Return value *)

  let set_return_value v =
    let name, env, return_value, depth = pop_context () in
    assert (return_value = Bot);
    push_context (name, env, Some v, depth)

  let set_return () =
    let name, env, return_value, depth = pop_context () in
    assert (return_value = Bot);
    push_context (name, env, None, depth)

  let get_return_value () =
    let _, _, return_value, _ = get_context () in
    return_value

  (* Depth *)

  let get_depth () =
    let _, _, _, depth = get_context () in
    depth

  let increase_depth () =
    let name, env, return_value, depth = pop_context () in
    push_context (name, env, return_value, depth + 1)

  let rec decrease_depth () =
    let name, env, return_value, depth = pop_context () in
    if depth > 0 then
      push_context (name, env, return_value, depth - 1)
    else (
      decrease_depth ();
      push_context (name, env, return_value, depth)
    )

end


(* Wasm Context *)

module WasmContext = struct
  type t = value * value list * value list

  let top_level_context = TextV "TopLevelContexet", [], []
  let context_stack: t list ref = ref [top_level_context]

  let get_context () =
    match !context_stack with
    | h :: _ -> h
    | _ -> failwith "Wasm context stack underflow"

  let init_context () = context_stack := [top_level_context]

  let push_context ctx = context_stack := ctx :: !context_stack

  let pop_context () =
    match !context_stack with
    | h :: t -> context_stack := t; h
    | _ -> failwith "Wasm context stack underflow"

  (* Print *)

  let string_of_context ctx =
    let v, vs, vs_instr = ctx in
    Printf.sprintf "(%s, %s, %s)"
      (string_of_value v)
      (string_of_list string_of_value ", " vs)
      (string_of_list string_of_value ", " vs_instr)

  let string_of_context_stack () =
    List.fold_left
      (fun acc ctx -> (string_of_context ctx) ^ " :: " ^ acc)
      "[]" !context_stack

  (* Context *)

  let get_value_with_condition f =
    match List.find_opt (fun (v, _, _) -> f v) !context_stack with
    | Some (v, _, _) -> v
    | None -> failwith "Wasm context stack underflow"

  let get_current_context () =
    let ctx, _, _ = get_context () in
    ctx

  let get_current_frame () =
    let match_frame = function
      | FrameV _ -> true
      | _ -> false
    in get_value_with_condition match_frame

  let get_module_instance () =
    match get_current_frame () with
    | FrameV (_, mm) -> mm
    | _ -> failwith "Invalid frame"

  let get_current_label () =
    let match_label = function
      | LabelV _ -> true
      | _ -> false
    in get_value_with_condition match_label

  (* Value stack *)

  let is_value = function
    | CaseV ("CONST", _) -> true
    | CaseV ("VVCONST", _) -> true
    | CaseV (ref, _)
      when String.starts_with ~prefix:"REF." ref -> true
    | _ -> false

  let get_value_stack () =
    let _, vs, _ = get_context () in
    vs

  let push_value v =
    let v_ctx, vs, vs_instr = pop_context () in
    if is_value v then
      push_context (v_ctx, v :: vs, vs_instr)
    else
      string_of_value v
      |> Printf.sprintf "%s is not a Wasm value"
      |> failwith

  let pop_value () =
    let v_ctx, vs, vs_instr = pop_context () in
    match vs with
    | h :: t -> push_context (v_ctx, t, vs_instr); h
    | _ -> failwith "Wasm value stack underflow"

  (* Instr stack *)

  let pop_instr () =
    let v_ctx, vs, vs_instr = pop_context () in
    match vs_instr with
    | h :: t -> push_context (v_ctx, vs, t); h
    | _ -> failwith "Wasm instr stack underflow"
end


(* Initialization *)

let init algos =
  (* Initialize info_map *)
  let init_info algo =
    let algo_name = get_name algo in
    let config = {
      Walk.default_config with pre_instr =
        (fun i ->
          let info = Info.make_info algo_name i in
          Info.add i.note info;
          [i])
    } in
    Walk.walk config algo
  in
  List.map init_info algos |> ignore;

  (* Initialize algo_map *)

  let rmap, fmap = to_map algos in
  rule_map := rmap;
  func_map := fmap;

  (* Initialize store *)
  _store :=
    Record.empty
    |> Record.add "FUNC" (listV [||])
    |> Record.add "GLOBAL" (listV [||])
    |> Record.add "TABLE" (listV [||])
    |> Record.add "MEM" (listV [||])
    |> Record.add "ELEM" (listV [||])
    |> Record.add "DATA" (listV [||])
    |> Record.add "STRUCT" (listV [||])
    |> Record.add "ARRAY" (listV [||])

open Ast

(* Smart Constructors *)

let _nid_count = ref 0
let gen_nid () =
  let nid = !_nid_count in
  _nid_count := nid + 1;
  nid

let mk_node it = { it; nid = gen_nid () }

let ifI (c, il1, il2) = IfI (c, il1, il2) |> mk_node
let eitherI (il1, il2) = EitherI (il1, il2) |> mk_node
let enterI (e1, e2, il) = EnterI (e1, e2, il) |> mk_node
let assertI c = AssertI c |> mk_node
let pushI e = PushI e |> mk_node
let popI e = PopI e |> mk_node
let popallI e = PopAllI e |> mk_node
let letI (e1, e2) = LetI (e1, e2) |> mk_node
let trapI = TrapI |> mk_node
let nopI = NopI |> mk_node
let returnI e_opt = ReturnI e_opt |> mk_node
let executeI e = ExecuteI e |> mk_node
let executeseqI e = ExecuteSeqI e |> mk_node
let performI (id, el) = PerformI (id, el) |> mk_node
let exitI = ExitI |> mk_node
let replaceI (e1, p, e2) = ReplaceI (e1, p, e2) |> mk_node
let appendI (e1, e2) = AppendI (e1, e2) |> mk_node
let otherwiseI il = OtherwiseI il |> mk_node
let yetI s = YetI s |> mk_node

let id str = VarE str 

let singleton x = CaseV (String.uppercase_ascii x, [])
let listV l = ListV (ref (Array.of_list l))
let zero = NumV 0L
let one = NumV 1L
let case_v case v = CaseV (case, [ v ])
let case_vv case v1 v2 = CaseV (case, [ v1; v2 ])
let case_vvv case v1 v2 v3 = CaseV (case, [ v1; v2; v3 ])
let numV i = NumV (i |> Int64.of_int)
let optV_none = OptV None
let optV_some v = OptV (Some v)
let empty x = case_v x optV_none
let non_empty x = case_v x (optV_some (TupV []))


(* Smart getters *)
let arg_of_case case i = function
| CaseV (case', args) when case = case' -> List.nth args i
| _ -> failwith "invalid arg_of_case"

let arg_of_tup i = function
| TupV args -> List.nth args i
| ArrowV (v0, _) when i = 0 -> v0
| ArrowV (_, v1) when i = 1 -> v1
| v -> failwith ("invalid arg_of_tup " ^ Print.string_of_value v ^ "." ^ string_of_int i)

let arg_of_list i = function
| ListV args -> Array.get !args i
| v -> failwith ("invalid arg_of_list " ^ Print.string_of_value v ^ "." ^ string_of_int i)

let field_of_str k = function
| StrV record -> Util.Record.Record.find k record
| _ -> failwith "invalid field_of_str"

let is_case case = function
| CaseV (case', _) -> case = case'
| _ -> false

let case_of_case = function
| CaseV (case, _) -> case
| _ -> failwith "invalid case_of_case"

let get_name = function
  | RuleA ((name, _), _, _) -> name
  | FuncA (name, _, _) -> name

let get_param = function
  | RuleA (_, params, _) -> params
  | FuncA (_, params, _) -> params

let get_body = function
  | RuleA (_, _, body) -> body
  | FuncA (_, _, body) -> body

(* Smart converters *)
let al_to_list = function
| ListV a -> Array.to_list !a
| _ -> failwith "invalid al_to_list"

let al_to_string = function
| TextV s -> s
| _ -> failwith "invalid al_to_string"

let al_to_int = function
| NumV i64 -> Int64.to_int i64
| _ -> failwith "invalid al_to_int"

let listv_map f = function
| ListV a -> ListV (Array.map f !a |> ref)
| _ -> failwith "invalid listv_map"

let replace_case case i v = function
| CaseV (case', args) when case = case' -> CaseV (case', List.mapi (fun i' v' -> if i' = i then v else v') args)
| _ -> failwith "invalid arg_of_case"

(* Smart operators *)
let add_num v1 v2 = match v1, v2 with
| NumV i64, NumV i64' -> NumV (Int64.add i64 i64')
| _ -> failwith "invalid add_num"

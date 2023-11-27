open Flag
open Util.Source
open Util.Record

(* Specs *)
let el: El.Ast.script ref = ref []
let il: Il.Ast.script ref = ref []
let al: Al.Ast.script ref = ref []

(* Helpers *)
let choose l =
  let n = List.length l in
  assert (n > 0);
  let i = Random.int n in
  List.nth l i

let contains x = List.exists (fun x' -> x = x')

let flatten_rec = List.concat_map (fun def ->
  match def.it with
  | Il.Ast.RecD defs -> defs
  | _ -> [ def ]
)

let find_syn name =
  let syn_opt = List.find_map (fun def -> match def.it with
  | Il.Ast.SynD (id, deftyp) when id.it = name -> Some deftyp
  | _ -> None ) !il in
  match syn_opt with
  | Some syn -> syn
  | None -> failwith (Printf.sprintf "The syntax named %s does not exist in the input spec" name)

let listV l = Al.Ast.ListV (l |> Array.of_list |> ref)
let numV i = Al.Ast.NumV (i |> Int64.of_int)
let optV_none = Al.Ast.OptV None
let optV_some v = Al.Ast.OptV (Some v)

let string_of_atom = Il.Print.string_of_atom
let string_of_module m = match m with
| Al.Ast.CaseV ("MODULE", args) ->
  "(MODULE\n  " ^ (List.map Al.Print.string_of_value args |> String.concat "\n  ") ^ "\n)"
| _ -> failwith "Unreachable"

let push v s = s := v :: !s
let pop s = s := List.tl !s
let top s = List.hd !s

(** Initialize **)

type dim = Exactly of int | AtLeast of int

let dims = ref Record.empty

let print_dims () =
  let string_of_dim = function
    | Exactly v -> string_of_int v
    | AtLeast v -> string_of_int v ^ "+" in
  Record.iter (fun (k, v) ->
    let (v1, v2) = v in
    Printf.sprintf "%s : %s -> %s" k (string_of_dim v1) (string_of_dim v2) |> print_endline
  ) !dims

let estimate_dim () = Il.Ast.(
  let rec get_dim e = match e.it with
  | ListE es -> Exactly (List.length es)
  | VarE _ -> Exactly 1
  | IterE (_, (Il.Ast.List, _)) -> AtLeast 0
  | CatE (e1, e2) -> ( match get_dim e1, get_dim e2 with
    | Exactly n1, Exactly n2 -> Exactly (n1 + n2)
    | Exactly n1, AtLeast n2
    | AtLeast n1, Exactly n2
    | AtLeast n1, AtLeast n2 -> AtLeast (n1 + n2) )
  | _ -> failwith "unexpected type, can not get dimension of it" in

  let expected e kind =
    Printf.sprintf "Expected %s to be %s" (Il.Print.string_of_exp e) kind |> failwith
  in

  List.iter (fun def -> match def.it with
    | RelD (id, _, _, rules) when id.it = "Instr_ok" || id.it = "Instrf_ok" ->
      List.iter (fun rule -> match rule.it with
        | RuleD (id, _, _, exp, _) -> ( match exp.it with
          | TupE [_c; _lhs; rhs] -> ( match rhs.it with
            | MixE (_, args) -> ( match args.it with
              | TupE [t1; t2] | TupE [t1; _; t2] ->
                let name = String.split_on_char '-' id.it |> List.hd |> String.uppercase_ascii in
                let n1 = get_dim t1 in
                let n2 = get_dim t2 in
                dims := Record.add name (n1, n2) !dims;
              | _ -> expected args "t1, t2 or t1, x*, t2" )
            | _ -> expected rhs "e1 -> e2" )
          | _ -> expected exp "C |- lhs : rhs" )
      ) rules
    | _ -> ()
  ) !il
)

let dim_stack = ref []
let update_dim diff =
  match !dim_stack with
  | [] -> failwith "dim_stack is empty"
  | (cur, target) :: tl -> dim_stack := (cur + diff, target) :: tl

let fix_n = function
| Exactly n -> n
| AtLeast n -> n + Random.int 3

let consts = ref []
let const_ctxs = ref []
let estimate_const () = Il.Ast.(
  List.iter (fun def -> match def.it with
    | RelD (id, _, _, rules) when id.it = "Instr_const" ->
      List.iter (fun rule -> match rule.it with
        | RuleD (id, _, _, _, _) -> push (String.uppercase_ascii id.it) consts
      ) rules
    | RelD (_, _, _, rules) ->
      List.iter (fun rule -> match rule.it with
        | RuleD (_, _, _, args, prems) when
          let rec is_const_checking prem = match prem.it with
          | RulePr (id, _, _) -> id.it = "Expr_ok_const"
          | IterPr (prem', _) -> is_const_checking prem'
          | _ -> false in
          List.exists is_const_checking prems -> ( match args.it with
            | TupE [_; e; _] | TupE [_; e] -> ( match e.it with
              | CaseE (atom, _) | MixE ([atom] :: _, _) ->
                push (atom |> string_of_atom |> String.uppercase_ascii) const_ctxs
              | _ -> () )
            | _ -> () )
        | _ -> () ) rules
    | _ -> ()
  ) !il
)

let print_consts () = List.iter (fun i -> print_endline i) !consts

(** Seed generation **)

let case_stack = ref []

(* Generate specific syntax from input IL *)
let rec gen name =
  let syn = find_syn name in

  (* HARDCODE: Wasm expression *)
  if name = "expr" then gen_wasm_expr 0 (if top case_stack = "FUNC" then None else Some 1) else

  (* HARDCODE: name *)
  if name = "name" then (Al.Ast.TextV (choose ["a"; "b"; "c"] ^ choose ["1"; "2"; "3"])) else

  match syn.it with
  | AliasT typ -> gen_typ typ
  (* TupV *)
  | NotationT ([[]; []; []], typs)
  | NotationT ([[LBrack]; [Dot2]; [RBrack]], typs) ->
    let pair = gen_typs typs in
    let fst = List.hd pair in
    let snd = List.hd (List.tl pair) in
    Al.Ast.TupV (fst, snd)
  (* ArrowV *)
  | NotationT ([[]; [Arrow]; []], typs) ->
    let pair = gen_typs typs in
    let fst = List.hd pair in
    let snd = List.hd (List.tl pair) in
    Al.Ast.ArrowV (fst, snd)
  (* CaseV *)
  | NotationT ((atom :: _) :: _, typs)
  | NotationT ([[]; [atom]], typs) (* Hack for I8 *) ->
    let case = string_of_atom atom in
    push case case_stack;
    let args = gen_typs typs in
    pop case_stack;
    Al.Ast.CaseV (case, args)
  (* StrV *)
  | StructT typfields ->
    let rec_ = List.fold_right (fun typefield ->
      let (atom, (_, typ, _), _) = typefield in
      let key = string_of_atom atom in
      let value = gen_typ typ in
      Record.add key value
    ) typfields Record.empty in
    Al.Ast.StrV rec_
  (* CaseV *)
  (* HARDCODE: Wasm instruction *)
  | VariantT typcases when name = "instr" ->
    let (stack_size, _) = top dim_stack in
    (* HARDCODE: checks if currently in a context that requires const instrution *)
    let typcases' = if not (contains (top case_stack) !const_ctxs) then typcases else
      let is_const (atom, _, _) = contains (string_of_atom atom) !consts in
      List.filter is_const typcases
    in
    let rec try_instr () =
      let typcase = choose typcases' in
      let (atom, (_, typs, _), _) = typcase in
      let case = string_of_atom atom in
      let (t1, t2) = !dims |> Record.find case in
      let n1 = fix_n t1 in
      let n2 = fix_n t2 in
      if n1 > stack_size then
        try_instr ()
      else (
        push case case_stack;
        let args = match case with
        | "BLOCK"
        | "LOOP" -> [ gen "blocktype"; gen_wasm_expr n1 (Some n2) ]
        | "IF" -> [ gen "blocktype"; gen_wasm_expr (n1-1) (Some n2); gen_wasm_expr (n1-1) (Some n2) ]
        | _ -> gen_typs typs in
        pop case_stack;
        update_dim (n2 - n1);
        Al.Ast.CaseV (case, args) )
    in try_instr ()
  | VariantT typcases ->
    (* HARDCODE: Remove V128 *)
    let typcases = List.filter (fun (atom, _, _) -> atom <> Il.Ast.Atom "V128") typcases in
    let typcases = List.filter (fun (atom, _, _) -> atom <> Il.Ast.Atom "BOT") typcases in
    let typcase = choose typcases in
    let (atom, (_, typs, _), _) = typcase in
    let case = string_of_atom atom in
    push case case_stack;
    let args = gen_typs typs in
    pop case_stack;
    Al.Ast.CaseV (case, args)
  | _ -> failwith ("TODO: gen of " ^ Il.Print.string_of_deftyp syn ^ Il.Print.structured_string_of_deftyp syn)

and gen_wasm_expr n1 n2_opt =
  let rec try_expr life =
    dim_stack := (n1, n2_opt) :: !dim_stack;
    let n = Random.int 5 + 1 (* 1, 2, 3, 4, 5 *) in
    let ret = List.init n (fun _ -> gen "instr") |> listV in
    let (actual, expected) = List.hd !dim_stack in
    dim_stack := List.tl !dim_stack;
    match actual, expected with
    | _, None -> ret
    | n, Some m when n = m -> ret
    | _ when life > 0 -> try_expr (life - 1)
    | _ -> ret
  in
  try_expr 100

and gen_typ typ = match typ.it with
  | VarT id -> gen id.it
  | NumT NatT ->
    let i = Random.int 3 in (* 0, 1, 2 *)
    numV i
  | IterT (typ', List) ->
    let n = Random.int 3 + 1 in (* 1, 2, 3 *)
    let l = List.init n (fun _ -> gen_typ typ') in
    listV l
  | TupT typs -> List.map gen_typ typs |> listV
  | IterT (typ', Opt) ->
    if Random.bool() then
      optV_none
    else
      optV_some (gen_typ typ')
  | _ -> failwith ("TODO: unhandled type for gen_typ: " ^ Il.Print.structured_string_of_typ typ)

and gen_typs typs = match typs.it with
  | TupT typs' -> List.map gen_typ typs'
  | _ -> [ gen_typ typs ]

(** Mutation **)
let mutate modules = modules (* TODO *)

(* Generate tests *)

let gen_test el' il' al' =
  (* Register spec *)
  el := el';
  il := flatten_rec il';
  al := al';

  (* Initialize *)
  Random.init !seed;
  Backend_interpreter.Ds.init !al;
  estimate_dim ();
  estimate_const ();

  (* Generate tests *)
  let seeds = List.init !test_num (fun _ -> gen "module") in

  (* Mutatiion *)
  let tests = mutate seeds in

  (* Result *)
  Backend_interpreter.Tester.init_tester ();

  tests |> List.iter (fun m ->
    print_endline "================";
    print_endline (string_of_module m);
    try
      let externvals = listV (
        List.init 7 (fun i -> Al.Ast.CaseV ("FUNC", [numV i]))
        @ List.init 4 (fun i -> Al.Ast.CaseV ("GLOBAL", [numV i]))
      ) in (*TODO *)
      Backend_interpreter.Interpreter.call_instantiate [ m; externvals ] |> ignore;
      print_endline "Instantiation success"
    with e ->
      print_endline (
        "Instantiation fail due to "
        ^ Printexc.to_string e
        ^ " during "
        ^ String.concat ", " !Backend_interpreter.Interpreter.algo_name_stack
      )
  )

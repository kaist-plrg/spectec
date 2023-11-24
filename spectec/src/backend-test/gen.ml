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
  let i = Random.int n in
  List.nth l i

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

(** Seed generation **)

(* Generate specific syntax from input IL *)
let rec gen name =
  let syn = find_syn name in

  match syn.it with
  | AliasT typ -> gen_typ typ
  (* CaseV *)
  | NotationT ((atom :: _) :: _, typs)
  | NotationT ([[]; [atom]], typs) (* Hack for I8 *) ->
    let name = Il.Print.string_of_atom atom in
    let args = gen_typs typs in
    Al.Ast.CaseV (name, args)
  (* TupV *)
  | NotationT ([[]; []; []], typs) ->
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
  (* StrV *)
  | StructT typfields ->
    let rec_ = List.fold_right (fun typefield ->
      let (atom, (_, typ, _), _) = typefield in
      let key = Il.Print.string_of_atom atom in
      let value = gen_typ typ in
      Record.add key value
    ) typfields Record.empty in
    Al.Ast.StrV rec_
  (* CaseV *)
  | VariantT typcases ->
    (* HARDCODE: Remove V128 *)
    let typcases = List.filter (fun (atom, _, _) -> atom <> Il.Ast.Atom "V128") typcases in
    let typcases = List.filter (fun (atom, _, _) -> atom <> Il.Ast.Atom "BOT") typcases in
    let typcase = choose typcases in
    let (atom, (_, typs, _), _) = typcase in
    let name = Il.Print.string_of_atom atom in
    let args = gen_typs typs in
    Al.Ast.CaseV (name, args)
  | _ -> failwith ("TODO: gen of " ^ Il.Print.string_of_deftyp syn ^ Il.Print.structured_string_of_deftyp syn)

and gen_typ typ = match typ.it with
  | VarT id -> gen id.it
  | NumT NatT ->
    let i = Random.int 4 - 1 in (* -1, 0, 1, 2 *)
    numV i
  | IterT (typ', List) ->
    let n = Random.int 3 in (* 0, 1, 2 *)
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

  (* Generate tests *)
  let seeds = List.init !test_num (fun _ -> gen "module") in

  (* Mutatiion *)
  let tests = mutate seeds in

  (* Result *)
  tests |> List.iter (fun m ->
    print_endline "================";
    print_endline (Al.Print.string_of_value m);
    try
      Backend_interpreter.Interpreter.call_instantiate [ m; listV [] ] |> ignore;
      print_endline "Instantiation success"
    with e ->
      print_endline ("Instantiation fail due to " ^ Printexc.to_string e)
  )

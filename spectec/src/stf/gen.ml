open Util.Source
open Il.Ast
open Il.Print

let max_depth = ref 0

module Env = Il.Env

let env: Env.t ref = ref Env.empty
let init (il: script) : unit = env := Env.env_of_script il


let admin_syntax = [ "REC"; "DEF"; "BOT" ]

let is_surface_syntax (tc: typcase) : bool =
  let mixop, _, _ = tc in
  match mixop with
  | ({ it = Atom name; _} :: _) :: _ when List.mem name admin_syntax -> false
  | _ -> true


(* Ast constructors *)

let (%) it note = it $$ no_region % note


let varT (typname: string) : typ = VarT (typname $ no_region, []) $ no_region
(*
let noinfo : info = { def=""; case="" }
let iterT (typ: typ) (iter: iter) : typ = IterT (typ, iter) $ no_region

let tupE (exps: exp list) : exp =
  let tup_typ = TupT (List.map (fun e -> e, e.note) exps) $ no_region in

  TupE exps % tup_typ

let caseE (typname: string) (mixop: mixop) (args: exp list) : exp =

  CaseE (mixop, tupE args) % varT typname

let listE (typname: string) (exps: exp list) : exp =
  ListE exps % iterT (varT typname) List
  *)


let rec cartesian_product : 'a list list -> 'a list list = function
  | [] -> [[]]
  | h :: t ->
    let suffixes = cartesian_product t in
    List.concat_map
      (fun prefix -> List.map (fun suffix -> prefix :: suffix) suffixes)
      h

(* Generate types *)

let rec tmp (depth) (typ: typ) : exp list =
  match typ.it with
  (* TODO *)
  | VarT (id, [ { it=TypA typ; _ } ]) when id.it = "list" ->
    tmp depth (IterT (typ, List) $ no_region)
  | VarT (id, _) -> types depth id.it
  | TupT ps ->
    ps
    |> List.map snd
    |> List.map (tmp depth)
    |> cartesian_product
    |> List.map (function exps -> TupE exps % typ)
  (* TODO *)
  | NumT `NatT -> [ NumE (`Nat Z.zero) % typ ]
  | IterT (typ, iter) ->
    (match iter with
    | Opt ->
      OptE None % typ :: 
        List.map (function ttt -> OptE (Some ttt) % typ) (tmp depth typ)
    | _ ->
      ListE [] % typ :: 
        List.map (function ttt -> ListE [ ttt ] % typ) (tmp depth typ)
    (* TODO: any length *)
    )
  | _ -> failwith (string_of_typ typ)

and caseE (depth: int) (typname: string) (tc: typcase) : exp list =
  let mixop, (_, typ, _), _ = tc in
  typ
  |> tmp depth
  |> List.map (function typ -> CaseE (mixop, typ) % varT typname)

and types (depth: int) (name: string) : exp list =
  let depth = depth + 1 in
  if depth > !max_depth then []
  else
    name $ no_region
    |> Env.find_typ !env
    |> snd
    |> function insts ->
      (* TODO: generalize this *)
      assert (List.length insts = 1);
      List.hd insts
    |> it
    |> function (InstD (_, _, dt)) -> dt.it
    |> (function
        | VariantT tcs ->
          tcs
          |> List.filter is_surface_syntax
          |> List.concat_map (caseE depth name)
        | AliasT typ -> tmp depth typ
        | dt -> failwith (string_of_deftyp `H (dt $ no_region)))

let types (max_depth': int) (name: string) : exp list =
  max_depth := max_depth';
  types 0 name

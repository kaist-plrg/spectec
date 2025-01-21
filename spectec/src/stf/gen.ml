open Util.Source
open Il.Ast
open Il.Print

module Env = Il.Env

let env: Env.t ref = ref Env.empty
let init (il: script) : unit = env := Env.env_of_script il


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

let rec tmp (typ: typ) : exp list =
  match typ.it with
  (* TODO *)
  | VarT (id, _) -> types id.it
  | TupT ps ->
    ps
    |> List.map snd
    |> List.map tmp
    |> cartesian_product
    |> List.map (function exps -> TupE exps % typ)
  (* TODO *)
  | NumT `NatT -> [ NumE (`Nat Z.zero) % typ ]
  | _ -> failwith (string_of_typ typ)

and caseE (typname: string) (tc: typcase) : exp list =
  let mixop, (_, typ, _), _ = tc in
  typ
  |> tmp
  |> List.map (function typ -> CaseE (mixop, typ) % varT typname)

and types (name: string) : exp list =
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
      | VariantT tcs -> List.concat_map (caseE name) tcs
      | AliasT typ -> tmp typ
      | dt -> failwith (string_of_deftyp `H (dt $ no_region)))


let types' (_name: string) : unit =
  types "mem"
  |> List.map string_of_exp
  |> List.iter print_endline

open Util.Source
open Il.Ast

(* HARDCODE: Never drop CONST, REF.NULL, types *)
let removable x = match x with
  | (mixop, _, _) when List.mem (Xl.Mixop.to_string mixop) ["CONST"; "VCONST"; "REF.NULL"; "I32"; "I64"; "F32"; "F64"; "FUNCREF"; "EXTERNREF"] -> false
  | _ -> true

(* Drop random elements from a list,
   while ensuring the existence of at least one element *)
let rec prune' xs acc = match xs with
  | [] -> acc
  | hd :: tl -> prune' tl (if removable hd && Random.int 2 = 0 then acc else hd :: acc)
let prune xs =
  match prune' xs [] with
  | [] -> if xs = [] then [] else [ Utils.choose xs ]
  | xs' -> List.rev xs'

let prune_deftyp' = function
  | VariantT typcases -> VariantT (prune typcases)
  | deftyp' -> deftyp'
let prune_deftyp deftyp = { deftyp with it = prune_deftyp' deftyp.it }

let prune_inst' = function
  | InstD (binds, args, deftyp) -> InstD (binds, args, prune_deftyp deftyp)
let prune_inst inst = { inst with it = prune_inst' inst.it }

let prune_def' = function
  | TypD (id, params, insts)
    (* HARDCODE: Blacklist for prunings, very un-robust *)
    when not (List.mem id.it ["inn"; "fnn" ]) ->
      TypD (id, params, List.map prune_inst insts)
  | def' -> def'
let prune_def def = { def with it = prune_def' def.it }

let prune_il il =
  List.map prune_def il

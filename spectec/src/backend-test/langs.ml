open Util.Source
open Utils

let el: El.Ast.script ref = ref []
let il: Il.Ast.script ref = ref []
let al: Al.Ast.script ref = ref []

let orig_il: Il.Ast.script ref = ref []

let il_env: Il.Env.t ref = ref Il.Env.empty

let get_rules rid =
  List.find_map (fun def ->
    match def.it with
    | Il.Ast.RelD (id, _, _, rules) when Il.Eq.eq_id rid id -> Some rules
    | _ -> None
  ) !il |> Option.get

let get_typing_rules () =
  List.concat_map (fun def ->
    match def.it with
    | Il.Ast.RelD (id, _, _, rules) when id.it = "Instr_ok" || id.it = "Instrf_ok" -> rules
    | _ -> []
  ) !il

let consts = ref []
let const_ctxs = ref []
let estimate_const () =
  let open Il.Ast in
  List.iter (fun def ->
    match def.it with
    | RelD (id, _, _, rules) when id.it = "Instr_const" ->
      List.iter (fun rule -> match rule.it with
        | RuleD (id, _, _, _, []) -> push (String.uppercase_ascii id.it) consts
        | _ -> () (* TODO: conditioned const instrs *)
      ) rules
    | RelD (_, _, _, rules) ->
      List.iter (fun rule ->
        let rec is_const_checking prem =
          match prem.it with
          | RulePr (id, _, _) -> id.it = "Expr_ok_const"
          | IterPr (prem', _) -> is_const_checking prem'
          | _ -> false
        in
        match rule.it with
        | RuleD (_, _, _, args, prems) when List.exists is_const_checking prems ->
          (match args.it with
          | TupE [_; e; _] | TupE [_; e] ->
            (match e.it with
            | CaseE ([ { it = Atom atomid; _ } ] :: _, _) ->
              push atomid const_ctxs
            | _ -> ()
            )
          | _ -> ()
          )
        | _ -> ()
      ) rules
    | _ -> ()
  ) !il

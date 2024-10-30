open Util.Source

let el: El.Ast.script ref = ref []
let il: Il.Ast.script ref = ref []
let al: Al.Ast.script ref = ref []

let orig_il: Il.Ast.script ref = ref []

let get_typing_rules () =
  List.concat_map (fun def ->
    match def.it with
    | Il.Ast.RelD (id, _, _, rules) when id.it = "Instr_ok" || id.it = "Instrf_ok" -> rules
    | _ -> []
  ) !il

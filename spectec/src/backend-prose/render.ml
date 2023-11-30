open Prose
open Printf
open Config

(* Environment *)

module Set = Set.Make(String)
module Map = Map.Make(String)

type env = 
  { 
    config: config;
    prose: prose;
    macro: Macro.env;
  }

let gen_macro env = 
  if env.config.macros then
    Macro.gen_macro env.macro

let env config inputs outputs el prose : env = 
  let macro = Macro.env inputs outputs el in
  let env = { config; prose; macro; } in
  env

(* Helpers *)

let indent = "   "

let rec repeat str num =
  if num = 0 then ""
  else if Int.rem num 2 = 0 then repeat (str ^ str) (num / 2)
  else str ^ repeat (str ^ str) (num / 2)

let math = ":math:"

let render_math s = math ^ sprintf "`%s`" s

let render_opt prefix stringifier suffix = function
  | None -> ""
  | Some x -> prefix ^ stringifier x ^ suffix

let render_order index depth =
  index := !index + 1;

  let num_idx = string_of_int !index in
  let alp_idx = Char.escaped (Char.chr (96 + !index)) in

  match depth mod 4 with
  | 0 -> num_idx ^ "."
  | 1 -> alp_idx ^ "."
  | 2 -> num_idx ^ ")"
  | 3 -> alp_idx ^ ")"
  | _ -> failwith "unreachable"

let render_list stringifier left sep right = function
  | [] -> left ^ right
  | h :: t ->
      let limit = 16 in
      let is_long = List.length t > limit in
      left
      ^ List.fold_left
          (fun acc elem -> acc ^ sep ^ stringifier elem)
          (stringifier h) (List.filteri (fun i _ -> i <= limit) t)
      ^ (if is_long then (sep ^ "...") else "")
      ^ right

(* Operators *)

let render_prose_cmpop = function
  | Eq -> "equal to"
  | Ne -> "different with"
  | Lt -> "less than"
  | Gt -> "greater than"
  | Le -> "less than or equal to"
  | Ge -> "greater than or equal to"

let render_al_unop = function
  | Al.Ast.NotOp -> "not"
  | Al.Ast.MinusOp -> "-"

let render_al_binop = function
  | Al.Ast.AndOp -> "and"
  | Al.Ast.OrOp -> "or"
  | Al.Ast.ImplOp -> "=>"
  | Al.Ast.EquivOp -> "<=>"
  | Al.Ast.AddOp -> "+"
  | Al.Ast.SubOp -> "-"
  | Al.Ast.MulOp -> "\\cdot"
  | Al.Ast.DivOp -> "/"
  | Al.Ast.ExpOp -> "^"

let render_al_cmpop = function
  | Al.Ast.EqOp -> "is"
  | Al.Ast.NeOp -> "is not"
  | Al.Ast.LtOp -> "is less than"
  | Al.Ast.GtOp -> "is greater than"
  | Al.Ast.LeOp -> "is less than or equal to"
  | Al.Ast.GeOp -> "is greater than or equal to"

(* Names and Iters *)

(* assume Names and Iters are always embedded in math blocks *)

let rec render_name name = match name with
  | "inverse_of_bytes_" -> "inverse\\_of\\_bytes"
  | "exec_expr_const" -> "exec\\_expr\\_const"
  | _ -> (match String.index_opt name '_' with 
    | Some idx ->
        let base = String.sub name 0 idx in
        let subscript = String.sub name (idx + 1) ((String.length name) - idx - 1) in
        base ^ "_{" ^ subscript ^ "}"
    | _ -> name)

and render_kwd env kwd = match Macro.find_kwd env.macro kwd with
  | Some (lhs, rhs) -> if env.config.macros then lhs else rhs 
  | None -> render_name (Al.Print.string_of_kwd kwd)

and render_funcname env funcname = match Macro.find_funcname env.macro funcname with
  | Some (lhs, rhs) -> if env.config.macros then lhs else rhs
  | None -> render_name funcname 

let rec render_iter env = function
  | Al.Ast.Opt -> "^?"
  | Al.Ast.List -> "^\\ast"
  | Al.Ast.List1 -> "^{+}"
  | Al.Ast.ListN (expr, None) -> "^{" ^ render_expr env true expr ^ "}"
  | Al.Ast.ListN (expr, Some name) ->
      "^{(" ^ render_name name ^ "<" ^ render_expr env true expr ^ ")}"

and render_iters env iters = List.map (render_iter env) iters |> List.fold_left (^) ""

(* Expressions and Paths *)

and render_expr env in_math = function
  | Al.Ast.NumE i ->
      let si = Int64.to_string i in
      if in_math then si else render_math si
  | Al.Ast.UnE (MinusOp, e) ->
      let se = render_expr env in_math e in
      let s = sprintf "-%s" se in
      if in_math then s else render_math s
  | Al.Ast.BinE (op, e1, e2) ->
      let sop = render_al_binop op in
      let se1 = render_expr env true e1 in
      let se2 = render_expr env true e2 in
      let s = sprintf "{%s} %s {%s}" se1 sop se2 in
      if in_math then s else render_math s
  | Al.Ast.TupE el ->
      let sel = render_list (render_expr env true) "(" "~" ")" el in
      if in_math then sel else render_math sel
  | Al.Ast.CallE (fn, es) ->
      let sfn = render_funcname env fn in
      let ses = render_list (render_expr env true) "" ", " "" es in
      let s = sprintf "%s(%s)" sfn ses in
      if in_math then s else render_math s
  (* TODO a better way to flatten single-element list? *)
  | Al.Ast.CatE (Al.Ast.ListE e1, Al.Ast.ListE e2) when List.length e1 = 1 && List.length e2 = 1 ->
      let se1 = render_expr env true (List.hd e1) in
      let se2 = render_expr env true (List.hd e2) in
      let s = sprintf "%s~%s" se1 se2 in 
      if in_math then s else render_math s
  | Al.Ast.CatE (Al.Ast.ListE e1, e2) when List.length e1 = 1 ->
      let se1 = render_expr env true (List.hd e1) in
      let se2 = render_expr env true e2 in
      let s = sprintf "%s~%s" se1 se2 in
      if in_math then s else render_math s
  | Al.Ast.CatE (e1, Al.Ast.ListE e2) when List.length e2 = 1 ->
      let se1 = render_expr env true e1 in
      let se2 = render_expr env true (List.hd e2) in
      let s = sprintf "%s~%s" se1 se2 in
      if in_math then s else render_math s
  | Al.Ast.CatE (e1, e2) ->
      let se1 = render_expr env true e1 in
      let se2 = render_expr env true e2 in
      let s = sprintf "%s~%s" se1 se2 in
      if in_math then s else render_math s
  | Al.Ast.LenE e ->
      let se = render_expr env true e in
      if in_math then "|" ^ se ^ "|" else "the length of " ^ render_math se
  | Al.Ast.ArityE e -> sprintf "the arity of %s" (render_expr env in_math e)
  | Al.Ast.GetCurLabelE -> "the current label"
  | Al.Ast.GetCurFrameE -> "the current frame"
  | Al.Ast.GetCurContextE -> "the current context"
  | Al.Ast.FrameE (None, e2) ->
      sprintf "the activation of %s" (render_expr env in_math e2)
  | Al.Ast.FrameE (Some e1, e2) ->
      sprintf "the activation of %s with arity %s" (render_expr env in_math e2)
        (render_expr env in_math e1)
  | Al.Ast.ListE el -> 
      let sel = 
        if List.length el > 0 then
          render_list (render_expr env true) "" "~" "" el
        else
          "\\epsilon"
      in
      if in_math then sel else render_math sel
  | Al.Ast.AccE (e, p) ->
      let se = render_expr env true e in
      let sp = render_path env p in
      let s = sprintf "%s%s" se sp in
      if in_math then s else render_math s
  | Al.Ast.ExtE (e1, ps, e2, dir) ->
      let se1 = render_expr env in_math e1 in
      let sps = render_paths env in_math ps in
      let se2 = render_expr env in_math e2 in
      if in_math then
        (match dir with
        | Al.Ast.Front -> sprintf "\\{%s~%s\\}~\\bigoplus~%s" sps se2 se1
        | Al.Ast.Back -> sprintf "%s~\\bigoplus~\\{%s~%s\\}" se1 sps se2)
      else
        (match dir with
        | Al.Ast.Front -> sprintf "%s with %s prepended by %s" se1 sps se2
        | Al.Ast.Back -> sprintf "%s with %s appended by %s" se1 sps se2)
  | Al.Ast.UpdE (e1, ps, e2) ->
      sprintf "%s with %s replaced by %s" 
        (render_expr env in_math e1) 
        (render_paths env in_math ps)
        (render_expr env in_math e2)
  | Al.Ast.StrE r ->
      let sr = 
        Util.Record.Record.fold
          (fun k v acc -> acc @ [ render_kwd env k ^ "~" ^ render_expr env true v ])
          r []
      in
      let sr = render_list Fun.id "\\{" ", " "\\}" sr in
      if in_math then sr else render_math sr
  | Al.Ast.ContE e -> sprintf "the continuation of %s" (render_expr env in_math e)
  | Al.Ast.LabelE (e1, e2) ->
      sprintf "the label whose arity is %s and whose continuation is %s" (render_expr env in_math e1) (render_expr env in_math e2)
  | Al.Ast.VarE n | Al.Ast.SubE (n, _) ->
      let sn = render_name n in
      if in_math then sn else render_math sn
  | Al.Ast.IterE (Al.Ast.VarE n, _, iter) ->
      let sn = render_name n in
      let siter = render_iter env iter in
      let s = sprintf "{%s}{%s}" sn siter in
      if in_math then s else render_math s
  | Al.Ast.IterE (e, _, iter) -> 
      let se = render_expr env true e in
      let siter = render_iter env iter in
      let s = sprintf "{(%s)}{%s}" se siter in
      if in_math then s else render_math s
  | Al.Ast.ArrowE (e1, e2) ->
      let se1 = render_expr env true e1 in
      let se2 = render_expr env true e2 in
      let s = sprintf "%s \\to %s" se1 se2 in
      if in_math then s else render_math s
  | Al.Ast.CaseE (tag, []) ->
      let stag = render_kwd env tag in
      if in_math then stag else render_math stag
  (* TODO a hard-coded hint for CONST *)
  | Al.Ast.CaseE (("CONST", _) as tag, [ e1; e2 ])->
      let stag = render_kwd env tag in
      let se1 = render_expr env true e1 in
      let se2 = render_expr env true e2 in
      let s = sprintf "%s.%s~%s" se1 stag se2 in
      if in_math then s else render_math s
  | Al.Ast.CaseE (tag, es) ->
      let stag = render_kwd env tag in
      let ses = render_list (render_expr env true) "" "~" "" es in
      let s = sprintf "%s~%s" stag ses in
      if in_math then s else render_math s
  | Al.Ast.OptE (Some e) -> 
      let se = render_expr env true e in
      let s = sprintf "{%s}^?" se in 
      if in_math then s else render_math s
  | Al.Ast.OptE None -> 
      let s = "\\epsilon" in
      if in_math then s else render_math s
  | Al.Ast.YetE s -> sprintf "YetE (%s)" s
  | _ -> failwith "unreachable"

(* assume Paths are always embedded in math blocks *)

and render_path env = function
  | Al.Ast.IdxP e -> sprintf "[%s]" (render_expr env true e)
  | Al.Ast.SliceP (e1, e2) ->
      sprintf "[%s : %s]" (render_expr env true e1) (render_expr env true e2)
  | Al.Ast.DotP s -> sprintf ".%s" (render_kwd env s)

and render_paths env in_math paths = 
  let spaths = List.map (render_path env) paths |> List.fold_left (^) "" in
  if in_math then spaths else render_math spaths

(* Conditions *)

(* assume Conditions are never embedded in math blocks *)

and render_cond env = function
  | Al.Ast.UnC (NotOp, Al.Ast.IsCaseOfC (e, c)) ->
      sprintf "%s is not of the case %s" 
        (render_expr env false e) 
        (render_math (render_kwd env c))
  | Al.Ast.UnC (NotOp, Al.Ast.IsDefinedC e) ->
      sprintf "%s is not defined" (render_expr env false e)
  | Al.Ast.UnC (NotOp, Al.Ast.IsValidC e) ->
      sprintf "%s is not valid" (render_expr env false e)
  | Al.Ast.UnC (op, c) ->
      sprintf "%s %s" (render_al_unop op) (render_cond env c)
  | Al.Ast.BinC (op, c1, c2) ->
      sprintf "%s %s %s" (render_cond env c1) (render_al_binop op) (render_cond env c2)
  | Al.Ast.CmpC (op, e1, e2) ->
      sprintf "%s %s %s" (render_expr env false e1) (render_al_cmpop op) (render_expr env false e2)
  | Al.Ast.ContextKindC (s, e) -> sprintf "%s is %s" (render_expr env false e) (render_kwd env s)
  | Al.Ast.IsDefinedC e -> sprintf "%s is defined" (render_expr env false e)
  | Al.Ast.IsCaseOfC (e, c) -> sprintf "%s is of the case %s" (render_expr env false e) (render_math (render_kwd env c))
  | Al.Ast.HasTypeC (e, t) -> sprintf "the type of %s is %s" (render_expr env false e) t
  | Al.Ast.IsValidC e -> sprintf "%s is valid" (render_expr env false e)
  | Al.Ast.TopLabelC -> "a label is now on the top of the stack"
  | Al.Ast.TopFrameC -> "a frame is now on the top of the stack"
  | Al.Ast.TopValueC (Some e) -> sprintf "a value of value type %s is on the top of the stack" (render_expr env false e)
  | Al.Ast.TopValueC None -> "a value is on the top of the stack"
  | Al.Ast.TopValuesC e -> sprintf "there are at least %s values on the top of the stack" (render_expr env false e)
  | Al.Ast.MatchC (e1, e2) ->
    sprintf "%s matches %s"
      (render_expr env false e1)
      (render_expr env false e2)
  | Al.Ast.YetC s -> sprintf "YetC (%s)" s

(* Instructions *)

let rec render_prose_instr env depth = function
  | LetI (e1, e2) ->
      sprintf "* Let %s be %s."
        (render_expr env false e1)
        (render_expr env false e2)
  | CmpI (e1, cmpop, e2) ->
      sprintf "* %s must be %s %s."
        (String.capitalize_ascii (render_expr env false e1))
        (render_prose_cmpop cmpop)
        (render_expr env false e2)
  | MustValidI (e1, e2, e3) ->
      sprintf "* Under the context %s, %s must be valid%s."
        (render_expr env false e1)
        (render_expr env false e2)
        (render_opt " with type " (render_expr env false) "" e3)
  | MustMatchI (e1, e2) ->
      sprintf "* %s must match %s."
        (String.capitalize_ascii (render_expr env false e1))
        (render_expr env false e2)
  | IsValidI e ->
      sprintf "* The instruction is valid%s."
        (render_opt " with type " (render_expr env false) "" e)
  | IfI (c, is) ->
      sprintf "* If %s,%s"
        (render_cond env c)
        (render_prose_instrs env (depth + 1) is)
  | ForallI (e1, e2, is) ->
      sprintf "* For all %s in %s,%s"
        (render_expr env false e1)
        (render_expr env false e2)
        (render_prose_instrs env (depth + 1) is)
  | EquivI (c1, c2) ->
      sprintf "* %s and %s are equivalent."
        (String.capitalize_ascii (render_cond env c1))
        (render_cond env c2)
  | YetI s ->
      sprintf "* YetI: %s." s

and render_prose_instrs env depth instrs =
  List.fold_left
    (fun sinstrs i ->
      sinstrs ^ "\n\n" ^ repeat indent depth ^ render_prose_instr env depth i)
    "" instrs

let rec render_al_instr env algoname index depth instr =
  match instr.Al.Ast.it with
  | Al.Ast.IfI (c, il, []) ->
      sprintf "%s If %s, then:%s" (render_order index depth) (render_cond env c)
        (render_al_instrs env algoname (depth + 1) il)
  | Al.Ast.IfI (c, il1, [ { it = IfI (inner_c, inner_il1, []); _ } ]) ->
      let if_index = render_order index depth in
      let else_if_index = render_order index depth in
      sprintf "%s If %s, then:%s\n\n%s Else if %s, then:%s"
        if_index
        (render_cond env c)
        (render_al_instrs env algoname (depth + 1) il1)
        (repeat indent depth ^ else_if_index)
        (render_cond env inner_c)
        (render_al_instrs env algoname (depth + 1) inner_il1)
  | Al.Ast.IfI (c, il1, [ { it = IfI (inner_c, inner_il1, inner_il2); _ } ]) ->
      let if_index = render_order index depth in
      let else_if_index = render_order index depth in
      let else_index = render_order index depth in
      sprintf "%s If %s, then:%s\n\n%s Else if %s, then:%s\n\n%s Else:%s"
        if_index
        (render_cond env c)
        (render_al_instrs env algoname (depth + 1) il1)
        (repeat indent depth ^ else_if_index)
        (render_cond env inner_c)
        (render_al_instrs env algoname (depth + 1) inner_il1)
        (repeat indent depth ^ else_index)
        (render_al_instrs env algoname (depth + 1) inner_il2)
  | Al.Ast.IfI (c, il1, il2) ->
      let if_index = render_order index depth in
      let else_index = render_order index depth in
      sprintf "%s If %s, then:%s\n\n%s Else:%s" if_index (render_cond env c)
        (render_al_instrs env algoname (depth + 1) il1)
        (repeat indent depth ^ else_index)
        (render_al_instrs env algoname (depth + 1) il2)
  | Al.Ast.OtherwiseI il ->
      sprintf "%s Otherwise:%s" (render_order index depth)
        (render_al_instrs env algoname (depth + 1) il)
  | Al.Ast.EitherI (il1, il2) ->
      let either_index = render_order index depth in
      let or_index = render_order index depth in
      sprintf "%s Either:%s\n\n%s Or:%s" either_index
        (render_al_instrs env algoname (depth + 1) il1)
        (repeat indent depth ^ or_index)
        (render_al_instrs env algoname (depth + 1) il2)
  | Al.Ast.AssertI c -> 
      let vref = if Macro.find_section env.macro ("valid-" ^ algoname) then ":ref:`validation <valid-" ^ algoname ^">`" else "validation" in
      sprintf "%s Assert: Due to %s, %s." (render_order index depth) vref (render_cond env c) 
  | Al.Ast.PushI e ->
      sprintf "%s Push %s to the stack." (render_order index depth)
        (render_expr env false e)
  (* TODO hardcoded for PopI on label or frame by raw string *)
  | Al.Ast.PopI (Al.Ast.VarE s) when s = "the label" || s = "the frame" ->
      sprintf "%s Pop %s from the stack." (render_order index depth) s
  | Al.Ast.PopI e ->
      sprintf "%s Pop %s from the stack." (render_order index depth)
        (render_expr env false e)
  | Al.Ast.PopAllI e ->
      sprintf "%s Pop all values %s from the stack." (render_order index depth)
        (render_expr env false e)
  | Al.Ast.LetI (n, e) ->
      sprintf "%s Let %s be %s." (render_order index depth) (render_expr env false n)
        (render_expr env false e)
  | Al.Ast.TrapI -> sprintf "%s Trap." (render_order index depth)
  | Al.Ast.NopI -> sprintf "%s Do nothing." (render_order index depth)
  | Al.Ast.ReturnI e_opt ->
      sprintf "%s Return%s." (render_order index depth)
        (render_opt " " (render_expr env false) "" e_opt)
  | Al.Ast.EnterI (e1, e2, il) ->
      sprintf "%s Enter %s with label %s:%s" (render_order index depth)
        (render_expr env false e1) (render_expr env false e2)
        (render_al_instrs env algoname (depth + 1) il)
  | Al.Ast.ExecuteI e ->
      sprintf "%s Execute %s." (render_order index depth) (render_expr env false e)
  | Al.Ast.ExecuteSeqI e ->
      sprintf "%s Execute the sequence %s." (render_order index depth) (render_expr env false e)
  | Al.Ast.PerformI (n, es) ->
      sprintf "%s Perform %s." (render_order index depth) (render_expr env false (Al.Ast.CallE (n, es)))
  | Al.Ast.ExitI -> render_order index depth ^ " Exit current context."
  | Al.Ast.ReplaceI (e1, p, e2) ->
      sprintf "%s Replace %s with %s." (render_order index depth)
        (render_expr env false (Al.Ast.AccE (e1, p))) (render_expr env false e2)
  | Al.Ast.AppendI (e1, e2) ->
      sprintf "%s Append %s to the %s." (render_order index depth)
        (render_expr env false e2) (render_expr env false e1)
  | Al.Ast.YetI s -> sprintf "%s YetI: %s." (render_order index depth) s

and render_al_instrs env algoname depth instrs =
  let index = ref 0 in
  List.fold_left
    (fun sinstrs i ->
      sinstrs ^ "\n\n" ^ repeat indent depth ^ render_al_instr env algoname index depth i)
    "" instrs

(* Prose *)

let render_kwd_title env kwd params =
  (* TODO a workaround, for algorithms named label or name
     that are defined as LABEL_ or FRAME_ in the dsl *) 
  let (name, syntax) = kwd in 
  let kwd = 
    if name = "LABEL" then ("LABEL_", syntax)
    else if name = "FRAME" then ("FRAME_", syntax)
    else kwd 
  in
  render_expr env false (Al.Ast.CaseE (kwd, params))

let render_funcname_title env fname params =
  render_expr env false (Al.Ast.CallE (fname, params))

let render_pred env name params instrs =
  let (pname, syntax) = name in
  let kwd = (String.uppercase_ascii pname, syntax) in
  let title = render_kwd_title env kwd params in
  title ^ "\n" ^
  String.make (String.length title) '.' ^ "\n" ^
  render_prose_instrs env 0 instrs

let render_rule env name params instrs =
  let (rname, syntax) = name in
  let kwd = (String.uppercase_ascii rname, syntax) in
  let title = render_kwd_title env kwd params in
  title ^ "\n" ^
  String.make (String.length title) '.' ^ "\n" ^
  render_al_instrs env rname 0 instrs

let render_func env fname params instrs =
  let title = render_funcname_title env fname params in 
  title ^ "\n" ^
  String.make (String.length title) '.' ^ "\n" ^
  render_al_instrs env fname 0 instrs

let render_def env = function
  | Pred (name, params, instrs) ->
    "\n" ^ render_pred env name params instrs ^ "\n\n"
  | Algo algo -> (match algo with 
    | Al.Ast.RuleA (name, params, instrs) ->
      "\n" ^ render_rule env name params instrs ^ "\n\n"
    | Al.Ast.FuncA (name, params, instrs) ->
      "\n" ^ render_func env name params instrs ^ "\n\n")

let render_prose env prose = List.map (render_def env) prose |> String.concat ""

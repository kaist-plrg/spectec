open Util
open Source
open Ast


(* Helpers *)

let concat = String.concat
let prefix s f x = s ^ f x
let suffix f s x = f x ^ s
let space f x = " " ^ f x ^ " "


(* Operators *)

let string_of_atom = function
  | Atom atomid -> atomid
  | Bot -> "_|_"
  | Dot -> "."
  | Dot2 -> ".."
  | Dot3 -> "..."
  | Semicolon -> ";"
  | Arrow -> "->"
  | Colon -> ":"
  | Sub -> "<:"
  | SqArrow -> "~>"
  | Tilesturn -> "-|"
  | Turnstile -> "|-"
  | LParen -> "("
  | LBrack -> "["
  | LBrace -> "{"
  | RParen -> ")"
  | RBrack -> "]"
  | RBrace -> "}"
  | Quest -> "?"
  | Star -> "*"

let string_of_unop = function
  | NotOp -> "~"
  | PlusOp -> "+"
  | MinusOp -> "-"

let string_of_binop = function
  | AndOp -> "/\\"
  | OrOp -> "\\/"
  | ImplOp -> "=>"
  | EquivOp -> "<=>"
  | AddOp -> "+"
  | SubOp -> "-"
  | MulOp -> "*"
  | DivOp -> "/"
  | ExpOp -> "^"

let string_of_cmpop = function
  | EqOp -> "="
  | NeOp -> "=/="
  | LtOp -> "<"
  | GtOp -> ">"
  | LeOp -> "<="
  | GeOp -> ">="

let string_of_mixop = function
  | [Atom a]::tail when List.for_all ((=) []) tail -> a
  | mixop ->
    let s =
      String.concat "%" (List.map (
        fun atoms -> String.concat "" (List.map string_of_atom atoms)) mixop
      )
    in
    "`" ^ s ^ "`"


(* Types *)

let rec string_of_iter iter =
  match iter with
  | Opt -> "?"
  | List -> "*"
  | List1 -> "+"
  | ListN exp -> "^" ^ string_of_exp exp

and string_of_typ typ =
  match typ.it with
  | VarT id -> id.it
  | BoolT -> "bool"
  | NatT -> "nat"
  | TextT -> "text"
  | TupT typs -> "(" ^ string_of_typs ", " typs ^ ")"
  | IterT (typ1, iter) -> string_of_typ typ1 ^ string_of_iter iter

and string_of_typ_args typ =
  match typ.it with
  | TupT [] -> ""
  | TupT _ -> string_of_typ typ
  | _ -> "(" ^ string_of_typ typ ^ ")"

and string_of_typs sep typs =
  concat sep (List.map string_of_typ typs)

and string_of_deftyp deftyp =
  match deftyp.it with
  | AliasT typ -> string_of_typ typ
  | NotationT (mixop, typ) -> string_of_typmix (mixop, typ)
  | StructT typfields ->
    "{" ^ concat ", " (List.map string_of_typfield typfields) ^ "}"
  | VariantT (ids, typcases) ->
    "\n  | " ^ concat "\n  | "
      (List.map it ids @ List.map string_of_typcase typcases)

and string_of_typmix (mixop, typ) =
  if mixop = [[]; []] then string_of_typ typ else
  string_of_mixop mixop ^ string_of_typ_args typ

and string_of_typfield (atom, typ, _hints) =
  string_of_atom atom ^ " " ^ string_of_typ typ

and string_of_typcase (atom, typ, _hints) =
  string_of_atom atom ^ string_of_typ_args typ


(* Expressions *)

and string_of_exp exp =
  match exp.it with
  | VarE id -> id.it
  | BoolE b -> string_of_bool b
  | NatE n -> string_of_int n
  | TextE t -> "\"" ^ String.escaped t ^ "\""
  | UnE (unop, exp2) -> string_of_unop unop ^ " " ^ string_of_exp exp2
  | BinE (binop, exp1, exp2) ->
    "(" ^ string_of_exp exp1 ^ space string_of_binop binop ^ string_of_exp exp2 ^ ")"
  | CmpE (cmpop, exp1, exp2) ->
    "(" ^ string_of_exp exp1 ^ space string_of_cmpop cmpop ^ string_of_exp exp2 ^ ")"
  | IdxE (exp1, exp2) -> string_of_exp exp1 ^ "[" ^ string_of_exp exp2 ^ "]"
  | SliceE (exp1, exp2, exp3) ->
    string_of_exp exp1 ^
      "[" ^ string_of_exp exp2 ^ " : " ^ string_of_exp exp3 ^ "]"
  | UpdE (exp1, path, exp2) ->
    string_of_exp exp1 ^
      "[" ^ string_of_path path ^ " = " ^ string_of_exp exp2 ^ "]"
  | ExtE (exp1, path, exp2) ->
    string_of_exp exp1 ^
      "[" ^ string_of_path path ^ " =.. " ^ string_of_exp exp2 ^ "]"
  | StrE expfields ->
    "{" ^ concat ", " (List.map string_of_expfield expfields) ^ "}"
  | DotE (exp1, atom) -> string_of_exp exp1 ^ "." ^ string_of_atom atom
  | CompE (exp1, exp2) -> string_of_exp exp1 ^ " ++ " ^ string_of_exp exp2
  | LenE exp1 -> "|" ^ string_of_exp exp1 ^ "|"
  | TupE exps -> "(" ^ string_of_exps ", " exps ^ ")"
  | MixE (mixop, exp1) -> string_of_mixop mixop ^ string_of_exp_args exp1
  | CallE (id, exp) -> "$" ^ id.it ^ string_of_exp_args exp
  | IterE (exp1, iter) -> string_of_exp exp1 ^ string_of_iter iter
  | OptE expo -> "?(" ^ string_of_exps "" (Option.to_list expo) ^ ")"
  | ListE exps -> "[" ^ string_of_exps " " exps ^ "]"
  | CatE (exp1, exp2) -> string_of_exp exp1 ^ " :: " ^ string_of_exp exp2
  | CaseE (atom, exp1, typ) ->
    string_of_atom atom ^ "_" ^ string_of_typ typ ^ string_of_exp_args exp1
  | SubE (exp1, _typ1, typ2) ->
    "(" ^ string_of_exp exp1 ^ " <: " ^ string_of_typ typ2 ^ ")"

and string_of_exp_args exp =
  match exp.it with
  | TupE [] -> ""
  | TupE _ | SubE _ | BinE _ | CmpE _ -> string_of_exp exp
  | _ -> "(" ^ string_of_exp exp ^ ")"

and string_of_exps sep exps =
  concat sep (List.map string_of_exp exps)

and string_of_expfield (atom, exp) =
  string_of_atom atom ^ " " ^ string_of_exp exp

and string_of_path path =
  match path.it with
  | RootP -> ""
  | IdxP (path1, exp) -> string_of_path path1 ^ "[" ^ string_of_exp exp ^ "]"
  | DotP ({it = RootP; _}, atom) -> string_of_atom atom
  | DotP (path1, atom) -> string_of_path path1 ^ "." ^ string_of_atom atom


(* Definitions *)

let string_of_bind (id, typ) =
  id.it ^ " : " ^ string_of_typ typ

let string_of_binds = function
  | [] -> ""
  | binds -> " {" ^ concat ", " (List.map string_of_bind binds) ^ "}"


let string_of_premise prem =
  match prem.it with
  | RulePr (id, mixop, exp, None) ->
    id.it ^ ": " ^ string_of_exp (MixE (mixop, exp) $ exp.at)
  | RulePr (id, mixop, exp, Some iter) ->
    "(" ^ id.it ^ ": " ^ string_of_exp (MixE (mixop, exp) $ exp.at) ^ ")" ^ string_of_iter iter
  | IffPr (exp, None) -> "iff " ^ string_of_exp exp
  | IffPr (exp, Some iter) ->
    "(" ^ "iff " ^ string_of_exp exp ^ ")" ^ string_of_iter iter
  | ElsePr -> "otherwise"

let string_of_rule rule =
  match rule.it with
  | RuleD (id, binds, mixop, exp, prems) ->
    let id' = if id.it = "" then "_" else id.it in
    "\n  ;; " ^ string_of_region rule.at ^ "\n" ^
    "  rule " ^ id' ^ string_of_binds binds ^ ":\n    " ^
      string_of_exp (MixE (mixop, exp) $ exp.at) ^
      concat "" (List.map (prefix "\n    -- " string_of_premise) prems)

let string_of_clause id clause =
  match clause.it with
  | DefD (binds, exp1, exp2, prems) ->
    "\n  ;; " ^ string_of_region clause.at ^ "\n" ^
    "  def" ^ string_of_binds binds ^ " " ^ id.it ^ string_of_exp_args exp1 ^ " = " ^
      string_of_exp exp2 ^
      concat "" (List.map (prefix "\n    -- " string_of_premise) prems)

let rec string_of_def def =
  "\n;; " ^ string_of_region def.at ^ "\n" ^
  match def.it with
  | SynD (id, deftyp, _hints) ->
    "syntax " ^ id.it ^ " = " ^ string_of_deftyp deftyp
  | RelD (id, mixop, typ, rules, _hints) ->
    "relation " ^ id.it ^ ": " ^ string_of_typmix (mixop, typ) ^
      concat "\n" (List.map string_of_rule rules)
  | DecD (id, typ1, typ2, clauses, _hints) ->
    let s1 =
      match typ1.it with
      | TupT [] -> ""
      | _ -> string_of_typ typ1 ^ " -> "
    in
    "def " ^ id.it ^ " : " ^ s1 ^ string_of_typ typ2 ^
      concat "" (List.map (string_of_clause id) clauses)
  | RecD defs ->
    "rec {\n" ^ concat "\n" (List.map string_of_def defs) ^ "\n}"


(* Scripts *)

let string_of_script defs =
  concat "" (List.map (suffix string_of_def "\n") defs)

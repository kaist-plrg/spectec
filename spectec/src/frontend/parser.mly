%{
open Util
open Source
open El.Ast


(* Position handling *)

let position_to_pos position =
  { file = position.Lexing.pos_fname;
    line = position.Lexing.pos_lnum;
    column = position.Lexing.pos_cnum - position.Lexing.pos_bol
  }

let positions_to_region position1 position2 =
  { left = position_to_pos position1;
    right = position_to_pos position2
  }

let at (l, r) = positions_to_region l r

let ($$) it at = {it; at; note = ref ""}


(* Conversions *)

let as_seq_typ typ =
  match typ.it with
  | SeqT (_::_::_ as typs) -> typs
  | _ -> [typ]

let as_seq_exp exp =
  match exp.it with
  | SeqE (_::_::_ as exps) -> exps
  | _ -> [exp]

let as_seq_sym sym =
  match sym.it with
  | SeqG (_::_::_ as syms) -> syms
  | _ -> [Elem sym]

let as_alt_sym sym =
  match sym.it with
  | AltG (_::_::_ as syms) -> syms
  | _ -> [Elem sym]


(* Parentheses Role etc *)

type prec = Op | Seq | Post | Prim

let prec_of_exp = function  (* as far as iteration is concerned *)
  | VarE _ | BoolE _ | NatE _ | TextE _ | EpsE | StrE _
  | ParenE _ | TupE _ | BrackE _ | CallE _ | HoleE _ -> Prim
  | AtomE _ | IdxE _ | SliceE _ | UpdE _ | ExtE _ | DotE _ | IterE _ -> Post
  | SeqE _ -> Seq
  | UnE _ | BinE _ | CmpE _ | InfixE _ | LenE _ | SizeE _
  | CommaE _ | CompE _ | TypE _ | FuseE _ -> Op

(* Extra parentheses can be inserted to disambiguate the role of elements of
 * an iteration. For example, `( x* )` will be interpreted differently from `x*`
 * in a place where an expression of some type `t*` is expected. In particular,
 * we assume `x* : t*` in the latter case, but `x* : t` in the former
 * (which makes sense in the case where `t` itself is an iteration type).
 * To make this distinction ducing elaboration, we mark potential parentheses
 * as "significant" (true) when they are not syntactically enforced, and instead
 * are assumed to have been inserted to express iteration injection.
 *)
let signify_pars prec = function
  | ParenE (exp, false) -> ParenE (exp, prec < prec_of_exp exp.it)
  | exp' -> exp'

let is_post_exp e =
  match e.it with
  | VarE _ | AtomE _
  | BoolE _ | NatE _
  | EpsE
  | ParenE _ | TupE _ | BrackE _
  | IdxE _ | SliceE _ | ExtE _
  | StrE _ | DotE _
  | IterE _ | CallE _
  | HoleE _ -> true
  | _ -> false

%}

%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token COLON SEMICOLON COMMA DOT DOTDOT DOTDOTDOT BAR BARBAR DASH
%token COMMA_NL NL_BAR NL_NL_DASH NL_NL_NL
%token EQ NE LT GT LE GE APPROX EQUIV ASSIGN SUB SUP EQDOT2
%token NOT AND OR
%token QUEST PLUS MINUS STAR SLASH BACKSLASH UP COMPOSE
%token IN ARROW ARROW2 DARROW2 SQARROW SQARROWSTAR PREC SUCC TURNSTILE TILESTURN
%token DOLLAR TICK
%token BOT TOP
%token HOLE MULTIHOLE SKIP MULTISKIP FUSE
%token<int> HOLEN SKIPN
%token BOOL NAT INT RAT REAL TEXT
%token SYNTAX GRAMMAR RELATION RULE VAR DEF
%token IF OTHERWISE HINT_LPAREN
%token EPS INFINITY
%token<bool> BOOLLIT
%token<int> NATLIT HEXLIT CHARLIT
%token<string> TEXTLIT
%token<string> UPID LOID DOTID UPID_LPAREN LOID_LPAREN
%token EOF

%right ARROW2 DARROW2
%left OR
%left AND
%nonassoc TURNSTILE
%nonassoc TILESTURN
%right SQARROW SQARROWSTAR PREC SUCC
%left COLON SUB SUP ASSIGN EQUIV APPROX
%left COMMA COMMA_NL
%right EQ NE LT GT LE GE IN
%right ARROW
%left SEMICOLON
%left DOT DOTDOT DOTDOTDOT
%left PLUS MINUS COMPOSE
%left STAR SLASH BACKSLASH

%start script typ_eof exp_eof check_atom
%type<El.Ast.script> script
%type<El.Ast.typ> typ_eof
%type<El.Ast.exp> exp_eof
%type<bool> check_atom

%%

(* Lists *)

%inline bar :
  | BAR {}
  | NL_BAR {}

%inline comma :
  | COMMA {}
  | COMMA_NL {}

tup_list(X) :
  | (* empty *) { [], true }
  | X { $1::[], false }
  | X comma tup_list(X) { $1::(fst $3), true }

comma_list(X) :
  | tup_list(X) { fst $1 }

comma_nl_list(X) :
  | (* empty *) { [] }
  | X { (Elem $1)::[] }
  | X COMMA comma_nl_list(X) { (Elem $1)::$3 }
  | X COMMA_NL comma_nl_list(X) { (Elem $1)::Nl::$3 }

nl_bar_list(X) : nl_bar_list1(X, X) { $1 }
nl_bar_list1(X, Y) :
  | X { Elem $1 :: [] }
  | X BAR nl_bar_list(Y) { (Elem $1)::$3 }
  | X NL_BAR nl_bar_list(Y) { (Elem $1)::Nl::$3 }

nl_dash_list(X) :
  | (* empty *) { [] }
  | DASH X nl_dash_list(X) { (Elem $2)::$3 }
  | NL_NL_DASH X nl_dash_list(X) { Nl::(Elem $2)::$3 }

%inline dots :
  | DOTDOTDOT {}
  | bar DOTDOTDOT {}

dots_list(X) :
  | bar dots_list1(X) { let x, y = $2 in (NoDots, x, y) }
  | dots BAR dots_list1(X) { let x, y = $3 in (Dots, x, y) }
  | dots NL_BAR dots_list1(X) { let x, y = $3 in (Dots, Nl::x, y) }

dots_list1(X) :
  | (* empty *) { [], NoDots }
  | DOTDOTDOT { [], Dots }
  | X { (Elem $1)::[], NoDots }
  | X BAR dots_list1(X) { let x, y = $3 in (Elem $1)::x, y }
  | X NL_BAR dots_list1(X) { let x, y = $3 in (Elem $1)::Nl::x, y }


(* Identifiers *)

id : UPID { $1 } | LOID { $1 }
id_lparen : UPID_LPAREN { $1 } | LOID_LPAREN { $1 }

atomid_ : UPID { $1 }
varid : LOID { $1 $ at $sloc }
defid : id { $1 $ at $sloc } | IF { "if" $ at $sloc }
relid : id { $1 $ at $sloc }
gramid : id { $1 $ at $sloc }
hintid : id { $1 }
fieldid : atomid_ { Atom $1 $$ at $sloc }
dotid : DOTID { Atom $1 $$ at $sloc }

atomid_lparen : UPID_LPAREN { $1 }
varid_lparen : LOID_LPAREN { $1 $ at $sloc }
defid_lparen : id_lparen { $1 $ at $sloc }
gramid_lparen : id_lparen { $1 $ at $sloc }

ruleid : ruleid_ { $1 }
ruleid_ :
  | id { $1 }
  | NATLIT { Int.to_string $1 }
  | BOOLLIT { Bool.to_string $1 }
  | IF { "if" }
  | VAR { "var" }
  | DEF { "def" }
  | ruleid_ DOTID { $1 ^ "." ^ $2 }
atomid : atomid_ { $1 } | atomid DOTID { $1 ^ "." ^ $2 }

atom :
  | atom_ { $1 $$ at $sloc }
atom_ :
  | atomid { Atom $1 }
  | TICK QUEST { Quest }
  | TICK PLUS { Plus }
  | TICK STAR { Star }
  | BOT { Bot }
  | TOP { Top }
  | INFINITY { Infinity }

varid_bind :
  | varid { $1 }
  | atomid_ { Atom.make_var $1; $1 $ at $sloc }
varid_bind_lparen :
  | varid_lparen { $1 }
  | atomid_lparen { Atom.make_var $1; $1 $ at $sloc }

enter_scope :
  | (* empty *) { Atom.enter_scope () }
exit_scope :
  | (* empty *) { Atom.exit_scope () }

check_atom :
  | UPID EOF { Atom.is_var (El.Convert.strip_var_suffix ($1 $ at $sloc)).it }


(* Operators *)

%inline unop :
  | NOT { NotOp }
  | PLUS { PlusOp }
  | MINUS { MinusOp }

%inline binop :
  | PLUS { AddOp }
  | MINUS { SubOp }
  | STAR { MulOp }
  | SLASH { DivOp }

%inline cmpop :
  | EQ { EqOp }
  | NE { NeOp }
  | LT { LtOp }
  | GT { GtOp }
  | LE { LeOp }
  | GE { GeOp }

%inline boolop :
  | AND { AndOp }
  | OR { OrOp }
  | ARROW2 { ImplOp }
  | DARROW2 { EquivOp }

%inline infixop :
  | infixop_ { $1 $$ at $sloc }
%inline infixop_ :
  | DOT { Dot }
  | DOTDOT { Dot2 }
  | DOTDOTDOT { Dot3 }
  | SEMICOLON { Semicolon }
  | BACKSLASH { Backslash }
  | ARROW { Arrow }

%inline relop :
  | relop_ { $1 $$ at $sloc }
%inline relop_ :
  | COLON { Colon }
  | SUB { Sub }
  | SUP { Sup }
  | ASSIGN { Assign }
  | EQUIV { Equiv }
  | APPROX { Approx }
  | SQARROW { SqArrow }
  | SQARROWSTAR { SqArrowStar }
  | PREC { Prec }
  | SUCC { Succ }
  | TILESTURN { Tilesturn }
  | TURNSTILE { Turnstile }
  | IN { In }


(* Iteration *)

iter :
  | QUEST { Opt }
  | PLUS { List1 }
  | STAR { List }
  | UP arith_prim
    { match $2.it with
      | ParenE ({it = CmpE({it = VarE (id, []); _}, LtOp, e); _}, false) ->
        ListN (e, Some id)
      | _ -> ListN ($2, None)
    }


(* Types *)

typ_prim : typ_prim_ { $1 $ at $sloc }
typ_prim_ :
  | varid { VarT ($1, []) }
  | varid_lparen comma_list(arg) RPAREN { VarT ($1, $2) }
  | BOOL { BoolT }
  | NAT { NumT NatT }
  | INT { NumT IntT }
  | RAT { NumT RatT }
  | REAL { NumT RealT }
  | TEXT { TextT }

typ_post : typ_post_ { $1 $ at $sloc }
typ_post_ :
  | typ_prim_ { $1 }
  | LPAREN tup_list(typ) RPAREN
    { match $2 with [t], false -> ParenT t | ts, _ -> TupT ts }
  | typ_post iter { IterT ($1, $2) }

typ : typ_post { $1 }

deftyp : deftyp_ { $1 $ at $sloc }
deftyp_ :
  | nottyp { $1.it }
  | LBRACE comma_nl_list(fieldtyp) RBRACE { StrT $2 }
  | dots_list(casetyp)
    { let x, y, z = $1 in
      let y1, y2, _ =
        List.fold_right
          (fun elem (y1, y2, at) ->
            (* at is the position of leftmost id element so far *)
            match elem with
            | Nl -> if at = None then y1, Nl::y2, at else Nl::y1, y2, at
            | Elem (t, prems, hints) ->
              match t.it with
              | AtomT atom
              | SeqT ({it = AtomT atom; _}::_)
              | InfixT (_, atom, _)
              | BrackT (atom, _, _) when at = None ->
                y1, (Elem (atom, (t, prems), hints))::y2, at
              | _ when prems = [] && hints = [] ->
                (Elem t)::y1, y2, Some t.at
              | _ ->
                let at = Option.value at ~default:t.at in
                Source.error at "syntax" "misplaced type";
          ) y ([], [], None)
      in CaseT (x, y1, y2, z) }
  | nl_bar_list1(enumtyp(enum1), enumtyp(arith)) { RangeT $1 }


(*nottyp_prim : nottyp_prim_ { $1 $ at $sloc }*)
nottyp_prim_ :
  | typ_prim { $1.it }
  | atom { AtomT $1 }
  | atomid_lparen nottyp RPAREN
    { SeqT [
        AtomT (Atom $1 $$ at $loc($1)) $ at $loc($1);
        ParenT $2 $ at $loc($2)
      ] }
  | TICK LPAREN nottyp RPAREN
    { BrackT (LParen $$ at $loc($2), $3, RParen $$ at $loc($4)) }
  | TICK LBRACK nottyp RBRACK
    { BrackT (LBrack $$ at $loc($2), $3, RBrack $$ at $loc($4)) }
  | TICK LBRACE nottyp RBRACE
    { BrackT (LBrace $$ at $loc($2), $3, RBrace $$ at $loc($4)) }
  | LPAREN tup_list(nottyp) RPAREN
    { match $2 with [t], false -> ParenT t | ts, _ -> TupT ts }

nottyp_post : nottyp_post_ { $1 $ at $sloc }
nottyp_post_ :
  | nottyp_prim_ { $1 }
  | nottyp_post iter { IterT ($1, $2) }

nottyp_seq : nottyp_seq_ { $1 $ at $sloc }
nottyp_seq_ :
  | nottyp_post_ { $1 }
  | nottyp_post nottyp_seq { SeqT ($1 :: as_seq_typ $2) }

nottyp_un : nottyp_un_ { $1 $ at $sloc }
nottyp_un_ :
  | nottyp_seq_ { $1 }
  | infixop nottyp_un { InfixT (SeqT [] $ at $loc($1), $1, $2) }

nottyp_bin : nottyp_bin_ { $1 $ at $sloc }
nottyp_bin_ :
  | nottyp_un_ { $1 }
  | nottyp_bin infixop nottyp_bin { InfixT ($1, $2, $3) }

nottyp_rel : nottyp_rel_ { $1 $ at $sloc }
nottyp_rel_ :
  | nottyp_bin_ { $1 }
  | relop nottyp_rel { InfixT (SeqT [] $ at $loc($1), $1, $2) }
  | nottyp_rel relop nottyp_rel { InfixT ($1, $2, $3) }

nottyp : nottyp_rel { $1 }

fieldtyp :
  | fieldid typ_post hint* premise_bin_list { ($1, ($2, $4), $3) }

casetyp :
  | nottyp hint* premise_list { $1, $3, $2 }

%inline enumtyp(X) :
  | X { ($1, None) }
  | X BAR DOTDOTDOT BAR arith { ($1, Some $5) }

%inline enum1 :
  | exp_lit { $1 }
  | PLUS arith_un { UnE (PlusOp, $2) $ at $sloc }
  | MINUS arith_un { UnE (MinusOp, $2) $ at $sloc }
  | DOLLAR LPAREN exp RPAREN { $3 }


(* Expressions *)

exp_lit : exp_lit_ { $1 $ at $sloc }
exp_lit_ :
  | BOOLLIT { BoolE $1 }
  | NATLIT { NatE (DecOp, $1) }
  | HEXLIT { NatE (HexOp, $1) }
  | CHARLIT { NatE (CharOp, $1) }
  | TEXTLIT { TextE $1 }

exp_var_ :
  | varid { VarE ($1, []) }
  | varid_lparen comma_list(arg) RPAREN { VarE ($1, $2) }
  | BOOL { VarE ("bool" $ at $sloc, []) }
  | NAT { VarE ("nat" $ at $sloc, []) }
  | INT { VarE ("int" $ at $sloc, []) }
  | RAT { VarE ("rat" $ at $sloc, []) }
  | REAL { VarE ("real" $ at $sloc, []) }
  | TEXT { VarE ("text" $ at $sloc, []) }

exp_call_ :
  | DOLLAR defid { CallE ($2, []) }
  | DOLLAR defid_lparen comma_list(arg) RPAREN { CallE ($2, $3) }

exp_hole_ :
  | HOLEN { HoleE (`Use, `Num $1) }
  | HOLE { HoleE (`Use, `Next) }
  | MULTIHOLE { HoleE (`Use, `Rest) }
  | SKIPN { HoleE (`Skip, `Num $1) }
  | SKIP { HoleE (`Skip, `Next) }
  | MULTISKIP { HoleE (`Skip, `Rest) }

(*exp_prim : exp_prim_ { $1 $ at $sloc }*)
exp_prim_ :
  | exp_lit_ { $1 }
  | exp_var_ { $1 }
  | exp_call_ { $1 }
  | exp_hole_ { $1 }
  | EPS { EpsE }
  | LBRACE comma_nl_list(fieldexp) RBRACE { StrE $2 }
  | LPAREN tup_list(exp_bin) RPAREN
    { match $2 with [e], false -> ParenE (e, false) | es, _ -> TupE es }
  | TICK LPAREN exp RPAREN
    { BrackE (LParen $$ at $loc($2), $3, RParen $$ at $loc($4)) }
  | TICK LBRACK exp RBRACK
    { BrackE (LBrack $$ at $loc($2), $3, RBrack $$ at $loc($4)) }
  | TICK LBRACE exp RBRACE
    { BrackE (LBrace $$ at $loc($2), $3, RBrace $$ at $loc($4)) }
  | DOLLAR LPAREN arith RPAREN { $3.it }

exp_post : exp_post_ { $1 $ at $sloc }
exp_post_ :
  | exp_prim_ { signify_pars Post $1 }
  | exp_atom LBRACK arith RBRACK { IdxE ($1, $3) }
  | exp_atom LBRACK arith COLON arith RBRACK { SliceE ($1, $3, $5) }
  | exp_atom LBRACK path EQ exp RBRACK { UpdE ($1, $3, $5) }
  | exp_atom LBRACK path EQDOT2 exp RBRACK { ExtE ($1, $3, $5) }
  | exp_atom iter { IterE ($1, $2) }
  | exp_post dotid { DotE ($1, $2) }

exp_atom : exp_atom_ { $1 $ at $sloc }
exp_atom_ :
  | exp_post_ { $1 }
  | atom { AtomE $1 }
  | atomid_lparen exp RPAREN
    { SeqE [
        AtomE (Atom $1 $$ at $loc($1)) $ at $loc($1);
        ParenE ($2, false) $ at $loc($2)
      ] }

exp_seq : exp_seq_ { $1 $ at $sloc }
exp_seq_ :
  | exp_atom_ { signify_pars Seq $1 }
  | exp_seq exp_atom { SeqE (as_seq_exp $1 @ [$2]) }
  | exp_seq FUSE exp_atom { FuseE ($1, $3) }

exp_un : exp_un_ { $1 $ at $sloc }
exp_un_ :
  | exp_seq_ { signify_pars Op $1 }
  | bar exp bar { LenE $2 }
  | BARBAR gramid BARBAR { SizeE $2 }
  | NOT exp_un { UnE (NotOp, $2) }
  | infixop exp_un { InfixE (SeqE [] $ at $loc($1), $1, $2) }

exp_bin : exp_bin_ { $1 $ at $sloc }
exp_bin_ :
  | exp_un_ { $1 }
  | exp_bin COMPOSE exp_bin { CompE ($1, $3) }
  | exp_bin infixop exp_bin { InfixE ($1, $2, $3) }
  | exp_bin cmpop exp_bin { CmpE ($1, $2, $3) }
  | exp_bin boolop exp_bin { BinE ($1, $2, $3) }

exp_rel : exp_rel_ { $1 $ at $sloc }
exp_rel_ :
  | exp_bin_ { $1 }
  | comma exp_rel { CommaE (SeqE [] $ at $loc($1), $2) }
  | relop exp_rel { InfixE (SeqE [] $ at $loc($1), $1, $2) }
  | exp_rel comma exp_rel { CommaE ($1, $3) }
  | exp_rel relop exp_rel { InfixE ($1, $2, $3) }

exp : exp_rel { $1 }

fieldexp :
  | fieldid exp_post+
    { ($1, match $2 with [e] -> e | es -> SeqE es $ at $loc($2)) }


arith_prim : arith_prim_ { $1 $ at $sloc }
arith_prim_ :
  | exp_lit_ { $1 }
  | exp_var_ { $1 }
  | exp_call_ { $1 }
  | exp_hole_ { $1 }
  | LPAREN arith RPAREN { ParenE ($2, false) }
  | LPAREN arith_bin STAR RPAREN
    { (* HACK: to allow "(s*)" as arithmetic expression. *)
      if not (is_post_exp $2) then
        Source.error (at $loc($3)) "syntax" "misplaced token";
      IterE ($2, List) }
  | LPAREN arith_bin PLUS RPAREN
    { (* HACK: to allow "(s+)" as arithmetic expression. *)
      if not (is_post_exp $2) then
        Source.error (at $loc($3)) "syntax" "misplaced token";
      IterE ($2, List1) }
  | LPAREN arith_bin QUEST RPAREN
    { (* HACK: to allow "(s?)" as arithmetic expression. *)
      if not (is_post_exp $2) then
        Source.error (at $loc($3)) "syntax" "misplaced token";
      IterE ($2, Opt) }
  | DOLLAR LPAREN exp RPAREN { $3.it }

arith_post : arith_post_ { $1 $ at $sloc }
arith_post_ :
  | arith_prim_ { $1 }
  | arith_atom UP arith_prim { BinE ($1, ExpOp, $3) }
  | arith_atom LBRACK arith RBRACK { IdxE ($1, $3) }
  | arith_post dotid { DotE ($1, $2) }

arith_atom : arith_atom_ { $1 $ at $sloc }
arith_atom_ :
  | arith_post_ { $1 }
  | atom { AtomE $1 }

arith_un : arith_un_ { $1 $ at $sloc }
arith_un_ :
  | arith_atom_ { $1 }
  | bar exp bar { LenE $2 }
  | unop arith_un { UnE ($1, $2) }

arith_bin : arith_bin_ { $1 $ at $sloc }
arith_bin_ :
  | arith_un_ { $1 }
  | arith_bin binop arith_bin { BinE ($1, $2, $3) }
  | arith_bin cmpop arith_bin { CmpE ($1, $2, $3) }
  | arith_bin boolop arith_bin { BinE ($1, $2, $3) }

arith : arith_bin { $1 }


path : path_ { $1 $ at $sloc }
path_ :
  | (* empty *) { RootP }
  | path LBRACK arith RBRACK { IdxP ($1, $3) }
  | path LBRACK arith COLON arith RBRACK { SliceP ($1, $3, $5) }
  | path dotid { DotP ($1, $2) }


(* Premises *)

premise_list :
  | nl_dash_list(premise) { $1 }

premise_bin_list :
  | nl_dash_list(premise_bin) { $1 }

(*premise_post : premise_post_ { $1 $ at $sloc }*)
premise_post_ :
  | OTHERWISE { ElsePr }
  | LPAREN premise RPAREN iter*
    { let rec iters prem = function
        | [] -> prem
        | iter::iters' -> iters (IterPr (prem, iter) $ at $sloc) iters'
      in (iters $2 $4).it }

premise_bin : premise_bin_ { $1 $ at $sloc }
premise_bin_ :
  | premise_post_ { $1 }
  | relid COLON exp_bin { RulePr ($1, $3) }
  | IF exp_bin
    { let rec iters e =
        match e.it with
        | IterE (e1, iter) -> IterPr (iters e1 $ e1.at, iter)
        | _ -> IfPr e
      in iters $2 }

premise : premise_ { $1 $ at $sloc }
premise_ :
  | premise_post_ { $1 }
  | relid COLON exp { RulePr ($1, $3) }
  | IF exp
    { let rec iters e =
        match e.it with
        | IterE (e1, iter) -> IterPr (iters e1 $ e1.at, iter)
        | _ -> IfPr e
      in iters $2 }


(* Grammars *)

(*sym_prim : sym_prim_ { $1 $ at $sloc }*)
sym_prim_ :
  | gramid { VarG ($1, []) }
  | gramid_lparen comma_list(arg) RPAREN { VarG ($1, $2) }
  | NATLIT { NatG (DecOp, $1) }
  | HEXLIT { NatG (HexOp, $1) }
  | CHARLIT { NatG (CharOp, $1) }
  | TEXTLIT { TextG $1 }
  | EPS { EpsG }
  | LPAREN tup_list(sym) RPAREN
    { match $2 with [g], false -> ParenG g | gs, _ -> TupG gs }
  | DOLLAR LPAREN arith RPAREN { ArithG $3 }

sym_post : sym_post_ { $1 $ at $sloc }
sym_post_ :
  | sym_prim_ { $1 }
  | sym_post iter { IterG ($1, $2) }

sym_attr : sym_attr_ { $1 $ at $sloc }
sym_attr_ :
  | sym_post_ { $1 }
  | sym_post COLON sym_post { AttrG (El.Convert.exp_of_sym $1, $3) }

sym_seq : sym_seq_ { $1 $ at $sloc }
sym_seq_ :
  | sym_attr_ { $1 }
  | sym_seq sym_attr { SeqG (as_seq_sym $1 @ [Elem $2]) }
  | sym_seq NL_NL_NL sym_attr { SeqG (as_seq_sym $1 @ [Nl; Elem $3]) }

sym_alt : sym_alt_ { $1 $ at $sloc }
sym_alt_ :
  | sym_seq_ { $1 }
  | sym_alt BAR sym_seq { AltG (as_alt_sym $1 @ [Elem $3]) }
  | sym_alt NL_BAR sym_seq { AltG (as_alt_sym $1 @ [Nl; Elem $3]) }
  | sym_alt bar DOTDOTDOT bar sym_seq { RangeG ($1, $5) }

sym : sym_alt { $1 }

prod : prod_ { $1 $ at $sloc }
prod_ :
  | sym ARROW2 exp premise_list { ($1, $3, $4) }

gram :
  | dots_list(prod) { $1 $ at $sloc }


(* Definitions *)

arg : arg_ { ref $1 $ at $sloc }
arg_ :
  | exp_bin { ExpA $1 }
  | SYNTAX typ { SynA $2 }
  | GRAMMAR sym { GramA $2 }

param : param_ { $1 $ at $sloc }
param_ :
  | varid_bind COLON typ { ExpP ($1, $3) }
  | typ { ExpP ("" $ at $sloc, $1) }
  | SYNTAX varid_bind { SynP $2 }
  | GRAMMAR gramid COLON typ { GramP ($2, $4) }


def : def_ { $1 $ at $sloc }
def_ :
  | SYNTAX varid_bind ruleid_list hint* EQ deftyp
    { let id = if $3 = "" then "" else String.sub $3 1 (String.length $3 - 1) in
      SynD ($2, id $ at $loc($3), [], $6, $4) }
  | SYNTAX varid_bind_lparen enter_scope comma_list(param) RPAREN ruleid_list hint* EQ deftyp exit_scope
    { let id = if $6 = "" then "" else String.sub $6 1 (String.length $6 - 1) in
      SynD ($2, id $ at $loc($6), $4, $9, $7) }
  | GRAMMAR varid_bind ruleid_list COLON typ hint* EQ gram
    { let id = if $3 = "" then "" else String.sub $3 1 (String.length $3 - 1) in
      GramD ($2, id $ at $loc($3), [], $5, $8, $6) }
  | GRAMMAR varid_bind_lparen enter_scope comma_list(param) RPAREN ruleid_list COLON typ hint* EQ gram exit_scope
    { let id = if $6 = "" then "" else String.sub $6 1 (String.length $6 - 1) in
      GramD ($2, id $ at $loc($6), $4, $8, $11, $9) }
  | RELATION relid COLON nottyp hint*
    { RelD ($2, $4, $5) }
  | RULE relid ruleid_list COLON exp premise_list
    { let id = if $3 = "" then "" else String.sub $3 1 (String.length $3 - 1) in
      RuleD ($2, id $ at $loc($3), $5, $6) }
  | VAR varid_bind COLON typ hint*
    { VarD ($2, $4, $5) }
  | DEF DOLLAR defid COLON typ hint*
    { DecD ($3, [], $5, $6) }
  | DEF DOLLAR defid_lparen comma_list(arg) RPAREN COLON typ hint*
    { DecD ($3, List.map El.Convert.param_of_arg $4, $7, $8) }
  | DEF DOLLAR defid EQ exp premise_list
    { DefD ($3, [], $5, $6) }
  | DEF DOLLAR defid_lparen comma_list(arg) RPAREN EQ exp premise_list
    { DefD ($3, $4, $7, $8) }
  | NL_NL_NL
    { SepD }
  | SYNTAX varid_bind ruleid_list hint*
    { let id = if $3 = "" then "" else String.sub $3 1 (String.length $3 - 1) in
      HintD (SynH ($2, id $ at $loc($3), $4) $ at $sloc) }
  | SYNTAX varid_bind ruleid_list atomid hint*
    { HintD (AtomH ($4 $ at $loc($4), $5) $ at $sloc) }
  | GRAMMAR varid_bind ruleid_list hint*
    { let id = if $3 = "" then "" else String.sub $3 1 (String.length $3 - 1) in
      HintD (GramH ($2, id $ at $loc($3), $4) $ at $sloc) }
  | RELATION relid hint*
    { HintD (RelH ($2, $3) $ at $sloc) }
  | VAR varid hint*
    { HintD (VarH ($2, $3) $ at $sloc) }
  | DEF DOLLAR defid hint*
    { HintD (DecH ($3, $4) $ at $sloc) }

ruleid_list :
  | (* empty *) { "" }
  | SLASH ruleid ruleid_list { "/" ^ $2 ^ $3 }
  | MINUS ruleid ruleid_list { "-" ^ $2 ^ $3 }


hint :
  | HINT_LPAREN hintid exp RPAREN { {hintid = $2 $ at $loc($2); hintexp = $3} }
  | HINT_LPAREN hintid RPAREN { {hintid = $2 $ at $loc($2); hintexp = SeqE [] $ at $loc($2)} }


(* Scripts *)

script :
  | def* EOF { $1 }

typ_eof :
  | typ EOF { $1 }

exp_eof :
  | exp EOF { $1 }

%%

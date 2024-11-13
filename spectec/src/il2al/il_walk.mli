open Il.Ast

val transform_expr : (exp -> exp) -> exp -> exp
val transform_typ : (exp -> exp) -> typ -> typ
val transform_prem : (exp -> exp) -> prem -> prem

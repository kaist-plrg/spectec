open Il.Ast

val transform_exp : (exp -> exp) -> exp -> exp
val transform_typ : (exp -> exp) -> typ -> typ
val transform_prem : (exp -> exp) -> prem -> prem

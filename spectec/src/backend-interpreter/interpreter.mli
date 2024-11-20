open Al.Ast

val eval_expr: Ds.env -> expr -> value

val instantiate: value list -> value
val invoke: value list -> value

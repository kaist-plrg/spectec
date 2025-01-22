open Al.Ast

val eval_expr: value Ds.Env.t -> expr -> value

val instantiate: value list -> value
val invoke: value list -> value

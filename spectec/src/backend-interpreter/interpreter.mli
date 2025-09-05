open Al.Ast

val instantiate: value list -> value
val invoke: value list -> value
val call_func: string -> value list -> value option

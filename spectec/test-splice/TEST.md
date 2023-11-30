# Preview

```sh
$ (dune exec ../src/exe-watsup/main.exe -- ../spec/wasm-3.0/*.watsup -l --splice-latex -p spec-splice.in.tex -w)
== Parsing...
== Elaboration...
== IL Validation...
prem_to_instr: Invalid prem 2
prem_to_instr: Invalid prem 2
prem_to_instr: Invalid prem 2
prem_to_instr: Invalid prem 2
prem_to_instr: Invalid prem 2
prem_to_instr: Invalid prem 2
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.FUNC_context[x], FUNC_comptype(`%->%`(t_1*{t_1}, t_2*{t_2}))))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], FUNC_comptype(`%->%`(t_1*{t_1}, t_2*{t_2}))))
prem_to_instr: Invalid prem 2
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[y], FUNC_comptype(`%->%`(t_1*{t_1}, t_2*{t_2}))))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.FUNC_context[x], FUNC_comptype(`%->%`(t_1*{t_1}, t_2*{t_2}))))
prem_to_instr: Invalid prem 2
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], FUNC_comptype(`%->%`(t_1*{t_1}, t_2*{t_2}))))
prem_to_instr: Invalid prem 2
prem_to_instr: Invalid prem 2
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[y], FUNC_comptype(`%->%`(t_1*{t_1}, t_2*{t_2}))))
prem_to_instr: Invalid prem 2
prem_to_instr: Invalid prem 2
prem_to_instr: Invalid prem 2
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], STRUCT_comptype(`%%`(mut, zt)*{mut zt})))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], STRUCT_comptype(`%%`(mut, zt)*{mut zt})))
prem_to_instr: Invalid prem 3
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], STRUCT_comptype(yt*{yt})))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], STRUCT_comptype(yt*{yt})))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], ARRAY_comptype(`%%`(mut, zt))))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], ARRAY_comptype(`%%`(mut, zt))))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], ARRAY_comptype(`%%`(mut, zt))))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], ARRAY_comptype(`%%`(mut, (rt <: storagetype)))))
prem_to_instr: Invalid prem 2
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], ARRAY_comptype(`%%`(mut, (t <: storagetype)))))
if_expr_to_instrs: Invalid if_prem (((t = (numtype <: valtype)) \/ (t = (vectype <: valtype))))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], ARRAY_comptype(`%%`(mut, zt))))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], ARRAY_comptype(`%%`(`MUT%?`(?(())), zt))))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], ARRAY_comptype(`%%`(`MUT%?`(?(())), zt))))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], ARRAY_comptype(`%%`(`MUT%?`(?(())), zt))))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x_1], ARRAY_comptype(`%%`(`MUT%?`(?(())), zt_1))))
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x_2], ARRAY_comptype(`%%`(mut, zt_2))))
prem_to_instr: Invalid prem 2
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], ARRAY_comptype(`%%`(`MUT%?`(?(())), zt))))
prem_to_instr: Invalid prem 2
prem_to_instrs: Invalid prem (Expand: `%~~%`(C.TYPE_context[x], ARRAY_comptype(`%%`(`MUT%?`(?(())), zt))))
if_expr_to_instrs: Invalid if_prem (((t = (numtype <: valtype)) \/ (t = (vectype <: valtype))))
prem_to_instr: Invalid prem 2
prem_to_instr: Invalid prem 2
spec-splice.in.tex:59.19: splice replacement error: unknown rule identifier `Instr_ok/convert-*`
[1]
```

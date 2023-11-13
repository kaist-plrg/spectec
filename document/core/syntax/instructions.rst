.. _syntax-instr:

Instructions
------------

.. _syntax-sx:
.. _syntax-const:
.. _syntax-iunop:
.. _syntax-ibinop:
.. _syntax-itestop:
.. _syntax-irelop:
.. _syntax-funop:
.. _syntax-fbinop:
.. _syntax-ftestop:
.. _syntax-frelop:
.. _syntax-instr-numeric:

Numeric Instructions
~~~~~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llcl}
   \production{width} & \X{nn}, \X{mm} &::=&
     \K{32} ~|~ \K{64} \\
   \production{signedness} & \sx &::=&
     \K{u} ~|~ \K{s} \\
   \production{instruction} & \instr &::=&
     \K{i}\X{nn}\K{.}\CONST~\xref{syntax/values}{syntax-int}{\uX{\X{nn}}} ~|~
     \K{f}\X{nn}\K{.}\CONST~\xref{syntax/values}{syntax-float}{\fX{\X{nn}}} \\&&|&
     \K{i}\X{nn}\K{.}\iunop ~|~
     \K{f}\X{nn}\K{.}\funop \\&&|&
     \K{i}\X{nn}\K{.}\ibinop ~|~
     \K{f}\X{nn}\K{.}\fbinop \\&&|&
     \K{i}\X{nn}\K{.}\itestop \\&&|&
     \K{i}\X{nn}\K{.}\irelop ~|~
     \K{f}\X{nn}\K{.}\frelop \\&&|&
     \K{i}\X{nn}\K{.}\EXTEND\K{8\_s} ~|~
     \K{i}\X{nn}\K{.}\EXTEND\K{16\_s} ~|~
     \K{i64.}\EXTEND\K{32\_s} \\&&|&
     \K{i32.}\WRAP\K{\_i64} ~|~
     \K{i64.}\EXTEND\K{\_i32}\K{\_}\sx ~|~
     \K{i}\X{nn}\K{.}\TRUNC\K{\_f}\X{mm}\K{\_}\sx \\&&|&
     \K{i}\X{nn}\K{.}\TRUNC\K{\_sat\_f}\X{mm}\K{\_}\sx \\&&|&
     \K{f32.}\DEMOTE\K{\_f64} ~|~
     \K{f64.}\PROMOTE\K{\_f32} ~|~
     \K{f}\X{nn}\K{.}\CONVERT\K{\_i}\X{mm}\K{\_}\sx \\&&|&
     \K{i}\X{nn}\K{.}\REINTERPRET\K{\_f}\X{nn} ~|~
     \K{f}\X{nn}\K{.}\REINTERPRET\K{\_i}\X{nn} \\&&|&
     \dots \\
   \production{integer unary operator} & \iunop &::=&
     \K{clz} ~|~
     \K{ctz} ~|~
     \K{popcnt} \\
   \production{integer binary operator} & \ibinop &::=&
     \K{add} ~|~
     \K{sub} ~|~
     \K{mul} ~|~
     \K{div\_}\sx ~|~
     \K{rem\_}\sx \\&&|&
     \K{and} ~|~
     \K{or} ~|~
     \K{xor} ~|~
     \K{shl} ~|~
     \K{shr\_}\sx ~|~
     \K{rotl} ~|~
     \K{rotr} \\
   \production{floating-point unary operator} & \funop &::=&
     \K{abs} ~|~
     \K{neg} ~|~
     \K{sqrt} ~|~
     \K{ceil} ~|~ 
     \K{floor} ~|~ 
     \K{trunc} ~|~ 
     \K{nearest} \\
   \production{floating-point binary operator} & \fbinop &::=&
     \K{add} ~|~
     \K{sub} ~|~
     \K{mul} ~|~
     \K{div} ~|~
     \K{min} ~|~
     \K{max} ~|~
     \K{copysign} \\
   \production{integer test operator} & \itestop &::=&
     \K{eqz} \\
   \production{integer relational operator} & \irelop &::=&
     \K{eq} ~|~
     \K{ne} ~|~
     \K{lt\_}\sx ~|~
     \K{gt\_}\sx ~|~
     \K{le\_}\sx ~|~
     \K{ge\_}\sx \\
   \production{floating-point relational operator} & \frelop &::=&
     \K{eq} ~|~
     \K{ne} ~|~
     \K{lt} ~|~
     \K{gt} ~|~
     \K{le} ~|~
     \K{ge} \\
   \end{array}

.. _syntax-unop:
.. _syntax-binop:
.. _syntax-testop:
.. _syntax-relop:
.. _syntax-cvtop:

Conventions
...........

Occasionally, it is convenient to group operators together according to the following grammar shorthands:

.. math::
   \begin{array}{llll}
   \production{unary operator} & \unop &::=&
     \iunop ~|~
     \funop ~|~
     \EXTEND{N}\K{\_s} \\
   \production{binary operator} & \binop &::=& \ibinop ~|~ \fbinop \\
   \production{test operator} & \testop &::=& \itestop \\
   \production{relational operator} & \relop &::=& \irelop ~|~ \frelop \\
   \production{conversion operator} & \cvtop &::=&
     \WRAP ~|~
     \EXTEND ~|~
     \TRUNC ~|~
     \TRUNC\K{\_sat} ~|~
     \CONVERT ~|~
     \DEMOTE ~|~
     \PROMOTE ~|~
     \REINTERPRET \\
   \end{array}

.. _syntax-ref.null:
.. _syntax-ref.is_null:
.. _syntax-ref.func:
.. _syntax-instr-ref:

Reference Instructions
~~~~~~~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llcl}
   \production{instruction} & \instr &::=&
     \dots \\&&|&
     \REFNULL~\reftype \\&&|&
     \REFISNULL \\&&|&
     \REFFUNC~\funcidx \\
   \end{array}

.. _syntax-instr-parametric:

Parametric Instructions
~~~~~~~~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llcl}
   \production{instruction} & \instr &::=&
     \dots \\&&|&
     \DROP \\&&|&
     \SELECT~(\valtype^\ast)^? \\
   \end{array}

.. _syntax-instr-variable:

Variable Instructions
~~~~~~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llcl}
   \production{instruction} & \instr &::=&
     \dots \\&&|&
     \LOCALGET~\localidx \\&&|&
     \LOCALSET~\localidx \\&&|&
     \LOCALTEE~\localidx \\&&|&
     \GLOBALGET~\globalidx \\&&|&
     \GLOBALSET~\globalidx \\
   \end{array}

.. _syntax-instr-table:
.. _syntax-table.get:
.. _syntax-table.set:
.. _syntax-table.size:
.. _syntax-table.grow:
.. _syntax-table.fill:

Table Instructions
~~~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llcl}
   \production{instruction} & \instr &::=&
     \dots \\&&|&
     \TABLEGET~\tableidx \\&&|&
     \TABLESET~\tableidx \\&&|&
     \TABLESIZE~\tableidx \\&&|&
     \TABLEGROW~\tableidx \\&&|&
     \TABLEFILL~\tableidx \\&&|&
     \TABLECOPY~\tableidx~\tableidx \\&&|&
     \TABLEINIT~\tableidx~\elemidx \\&&|&
     \ELEMDROP~\elemidx \\
   \end{array}

.. _syntax-loadn:
.. _syntax-storen:
.. _syntax-memarg:
.. _syntax-lanewidth:
.. _syntax-instr-memory:

Memory Instructions
~~~~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llcl}
   \production{memory immediate} & \memarg &::=&
     \{ \OFFSET~\u32, \ALIGN~\u32 \} \\
   \production{lane width} & \X{ww} &::=&
     8 ~|~ 16 ~|~ 32 ~|~ 64 \\
   \production{instruction} & \instr &::=&
     \dots \\&&|&
     \K{i}\X{nn}\K{.}\LOAD~\memarg ~|~
     \K{f}\X{nn}\K{.}\LOAD~\memarg ~|~
     \K{i}\X{nn}\K{.}\STORE~\memarg ~|~
     \K{f}\X{nn}\K{.}\STORE~\memarg ~|~
     \K{i}\X{nn}\K{.}\LOAD\K{8\_}\sx~\memarg ~|~
     \K{i}\X{nn}\K{.}\LOAD\K{16\_}\sx~\memarg ~|~
     \K{i64.}\LOAD\K{32\_}\sx~\memarg \\&&|&
     \K{i}\X{nn}\K{.}\STORE\K{8}~\memarg ~|~
     \K{i}\X{nn}\K{.}\STORE\K{16}~\memarg ~|~
     \K{i64.}\STORE\K{32}~\memarg \\&&|&
     \MEMORYSIZE \\&&|&
     \MEMORYGROW \\&&|&
     \MEMORYFILL \\&&|&
     \MEMORYCOPY \\&&|&
     \MEMORYINIT~\dataidx \\&&|&
     \DATADROP~\dataidx \\
   \end{array}

.. _syntax-blocktype:
.. _syntax-nop:
.. _syntax-unreachable:
.. _syntax-block:
.. _syntax-loop:
.. _syntax-if:
.. _syntax-br:
.. _syntax-br_if:
.. _syntax-br_table:
.. _syntax-return:
.. _syntax-call:
.. _syntax-call_indirect:
.. _syntax-instr-seq:
.. _syntax-instr-control:

Control Instructions
~~~~~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llcl}
   \production{block type} & \blocktype &::=&
     \typeidx ~|~ \valtype^? \\
   \production{instruction} & \instr &::=&
     \dots \\&&|&
     \NOP \\&&|&
     \UNREACHABLE \\&&|&
     \BLOCK~\blocktype~\instr^\ast~\END \\&&|&
     \LOOP~\blocktype~\instr^\ast~\END \\&&|&
     \IF~\blocktype~\instr^\ast~\ELSE~\instr^\ast~\END \\&&|&
     \BR~\labelidx \\&&|&
     \BRIF~\labelidx \\&&|&
     \BRTABLE~\vec(\labelidx)~\labelidx \\&&|&
     \RETURN \\&&|&
     \CALL~\funcidx \\&&|&
     \CALLINDIRECT~\tableidx~\typeidx \\
   \end{array}

.. _syntax-expr:

Expressions
~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{expression} & \expr &::=&
     \instr^\ast~\END \\
   \end{array}

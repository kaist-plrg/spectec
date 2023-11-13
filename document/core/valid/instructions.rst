.. _valid-instr:
.. _syntax-stacktype:
.. _syntax-opdtype:

Instructions
------------

.. _valid-instr-numeric:

Numeric Instructions
~~~~~~~~~~~~~~~~~~~~

.. _valid-const:

:math:`t\K{.}\CONST~c`
......................

.. math::
   \frac{
   }{
     C \vdashinstr t\K{.}\CONST~c : [] \to [t]
   }


.. _valid-unop:

:math:`t\K{.}\unop`
...................

.. math::
   \frac{
   }{
     C \vdashinstr t\K{.}\unop : [t] \to [t]
   }


.. _valid-binop:

:math:`t\K{.}\binop`
....................

.. math::
   \frac{
   }{
     C \vdashinstr t\K{.}\binop : [t~t] \to [t]
   }


.. _valid-testop:

:math:`t\K{.}\testop`
.....................

.. math::
   \frac{
   }{
     C \vdashinstr t\K{.}\testop : [t] \to [\I32]
   }


.. _valid-relop:

:math:`t\K{.}\relop`
....................

.. math::
   \frac{
   }{
     C \vdashinstr t\K{.}\relop : [t~t] \to [\I32]
   }


.. _valid-cvtop:

:math:`t_2\K{.}\cvtop\K{\_}t_1\K{\_}\sx^?`
..........................................

.. math::
   \frac{
   }{
     C \vdashinstr t_2\K{.}\cvtop\K{\_}t_1\K{\_}\sx^? : [t_1] \to [t_2]
   }

.. _valid-instr-ref:

Reference Instructions
~~~~~~~~~~~~~~~~~~~~~~

.. _valid-ref.null:

:math:`\REFNULL~t`
..................

.. math::
   \frac{
   }{
     C \vdashinstr \REFNULL~t : [] \to [t]
   }

.. note::
   In future versions of WebAssembly, there may be reference types for which no null reference is allowed.

.. _valid-ref.is_null:

:math:`\REFISNULL`
..................

.. math::
   \frac{
     t = \reftype
   }{
     C \vdashinstr \REFISNULL : [t] \to [\I32]
   }

.. _valid-ref.func:

:math:`\REFFUNC~x`
..................

.. math::
   \frac{
     C.\CFUNCS[x] = \functype
     \qquad
     x \in C.\CREFS
   }{
     C \vdashinstr \REFFUNC~x : [] \to [\FUNCREF]
   }

.. _valid-instr-parametric:

Parametric Instructions
~~~~~~~~~~~~~~~~~~~~~~~

.. _valid-drop:

:math:`\DROP`
.............

.. math::
   \frac{
   }{
     C \vdashinstr \DROP : [t] \to []
   }

.. _valid-select:

:math:`\SELECT~(t^\ast)^?`
..........................

.. math::
   \frac{
   }{
     C \vdashinstr \SELECT~t : [t~t~\I32] \to [t]
   }
   \qquad
   \frac{
     \vdash t \leq \numtype
   }{
     C \vdashinstr \SELECT : [t~t~\I32] \to [t]
   }
   \qquad
   \frac{
     \vdash t \leq \vectype
   }{
     C \vdashinstr \SELECT : [t~t~\I32] \to [t]
   }

.. _valid-instr-variable:

Variable Instructions
~~~~~~~~~~~~~~~~~~~~~

.. _valid-local.get:

:math:`\LOCALGET~x`
...................

.. math::
   \frac{
     C.\CLOCALS[x] = t
   }{
     C \vdashinstr \LOCALGET~x : [] \to [t]
   }


.. _valid-local.set:

:math:`\LOCALSET~x`
...................

.. math::
   \frac{
     C.\CLOCALS[x] = t
   }{
     C \vdashinstr \LOCALSET~x : [t] \to []
   }


.. _valid-local.tee:

:math:`\LOCALTEE~x`
...................

.. math::
   \frac{
     C.\CLOCALS[x] = t
   }{
     C \vdashinstr \LOCALTEE~x : [t] \to [t]
   }


.. _valid-global.get:

:math:`\GLOBALGET~x`
....................

.. math::
   \frac{
     C.\CGLOBALS[x] = \mut~t
   }{
     C \vdashinstr \GLOBALGET~x : [] \to [t]
   }


.. _valid-global.set:

:math:`\GLOBALSET~x`
....................

.. math::
   \frac{
     C.\CGLOBALS[x] = \MVAR~t
   }{
     C \vdashinstr \GLOBALSET~x : [t] \to []
   }

.. _valid-instr-table:

Table Instructions
~~~~~~~~~~~~~~~~~~

.. _valid-table.get:

:math:`\TABLEGET~x`
...................

.. math::
   \frac{
     C.\CTABLES[x] = \limits~t
   }{
     C \vdashinstr \TABLEGET~x : [\I32] \to [t]
   }


.. _valid-table.set:

:math:`\TABLESET~x`
...................

.. math::
   \frac{
     C.\CTABLES[x] = \limits~t
   }{
     C \vdashinstr \TABLESET~x : [\I32~t] \to []
   }


.. _valid-table.size:

:math:`\TABLESIZE~x`
....................

.. math::
   \frac{
     C.\CTABLES[x] = \tabletype
   }{
     C \vdashinstr \TABLESIZE~x : [] \to [\I32]
   }


.. _valid-table.grow:

:math:`\TABLEGROW~x`
....................

.. math::
   \frac{
     C.\CTABLES[x] = \limits~t
   }{
     C \vdashinstr \TABLEGROW~x : [t~\I32] \to [\I32]
   }


.. _valid-table.fill:

:math:`\TABLEFILL~x`
....................

.. math::
   \frac{
     C.\CTABLES[x] = \limits~t
   }{
     C \vdashinstr \TABLEFILL~x : [\I32~t~\I32] \to []
   }


.. _valid-table.copy:

:math:`\TABLECOPY~x~y`
......................

.. math::
   \frac{
     C.\CTABLES[x] = \limits_1~t
     \qquad
     C.\CTABLES[y] = \limits_2~t
   }{
     C \vdashinstr \TABLECOPY~x~y : [\I32~\I32~\I32] \to []
   }


.. _valid-table.init:

:math:`\TABLEINIT~x~y`
......................

.. math::
   \frac{
     C.\CTABLES[x] = \limits~t
     \qquad
     C.\CELEMS[y] = t
   }{
     C \vdashinstr \TABLEINIT~x~y : [\I32~\I32~\I32] \to []
   }


.. _valid-elem.drop:

:math:`\ELEMDROP~x`
...................

.. math::
   \frac{
     C.\CELEMS[x] = t
   }{
     C \vdashinstr \ELEMDROP~x : [] \to []
   }

.. _valid-memarg:
.. _valid-instr-memory:

Memory Instructions
~~~~~~~~~~~~~~~~~~~

.. _valid-load:

:math:`t\K{.}\LOAD~\memarg`
...........................

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
     \qquad
     2^{\memarg.\ALIGN} \leq |t|/8
   }{
     C \vdashinstr t\K{.load}~\memarg : [\I32] \to [t]
   }


.. _valid-loadn:

:math:`t\K{.}\LOAD{N}\K{\_}\sx~\memarg`
.......................................

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
     \qquad
     2^{\memarg.\ALIGN} \leq N/8
   }{
     C \vdashinstr t\K{.load}N\K{\_}\sx~\memarg : [\I32] \to [t]
   }


:math:`t\K{.}\STORE~\memarg`
............................

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
     \qquad
     2^{\memarg.\ALIGN} \leq |t|/8
   }{
     C \vdashinstr t\K{.store}~\memarg : [\I32~t] \to []
   }


.. _valid-storen:

:math:`t\K{.}\STORE{N}~\memarg`
...............................

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
     \qquad
     2^{\memarg.\ALIGN} \leq N/8
   }{
     C \vdashinstr t\K{.store}N~\memarg : [\I32~t] \to []
   }

.. _valid-memory.size:

:math:`\MEMORYSIZE`
...................

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
   }{
     C \vdashinstr \MEMORYSIZE : [] \to [\I32]
   }


.. _valid-memory.grow:

:math:`\MEMORYGROW`
...................

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
   }{
     C \vdashinstr \MEMORYGROW : [\I32] \to [\I32]
   }


.. _valid-memory.fill:

:math:`\MEMORYFILL`
...................

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
   }{
     C \vdashinstr \MEMORYFILL : [\I32~\I32~\I32] \to []
   }


.. _valid-memory.copy:

:math:`\MEMORYCOPY`
...................

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
   }{
     C \vdashinstr \MEMORYCOPY : [\I32~\I32~\I32] \to []
   }


.. _valid-memory.init:

:math:`\MEMORYINIT~x`
.....................

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
     \qquad
     C.\CDATAS[x] = {\ok}
   }{
     C \vdashinstr \MEMORYINIT~x : [\I32~\I32~\I32] \to []
   }


.. _valid-data.drop:

:math:`\DATADROP~x`
...................

.. math::
   \frac{
     C.\CDATAS[x] = {\ok}
   }{
     C \vdashinstr \DATADROP~x : [] \to []
   }

.. _valid-label:
.. _valid-instr-control:

Control Instructions
~~~~~~~~~~~~~~~~~~~~

.. _valid-nop:

:math:`\NOP`
............

.. math::
   \frac{
   }{
     C \vdashinstr \NOP : [] \to []
   }


.. _valid-unreachable:

:math:`\UNREACHABLE`
....................

.. math::
   \frac{
   }{
     C \vdashinstr \UNREACHABLE : [t_1^\ast] \to [t_2^\ast]
   }

.. _valid-block:

:math:`\BLOCK~\blocktype~\instr^\ast~\END`
..........................................

.. math::
   \frac{
     C \vdashblocktype \blocktype : [t_1^\ast] \to [t_2^\ast]
     \qquad
     C,\CLABELS\,[t_2^\ast] \vdashinstrseq \instr^\ast : [t_1^\ast] \to [t_2^\ast]
   }{
     C \vdashinstr \BLOCK~\blocktype~\instr^\ast~\END : [t_1^\ast] \to [t_2^\ast]
   }

.. _valid-loop:

:math:`\LOOP~\blocktype~\instr^\ast~\END`
.........................................

.. math::
   \frac{
     C \vdashblocktype \blocktype : [t_1^\ast] \to [t_2^\ast]
     \qquad
     C,\CLABELS\,[t_1^\ast] \vdashinstrseq \instr^\ast : [t_1^\ast] \to [t_2^\ast]
   }{
     C \vdashinstr \LOOP~\blocktype~\instr^\ast~\END : [t_1^\ast] \to [t_2^\ast]
   }

.. _valid-if:

:math:`\IF~\blocktype~\instr_1^\ast~\ELSE~\instr_2^\ast~\END`
.............................................................

.. math::
   \frac{
     C \vdashblocktype \blocktype : [t_1^\ast] \to [t_2^\ast]
     \qquad
     C,\CLABELS\,[t_2^\ast] \vdashinstrseq \instr_1^\ast : [t_1^\ast] \to [t_2^\ast]
     \qquad
     C,\CLABELS\,[t_2^\ast] \vdashinstrseq \instr_2^\ast : [t_1^\ast] \to [t_2^\ast]
   }{
     C \vdashinstr \IF~\blocktype~\instr_1^\ast~\ELSE~\instr_2^\ast~\END : [t_1^\ast~\I32] \to [t_2^\ast]
   }

.. _valid-br:

:math:`\BR~l`
.............

.. math::
   \frac{
     C.\CLABELS[l] = [t^\ast]
   }{
     C \vdashinstr \BR~l : [t_1^\ast~t^\ast] \to [t_2^\ast]
   }

.. _valid-br_if:

:math:`\BRIF~l`
...............

.. math::
   \frac{
     C.\CLABELS[l] = [t^\ast]
   }{
     C \vdashinstr \BRIF~l : [t^\ast~\I32] \to [t^\ast]
   }

.. _valid-br_table:

:math:`\BRTABLE~l^\ast~l_N`
...........................


.. math::
   \frac{
     (\vdash [t^\ast] \leq C.\CLABELS[l])^\ast
     \qquad
     \vdash [t^\ast] \leq C.\CLABELS[l_N]
   }{
     C \vdashinstr \BRTABLE~l^\ast~l_N : [t_1^\ast~t^\ast~\I32] \to [t_2^\ast]
   }

.. _valid-return:

:math:`\RETURN`
...............

.. math::
   \frac{
     C.\CRETURN = [t^\ast]
   }{
     C \vdashinstr \RETURN : [t_1^\ast~t^\ast] \to [t_2^\ast]
   }

.. _valid-call:

:math:`\CALL~x`
...............

.. math::
   \frac{
     C.\CFUNCS[x] = [t_1^\ast] \to [t_2^\ast]
   }{
     C \vdashinstr \CALL~x : [t_1^\ast] \to [t_2^\ast]
   }

.. _valid-call_indirect:

:math:`\CALLINDIRECT~x~y`
.........................

.. math::
   \frac{
     C.\CTABLES[x] = \limits~\FUNCREF
     \qquad
     C.\CTYPES[y] = [t_1^\ast] \to [t_2^\ast]
   }{
     C \vdashinstr \CALLINDIRECT~x~y : [t_1^\ast~\I32] \to [t_2^\ast]
   }

.. _valid-instr-seq:

Instruction Sequences
~~~~~~~~~~~~~~~~~~~~~

Empty Instruction Sequence: :math:`\epsilon`
............................................

.. math::
   \frac{
   }{
     C \vdashinstrseq \epsilon : [t^\ast] \to [t^\ast]
   }

Non-empty Instruction Sequence: :math:`\instr^\ast~\instr_N`
............................................................

.. math::
   \frac{
     C \vdashinstrseq \instr^\ast : [t_1^\ast] \to [t_0^\ast~{t'}^\ast]
     \qquad
     \vdash [{t'}^\ast] \leq [t^\ast]
     \qquad
     C \vdashinstr \instr_N : [t^\ast] \to [t_3^\ast]
   }{
     C \vdashinstrseq \instr^\ast~\instr_N : [t_1^\ast] \to [t_0^\ast~t_3^\ast]
   }

.. _valid-expr:

Expressions
~~~~~~~~~~~

:math:`\instr^\ast~\END`
........................

.. math::
   \frac{
     C \vdashinstrseq \instr^\ast : [] \to [{t'}^\ast]
     \qquad
     \vdash [{t'}^\ast] \leq [t^\ast]
   }{
     C \vdashexpr \instr^\ast~\END : [t^\ast]
   }

.. _valid-constant:

Constant Expressions
....................

.. math::
   \frac{
     (C \vdashinstrconst \instr \const)^\ast
   }{
     C \vdashexprconst \instr^\ast~\END \const
   }

.. math::
   \frac{
   }{
     C \vdashinstrconst t.\CONST~c \const
   }
   \qquad
   \frac{
   }{
     C \vdashinstrconst \REFNULL~t \const
   }
   \qquad
   \frac{
   }{
     C \vdashinstrconst \REFFUNC~x \const
   }

.. math::
   \frac{
     C.\CGLOBALS[x] = \CONST~t
   }{
     C \vdashinstrconst \GLOBALGET~x \const
   }

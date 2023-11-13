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

* The instruction is valid with type :math:`[] \to [t]`.

.. math::
   \frac{
   }{
     C \vdashinstr t\K{.}\CONST~c : [] \to [t]
   }


.. _valid-unop:

:math:`t\K{.}\unop`
...................

* The instruction is valid with type :math:`[t] \to [t]`.

.. math::
   \frac{
   }{
     C \vdashinstr t\K{.}\unop : [t] \to [t]
   }


.. _valid-binop:

:math:`t\K{.}\binop`
....................

* The instruction is valid with type :math:`[t~t] \to [t]`.

.. math::
   \frac{
   }{
     C \vdashinstr t\K{.}\binop : [t~t] \to [t]
   }


.. _valid-testop:

:math:`t\K{.}\testop`
.....................

* The instruction is valid with type :math:`[t] \to [\I32]`.

.. math::
   \frac{
   }{
     C \vdashinstr t\K{.}\testop : [t] \to [\I32]
   }


.. _valid-relop:

:math:`t\K{.}\relop`
....................

* The instruction is valid with type :math:`[t~t] \to [\I32]`.

.. math::
   \frac{
   }{
     C \vdashinstr t\K{.}\relop : [t~t] \to [\I32]
   }


.. _valid-cvtop:

:math:`t_2\K{.}\cvtop\K{\_}t_1\K{\_}\sx^?`
..........................................

* The instruction is valid with type :math:`[t_1] \to [t_2]`.

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

* The instruction is valid with type :math:`[] \to [t]`.

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

* The instruction is valid with type :math:`[t] \to [\I32]`, for any :ref:`reference type <syntax-reftype>` :math:`t`.

.. math::
   \frac{
     t = \reftype
   }{
     C \vdashinstr \REFISNULL : [t] \to [\I32]
   }

.. _valid-ref.func:

:math:`\REFFUNC~x`
..................

* The function :math:`C.\CFUNCS[x]` must be defined in the context.

* The :ref:`function index <syntax-funcidx>` :math:`x` must be contained in :math:`C.\CREFS`.

* The instruction is valid with type :math:`[] \to [\FUNCREF]`.

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

* The instruction is valid with type :math:`[t] \to []`, for any :ref:`operand type <syntax-opdtype>` :math:`t`.

.. math::
   \frac{
   }{
     C \vdashinstr \DROP : [t] \to []
   }

.. _valid-select:

:math:`\SELECT~(t^\ast)^?`
..........................

* If :math:`t^\ast` is present, then:

  * The length of :math:`t^\ast` must be :math:`1`.

  * Then the instruction is valid with type :math:`[t^\ast~t^\ast~\I32] \to [t^\ast]`.

* Else:

  * The instruction is valid with type :math:`[t~t~\I32] \to [t]`, for any :ref:`operand type <syntax-opdtype>` :math:`t` that :ref:`matches <match-opdtype>` some :ref:`number type <syntax-numtype>` or :ref:`vector type <syntax-vectype>`.

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

* The local :math:`C.\CLOCALS[x]` must be defined in the context.

* Let :math:`t` be the :ref:`value type <syntax-valtype>` :math:`C.\CLOCALS[x]`.

* Then the instruction is valid with type :math:`[] \to [t]`.

.. math::
   \frac{
     C.\CLOCALS[x] = t
   }{
     C \vdashinstr \LOCALGET~x : [] \to [t]
   }


.. _valid-local.set:

:math:`\LOCALSET~x`
...................

* The local :math:`C.\CLOCALS[x]` must be defined in the context.

* Let :math:`t` be the :ref:`value type <syntax-valtype>` :math:`C.\CLOCALS[x]`.

* Then the instruction is valid with type :math:`[t] \to []`.

.. math::
   \frac{
     C.\CLOCALS[x] = t
   }{
     C \vdashinstr \LOCALSET~x : [t] \to []
   }


.. _valid-local.tee:

:math:`\LOCALTEE~x`
...................

* The local :math:`C.\CLOCALS[x]` must be defined in the context.

* Let :math:`t` be the :ref:`value type <syntax-valtype>` :math:`C.\CLOCALS[x]`.

* Then the instruction is valid with type :math:`[t] \to [t]`.

.. math::
   \frac{
     C.\CLOCALS[x] = t
   }{
     C \vdashinstr \LOCALTEE~x : [t] \to [t]
   }


.. _valid-global.get:

:math:`\GLOBALGET~x`
....................

* The global :math:`C.\CGLOBALS[x]` must be defined in the context.

* Let :math:`\mut~t` be the :ref:`global type <syntax-globaltype>` :math:`C.\CGLOBALS[x]`.

* Then the instruction is valid with type :math:`[] \to [t]`.

.. math::
   \frac{
     C.\CGLOBALS[x] = \mut~t
   }{
     C \vdashinstr \GLOBALGET~x : [] \to [t]
   }


.. _valid-global.set:

:math:`\GLOBALSET~x`
....................

* The global :math:`C.\CGLOBALS[x]` must be defined in the context.

* Let :math:`\mut~t` be the :ref:`global type <syntax-globaltype>` :math:`C.\CGLOBALS[x]`.

* The mutability :math:`\mut` must be |MVAR|.

* Then the instruction is valid with type :math:`[t] \to []`.

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

* The table :math:`C.\CTABLES[x]` must be defined in the context.

* Let :math:`\limits~t` be the :ref:`table type <syntax-tabletype>` :math:`C.\CTABLES[x]`.

* Then the instruction is valid with type :math:`[\I32] \to [t]`.

.. math::
   \frac{
     C.\CTABLES[x] = \limits~t
   }{
     C \vdashinstr \TABLEGET~x : [\I32] \to [t]
   }


.. _valid-table.set:

:math:`\TABLESET~x`
...................

* The table :math:`C.\CTABLES[x]` must be defined in the context.

* Let :math:`\limits~t` be the :ref:`table type <syntax-tabletype>` :math:`C.\CTABLES[x]`.

* Then the instruction is valid with type :math:`[\I32~t] \to []`.

.. math::
   \frac{
     C.\CTABLES[x] = \limits~t
   }{
     C \vdashinstr \TABLESET~x : [\I32~t] \to []
   }


.. _valid-table.size:

:math:`\TABLESIZE~x`
....................

* The table :math:`C.\CTABLES[x]` must be defined in the context.

* Then the instruction is valid with type :math:`[] \to [\I32]`.

.. math::
   \frac{
     C.\CTABLES[x] = \tabletype
   }{
     C \vdashinstr \TABLESIZE~x : [] \to [\I32]
   }


.. _valid-table.grow:

:math:`\TABLEGROW~x`
....................

* The table :math:`C.\CTABLES[x]` must be defined in the context.

* Let :math:`\limits~t` be the :ref:`table type <syntax-tabletype>` :math:`C.\CTABLES[x]`.

* Then the instruction is valid with type :math:`[t~\I32] \to [\I32]`.

.. math::
   \frac{
     C.\CTABLES[x] = \limits~t
   }{
     C \vdashinstr \TABLEGROW~x : [t~\I32] \to [\I32]
   }


.. _valid-table.fill:

:math:`\TABLEFILL~x`
....................

* The table :math:`C.\CTABLES[x]` must be defined in the context.

* Let :math:`\limits~t` be the :ref:`table type <syntax-tabletype>` :math:`C.\CTABLES[x]`.

* Then the instruction is valid with type :math:`[\I32~t~\I32] \to []`.

.. math::
   \frac{
     C.\CTABLES[x] = \limits~t
   }{
     C \vdashinstr \TABLEFILL~x : [\I32~t~\I32] \to []
   }


.. _valid-table.copy:

:math:`\TABLECOPY~x~y`
......................

* The table :math:`C.\CTABLES[x]` must be defined in the context.

* Let :math:`\limits_1~t_1` be the :ref:`table type <syntax-tabletype>` :math:`C.\CTABLES[x]`.

* The table :math:`C.\CTABLES[y]` must be defined in the context.

* Let :math:`\limits_2~t_2` be the :ref:`table type <syntax-tabletype>` :math:`C.\CTABLES[y]`.

* The :ref:`reference type <syntax-reftype>` :math:`t_1` must be the same as :math:`t_2`.

* Then the instruction is valid with type :math:`[\I32~\I32~\I32] \to []`.

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

* The table :math:`C.\CTABLES[x]` must be defined in the context.

* Let :math:`\limits~t_1` be the :ref:`table type <syntax-tabletype>` :math:`C.\CTABLES[x]`.

* The element segment :math:`C.\CELEMS[y]` must be defined in the context.

* Let :math:`t_2` be the :ref:`reference type <syntax-reftype>` :math:`C.\CELEMS[y]`.

* The :ref:`reference type <syntax-reftype>` :math:`t_1` must be the same as :math:`t_2`.

* Then the instruction is valid with type :math:`[\I32~\I32~\I32] \to []`.

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

* The element segment :math:`C.\CELEMS[x]` must be defined in the context.

* Then the instruction is valid with type :math:`[] \to []`.

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

* The memory :math:`C.\CMEMS[0]` must be defined in the context.

* The alignment :math:`2^{\memarg.\ALIGN}` must not be larger than the :ref:`bit width <syntax-numtype>` of :math:`t` divided by :math:`8`.

* Then the instruction is valid with type :math:`[\I32] \to [t]`.

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

* The memory :math:`C.\CMEMS[0]` must be defined in the context.

* The alignment :math:`2^{\memarg.\ALIGN}` must not be larger than :math:`N/8`.

* Then the instruction is valid with type :math:`[\I32] \to [t]`.

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

* The memory :math:`C.\CMEMS[0]` must be defined in the context.

* The alignment :math:`2^{\memarg.\ALIGN}` must not be larger than the :ref:`bit width <syntax-numtype>` of :math:`t` divided by :math:`8`.

* Then the instruction is valid with type :math:`[\I32~t] \to []`.

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

* The memory :math:`C.\CMEMS[0]` must be defined in the context.

* The alignment :math:`2^{\memarg.\ALIGN}` must not be larger than :math:`N/8`.

* Then the instruction is valid with type :math:`[\I32~t] \to []`.

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

* The memory :math:`C.\CMEMS[0]` must be defined in the context.

* Then the instruction is valid with type :math:`[] \to [\I32]`.

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
   }{
     C \vdashinstr \MEMORYSIZE : [] \to [\I32]
   }


.. _valid-memory.grow:

:math:`\MEMORYGROW`
...................

* The memory :math:`C.\CMEMS[0]` must be defined in the context.

* Then the instruction is valid with type :math:`[\I32] \to [\I32]`.

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
   }{
     C \vdashinstr \MEMORYGROW : [\I32] \to [\I32]
   }


.. _valid-memory.fill:

:math:`\MEMORYFILL`
...................

* The memory :math:`C.\CMEMS[0]` must be defined in the context.

* Then the instruction is valid with type :math:`[\I32~\I32~\I32] \to []`.

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
   }{
     C \vdashinstr \MEMORYFILL : [\I32~\I32~\I32] \to []
   }


.. _valid-memory.copy:

:math:`\MEMORYCOPY`
...................

* The memory :math:`C.\CMEMS[0]` must be defined in the context.

* Then the instruction is valid with type :math:`[\I32~\I32~\I32] \to []`.

.. math::
   \frac{
     C.\CMEMS[0] = \memtype
   }{
     C \vdashinstr \MEMORYCOPY : [\I32~\I32~\I32] \to []
   }


.. _valid-memory.init:

:math:`\MEMORYINIT~x`
.....................

* The memory :math:`C.\CMEMS[0]` must be defined in the context.

* The data segment :math:`C.\CDATAS[x]` must be defined in the context.

* Then the instruction is valid with type :math:`[\I32~\I32~\I32] \to []`.

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

* The data segment :math:`C.\CDATAS[x]` must be defined in the context.

* Then the instruction is valid with type :math:`[] \to []`.

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

* The instruction is valid with type :math:`[] \to []`.

.. math::
   \frac{
   }{
     C \vdashinstr \NOP : [] \to []
   }


.. _valid-unreachable:

:math:`\UNREACHABLE`
....................

* The instruction is valid with type :math:`[t_1^\ast] \to [t_2^\ast]`, for any sequences of :ref:`operand types <syntax-opdtype>` :math:`t_1^\ast` and :math:`t_2^\ast`.

.. math::
   \frac{
   }{
     C \vdashinstr \UNREACHABLE : [t_1^\ast] \to [t_2^\ast]
   }

.. _valid-block:

:math:`\BLOCK~\blocktype~\instr^\ast~\END`
..........................................

* The :ref:`block type <syntax-blocktype>` must be :ref:`valid <valid-blocktype>` as some :ref:`function type <syntax-functype>` :math:`[t_1^\ast] \to [t_2^\ast]`.

* Let :math:`C'` be the same :ref:`context <context>` as :math:`C`, but with the :ref:`result type <syntax-resulttype>` :math:`[t_2^\ast]` prepended to the |CLABELS| vector.

* Under context :math:`C'`,
  the instruction sequence :math:`\instr^\ast` must be :ref:`valid <valid-instr-seq>` with type :math:`[t_1^\ast] \to [t_2^\ast]`.

* Then the compound instruction is valid with type :math:`[t_1^\ast] \to [t_2^\ast]`.

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

* The :ref:`block type <syntax-blocktype>` must be :ref:`valid <valid-blocktype>` as some :ref:`function type <syntax-functype>` :math:`[t_1^\ast] \to [t_2^\ast]`.

* Let :math:`C'` be the same :ref:`context <context>` as :math:`C`, but with the :ref:`result type <syntax-resulttype>` :math:`[t_1^\ast]` prepended to the |CLABELS| vector.

* Under context :math:`C'`,
  the instruction sequence :math:`\instr^\ast` must be :ref:`valid <valid-instr-seq>` with type :math:`[t_1^\ast] \to [t_2^\ast]`.

* Then the compound instruction is valid with type :math:`[t_1^\ast] \to [t_2^\ast]`.

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

* The :ref:`block type <syntax-blocktype>` must be :ref:`valid <valid-blocktype>` as some :ref:`function type <syntax-functype>` :math:`[t_1^\ast] \to [t_2^\ast]`.

* Let :math:`C'` be the same :ref:`context <context>` as :math:`C`, but with the :ref:`result type <syntax-resulttype>` :math:`[t_2^\ast]` prepended to the |CLABELS| vector.

* Under context :math:`C'`,
  the instruction sequence :math:`\instr_1^\ast` must be :ref:`valid <valid-instr-seq>` with type :math:`[t_1^\ast] \to [t_2^\ast]`.

* Under context :math:`C'`,
  the instruction sequence :math:`\instr_2^\ast` must be :ref:`valid <valid-instr-seq>` with type :math:`[t_1^\ast] \to [t_2^\ast]`.

* Then the compound instruction is valid with type :math:`[t_1^\ast~\I32] \to [t_2^\ast]`.

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

* The label :math:`C.\CLABELS[l]` must be defined in the context.

* Let :math:`[t^\ast]` be the :ref:`result type <syntax-resulttype>` :math:`C.\CLABELS[l]`.

* Then the instruction is valid with type :math:`[t_1^\ast~t^\ast] \to [t_2^\ast]`, for any sequences of :ref:`operand types <syntax-opdtype>` :math:`t_1^\ast` and :math:`t_2^\ast`.

.. math::
   \frac{
     C.\CLABELS[l] = [t^\ast]
   }{
     C \vdashinstr \BR~l : [t_1^\ast~t^\ast] \to [t_2^\ast]
   }

.. _valid-br_if:

:math:`\BRIF~l`
...............

* The label :math:`C.\CLABELS[l]` must be defined in the context.

* Let :math:`[t^\ast]` be the :ref:`result type <syntax-resulttype>` :math:`C.\CLABELS[l]`.

* Then the instruction is valid with type :math:`[t^\ast~\I32] \to [t^\ast]`.

.. math::
   \frac{
     C.\CLABELS[l] = [t^\ast]
   }{
     C \vdashinstr \BRIF~l : [t^\ast~\I32] \to [t^\ast]
   }

.. _valid-br_table:

:math:`\BRTABLE~l^\ast~l_N`
...........................


* The label :math:`C.\CLABELS[l_N]` must be defined in the context.

* For all :math:`l_i` in :math:`l^\ast`,
  the label :math:`C.\CLABELS[l_i]` must be defined in the context.

* There must be a sequence :math:`t^\ast` of :ref:`operand types <syntax-opdtype>`, such that:

  * For each :ref:`operand type <syntax-opdtype>` :math:`t_j` in :math:`t^\ast` and corresponding type :math:`t'_{Nj}` in :math:`C.\CLABELS[l_N]`, :math:`t_j` :ref:`matches <match-opdtype>` :math:`t'_{Nj}`.

  * For all :math:`l_i` in :math:`l^\ast`,
    and for each :ref:`operand type <syntax-opdtype>` :math:`t_j` in :math:`t^\ast` and corresponding type :math:`t'_{ij}` in :math:`C.\CLABELS[l_i]`, :math:`t_j` :ref:`matches <match-opdtype>` :math:`t'_{ij}`.

* Then the instruction is valid with type :math:`[t_1^\ast~t^\ast~\I32] \to [t_2^\ast]`, for any sequences of :ref:`operand types <syntax-opdtype>` :math:`t_1^\ast` and :math:`t_2^\ast`.

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

* The return type :math:`C.\CRETURN` must not be absent in the context.

* Let :math:`[t^\ast]` be the :ref:`result type <syntax-resulttype>` of :math:`C.\CRETURN`.

* Then the instruction is valid with type :math:`[t_1^\ast~t^\ast] \to [t_2^\ast]`, for any sequences of :ref:`operand types <syntax-opdtype>` :math:`t_1^\ast` and :math:`t_2^\ast`.

.. math::
   \frac{
     C.\CRETURN = [t^\ast]
   }{
     C \vdashinstr \RETURN : [t_1^\ast~t^\ast] \to [t_2^\ast]
   }

.. _valid-call:

:math:`\CALL~x`
...............

* The function :math:`C.\CFUNCS[x]` must be defined in the context.

* Then the instruction is valid with type :math:`C.\CFUNCS[x]`.

.. math::
   \frac{
     C.\CFUNCS[x] = [t_1^\ast] \to [t_2^\ast]
   }{
     C \vdashinstr \CALL~x : [t_1^\ast] \to [t_2^\ast]
   }

.. _valid-call_indirect:

:math:`\CALLINDIRECT~x~y`
.........................

* The table :math:`C.\CTABLES[x]` must be defined in the context.

* Let :math:`\limits~t` be the :ref:`table type <syntax-tabletype>` :math:`C.\CTABLES[x]`.

* The :ref:`reference type <syntax-reftype>` :math:`t` must be |FUNCREF|.

* The type :math:`C.\CTYPES[y]` must be defined in the context.

* Let :math:`[t_1^\ast] \to [t_2^\ast]` be the :ref:`function type <syntax-functype>` :math:`C.\CTYPES[y]`.

* Then the instruction is valid with type :math:`[t_1^\ast~\I32] \to [t_2^\ast]`.

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

* The empty instruction sequence is valid with type :math:`[t^\ast] \to [t^\ast]`,
  for any sequence of :ref:`operand types <syntax-opdtype>` :math:`t^\ast`.

.. math::
   \frac{
   }{
     C \vdashinstrseq \epsilon : [t^\ast] \to [t^\ast]
   }

Non-empty Instruction Sequence: :math:`\instr^\ast~\instr_N`
............................................................

* The instruction sequence :math:`\instr^\ast` must be valid with type :math:`[t_1^\ast] \to [t_2^\ast]`,
  for some sequences of :ref:`operand types <syntax-opdtype>` :math:`t_1^\ast` and :math:`t_2^\ast`.

* The instruction :math:`\instr_N` must be valid with type :math:`[t^\ast] \to [t_3^\ast]`,
  for some sequences of :ref:`operand types <syntax-opdtype>` :math:`t^\ast` and :math:`t_3^\ast`.

* There must be a sequence of :ref:`operand types <syntax-opdtype>` :math:`t_0^\ast`,
  such that :math:`t_2^\ast = t_0^\ast~{t'}^\ast` where the type sequence :math:`{t'}^\ast` is as long as :math:`t^\ast`.

* For each :ref:`operand type <syntax-opdtype>` :math:`t'_i` in :math:`{t'}^\ast` and corresponding type :math:`t_i` in :math:`t^\ast`, :math:`t'_i` :ref:`matches <match-opdtype>` :math:`t_i`.

* Then the combined instruction sequence is valid with type :math:`[t_1^\ast] \to [t_0^\ast~t_3^\ast]`.

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

* The instruction sequence :math:`\instr^\ast` must be :ref:`valid <valid-instr-seq>` with some :ref:`stack type <syntax-stacktype>` :math:`[] \to [{t'}^\ast]`.

* For each :ref:`operand type <syntax-opdtype>` :math:`t'_i` in :math:`{t'}^\ast` and corresponding :ref:`value type <syntax-valtype>` :math:`t_i` in :math:`t^\ast`, :math:`t'_i` :ref:`matches <match-opdtype>` :math:`t_i`.

* Then the expression is valid with :ref:`result type <syntax-resulttype>` :math:`[t^\ast]`.

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

* In a *constant* expression :math:`\instr^\ast~\END` all instructions in :math:`\instr^\ast` must be constant.

* A constant instruction :math:`\instr` must be:

  * either of the form :math:`t.\CONST~c`,

  * or of the form :math:`\REFNULL`,

  * or of the form :math:`\REFFUNC~x`,

  * or of the form :math:`\GLOBALGET~x`, in which case :math:`C.\CGLOBALS[x]` must be a :ref:`global type <syntax-globaltype>` of the form :math:`\CONST~t`.

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

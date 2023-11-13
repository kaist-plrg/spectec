Modules
-------

.. _valid-local:
.. _valid-func:

Functions
~~~~~~~~~

:math:`\{ \FTYPE~x, \FLOCALS~t^\ast, \FBODY~\expr \}`
.....................................................

.. math::
   \frac{
     C.\CTYPES[x] = [t_1^\ast] \to [t_2^\ast]
     \qquad
     C,\CLOCALS\,t_1^\ast~t^\ast,\CLABELS~[t_2^\ast],\CRETURN~[t_2^\ast] \vdashexpr \expr : [t_2^\ast]
   }{
     C \vdashfunc \{ \FTYPE~x, \FLOCALS~t^\ast, \FBODY~\expr \} : [t_1^\ast] \to [t_2^\ast]
   }

.. _valid-table:

Tables
~~~~~~

:math:`\{ \TTYPE~\tabletype \}`
...............................

.. math::
   \frac{
     \vdashtabletype \tabletype \ok
   }{
     C \vdashtable \{ \TTYPE~\tabletype \} : \tabletype
   }

.. _valid-mem:

Memories
~~~~~~~~

:math:`\{ \MTYPE~\memtype \}`
.............................

.. math::
   \frac{
     \vdashmemtype \memtype \ok
   }{
     C \vdashmem \{ \MTYPE~\memtype \} : \memtype
   }

.. _valid-global:

Globals
~~~~~~~

:math:`\{ \GTYPE~\mut~t, \GINIT~\expr \}`
.........................................

.. math::
   \frac{
     \vdashglobaltype \mut~t \ok
     \qquad
     C \vdashexpr \expr : [t]
     \qquad
     C \vdashexprconst \expr \const
   }{
     C \vdashglobal \{ \GTYPE~\mut~t, \GINIT~\expr \} : \mut~t
   }

.. _valid-elem:

Element Segments
~~~~~~~~~~~~~~~~

:math:`\{ \ETYPE~t, \EINIT~e^\ast, \EMODE~\elemmode \}`
.......................................................

.. math::
   \frac{
     (C \vdashexpr e : [t])^\ast
     \qquad
     (C \vdashexprconst e \const)^\ast
     \qquad
     C \vdashelemmode \elemmode : t
   }{
     C \vdashelem \{ \ETYPE~t, \EINIT~e^\ast, \EMODE~\elemmode \} : t
   }

.. _valid-elemmode:

:math:`\EPASSIVE`
.................

.. math::
   \frac{
   }{
     C \vdashelemmode \EPASSIVE : \reftype
   }

:math:`\EACTIVE~\{ \ETABLE~x, \EOFFSET~\expr \}`
................................................

.. math::
   \frac{
     \begin{array}{@{}c@{}}
     C.\CTABLES[x] = \limits~t
     \\
     C \vdashexpr \expr : [\I32]
     \qquad
     C \vdashexprconst \expr \const
     \end{array}
   }{
     C \vdashelemmode \EACTIVE~\{ \ETABLE~x, \EOFFSET~\expr \} : t
   }

:math:`\EDECLARATIVE`
.....................

.. math::
   \frac{
   }{
     C \vdashelemmode \EDECLARATIVE : \reftype
   }

.. _valid-data:

Data Segments
~~~~~~~~~~~~~

:math:`\{ \DINIT~b^\ast, \DMODE~\datamode \}`
....................................................

.. math::
   \frac{
     C \vdashdatamode \datamode \ok
   }{
     C \vdashdata \{ \DINIT~b^\ast, \DMODE~\datamode \} \ok
   }

.. _valid-datamode:

:math:`\DPASSIVE`
.................

.. math::
   \frac{
   }{
     C \vdashdatamode \DPASSIVE \ok
   }

:math:`\DACTIVE~\{ \DMEM~x, \DOFFSET~\expr \}`
..............................................

.. math::
   \frac{
     C.\CMEMS[x] = \limits
     \qquad
     C \vdashexpr \expr : [\I32]
     \qquad
     C \vdashexprconst \expr \const
   }{
     C \vdashdatamode \DACTIVE~\{ \DMEM~x, \DOFFSET~\expr \} \ok
   }

.. _valid-start:

Start Function
~~~~~~~~~~~~~~

:math:`\{ \SFUNC~x \}`
......................

.. math::
   \frac{
     C.\CFUNCS[x] = [] \to []
   }{
     C \vdashstart \{ \SFUNC~x \} \ok
   }

.. _valid-exportdesc:
.. _valid-export:

Exports
~~~~~~~

:math:`\{ \ENAME~\name, \EDESC~\exportdesc \}`
..............................................

.. math::
   \frac{
     C \vdashexportdesc \exportdesc : \externtype
   }{
     C \vdashexport \{ \ENAME~\name, \EDESC~\exportdesc \} : \externtype
   }

:math:`\EDFUNC~x`
.................

.. math::
   \frac{
     C.\CFUNCS[x] = \functype
   }{
     C \vdashexportdesc \EDFUNC~x : \ETFUNC~\functype
   }

:math:`\EDTABLE~x`
..................

.. math::
   \frac{
     C.\CTABLES[x] = \tabletype
   }{
     C \vdashexportdesc \EDTABLE~x : \ETTABLE~\tabletype
   }

:math:`\EDMEM~x`
................

.. math::
   \frac{
     C.\CMEMS[x] = \memtype
   }{
     C \vdashexportdesc \EDMEM~x : \ETMEM~\memtype
   }

:math:`\EDGLOBAL~x`
...................

.. math::
   \frac{
     C.\CGLOBALS[x] = \globaltype
   }{
     C \vdashexportdesc \EDGLOBAL~x : \ETGLOBAL~\globaltype
   }

.. _valid-importdesc:
.. _valid-import:

Imports
~~~~~~~

:math:`\{ \IMODULE~\name_1, \INAME~\name_2, \IDESC~\importdesc \}`
..................................................................

.. math::
   \frac{
     C \vdashimportdesc \importdesc : \externtype
   }{
     C \vdashimport \{ \IMODULE~\name_1, \INAME~\name_2, \IDESC~\importdesc \} : \externtype
   }

:math:`\IDFUNC~x`
.................

.. math::
   \frac{
     C.\CTYPES[x] = [t_1^\ast] \to [t_2^\ast]
   }{
     C \vdashimportdesc \IDFUNC~x : \ETFUNC~[t_1^\ast] \to [t_2^\ast]
   }

:math:`\IDTABLE~\tabletype`
...........................

.. math::
   \frac{
     \vdashtable \tabletype \ok
   }{
     C \vdashimportdesc \IDTABLE~\tabletype : \ETTABLE~\tabletype
   }

:math:`\IDMEM~\memtype`
.......................

.. math::
   \frac{
     \vdashmemtype \memtype \ok
   }{
     C \vdashimportdesc \IDMEM~\memtype : \ETMEM~\memtype
   }

:math:`\IDGLOBAL~\globaltype`
.............................

.. math::
   \frac{
     \vdashglobaltype \globaltype \ok
   }{
     C \vdashimportdesc \IDGLOBAL~\globaltype : \ETGLOBAL~\globaltype
   }

.. _valid-module:

Modules
~~~~~~~

.. math::
   \frac{
     \begin{array}{@{}c@{}}
     (\vdashfunctype \type \ok)^\ast
     \quad
     (C \vdashfunc \func : \X{ft})^\ast
     \quad
     (C' \vdashtable \table : \X{tt})^\ast
     \quad
     (C' \vdashmem \mem : \X{mt})^\ast
     \quad
     (C' \vdashglobal \global : \X{gt})^\ast
     \\
     (C' \vdashelem \elem : \X{rt})^\ast
     \quad
     (C' \vdashdata \data \ok)^n
     \quad
     (C \vdashstart \start \ok)^?
     \quad
     (C \vdashimport \import : \X{it})^\ast
     \quad
     (C \vdashexport \export : \X{et})^\ast
     \\
     \X{ift}^\ast = \etfuncs(\X{it}^\ast)
     \qquad
     \X{itt}^\ast = \ettables(\X{it}^\ast)
     \qquad
     \X{imt}^\ast = \etmems(\X{it}^\ast)
     \qquad
     \X{igt}^\ast = \etglobals(\X{it}^\ast)
     \\
     x^\ast = \freefuncidx(\module \with \MFUNCS = \epsilon \with \MSTART = \epsilon)
     \\
     C = \{ \CTYPES~\type^\ast, \CFUNCS~\X{ift}^\ast\,\X{ft}^\ast, \CTABLES~\X{itt}^\ast\,\X{tt}^\ast, \CMEMS~\X{imt}^\ast\,\X{mt}^\ast, \CGLOBALS~\X{igt}^\ast\,\X{gt}^\ast, \CELEMS~\X{rt}^\ast, \CDATAS~{\ok}^n, \CREFS~x^\ast \}
     \\
     C' = \{ \CGLOBALS~\X{igt}^\ast, \CFUNCS~(C.\CFUNCS), \CREFS~(C.\CREFS) \}
     \qquad
     |C.\CMEMS| \leq 1
     \qquad
     (\export.\ENAME)^\ast ~\F{disjoint}
     \\
     \module = \{
       \begin{array}[t]{@{}l@{}}
         \MTYPES~\type^\ast,
         \MFUNCS~\func^\ast,
         \MTABLES~\table^\ast,
         \MMEMS~\mem^\ast,
         \MGLOBALS~\global^\ast, \\
         \MELEMS~\elem^\ast,
         \MDATAS~\data^n,
         \MSTART~\start^?,
         \MIMPORTS~\import^\ast,
         \MEXPORTS~\export^\ast \}
       \end{array}
     \end{array}
   }{
     \vdashmodule \module : \X{it}^\ast \to \X{et}^\ast
   }

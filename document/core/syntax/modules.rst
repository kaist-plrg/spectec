.. _syntax-module:

Modules
-------

.. math::
   \begin{array}{lllll}
   \production{module} & \module &::=& \{ &
     \MTYPES~\vec(\functype), \\&&&&
     \MFUNCS~\vec(\func), \\&&&&
     \MTABLES~\vec(\table), \\&&&&
     \MMEMS~\vec(\mem), \\&&&&
     \MGLOBALS~\vec(\global), \\&&&&
     \MELEMS~\vec(\elem), \\&&&&
     \MDATAS~\vec(\data), \\&&&&
     \MSTART~\start^?, \\&&&&
     \MIMPORTS~\vec(\import), \\&&&&
     \MEXPORTS~\vec(\export) \quad\} \\
   \end{array}

.. _syntax-typeidx:
.. _syntax-funcidx:
.. _syntax-tableidx:
.. _syntax-memidx:
.. _syntax-globalidx:
.. _syntax-elemidx:
.. _syntax-dataidx:
.. _syntax-localidx:
.. _syntax-labelidx:
.. _syntax-index:

Indices
~~~~~~~

.. math::
   \begin{array}{llll}
   \production{type index} & \typeidx &::=& \u32 \\
   \production{function index} & \funcidx &::=& \u32 \\
   \production{table index} & \tableidx &::=& \u32 \\
   \production{memory index} & \memidx &::=& \u32 \\
   \production{global index} & \globalidx &::=& \u32 \\
   \production{element index} & \elemidx &::=& \u32 \\
   \production{data index} & \dataidx &::=& \u32 \\
   \production{local index} & \localidx &::=& \u32 \\
   \production{label index} & \labelidx &::=& \u32 \\
   \end{array}

.. _syntax-local:
.. _syntax-func:

Functions
~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{function} & \func &::=&
     \{ \FTYPE~\typeidx, \FLOCALS~\vec(\valtype), \FBODY~\expr \} \\
   \end{array}

.. _syntax-table:

Tables
~~~~~~

.. math::
   \begin{array}{llll}
   \production{table} & \table &::=&
     \{ \TTYPE~\tabletype \} \\
   \end{array}

.. _syntax-mem:

Memories
~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{memory} & \mem &::=&
     \{ \MTYPE~\memtype \} \\
   \end{array}

.. _syntax-global:

Globals
~~~~~~~

.. math::
   \begin{array}{llll}
   \production{global} & \global &::=&
     \{ \GTYPE~\globaltype, \GINIT~\expr \} \\
   \end{array}

.. _syntax-elem:
.. _syntax-elemmode:

Element Segments
~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{element segment} & \elem &::=&
     \{ \ETYPE~\reftype, \EINIT~\vec(\expr), \EMODE~\elemmode \} \\
   \production{element segment mode} & \elemmode &::=&
     \EPASSIVE \\&&|&
     \EACTIVE~\{ \ETABLE~\tableidx, \EOFFSET~\expr \} \\&&|&
     \EDECLARATIVE \\
   \end{array}

.. _syntax-data:
.. _syntax-datamode:

Data Segments
~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{data segment} & \data &::=&
     \{ \DINIT~\vec(\byte), \DMODE~\datamode \} \\
   \production{data segment mode} & \datamode &::=&
     \DPASSIVE \\&&|&
     \DACTIVE~\{ \DMEM~\memidx, \DOFFSET~\expr \} \\
   \end{array}

.. _syntax-start:

Start Function
~~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{start function} & \start &::=&
     \{ \SFUNC~\funcidx \} \\
   \end{array}

.. _syntax-exportdesc:
.. _syntax-export:

Exports
~~~~~~~

.. math::
   \begin{array}{llcl}
   \production{export} & \export &::=&
     \{ \ENAME~\name, \EDESC~\exportdesc \} \\
   \production{export description} & \exportdesc &::=&
     \EDFUNC~\funcidx \\&&|&
     \EDTABLE~\tableidx \\&&|&
     \EDMEM~\memidx \\&&|&
     \EDGLOBAL~\globalidx \\
   \end{array}

* :math:`\edfuncs(\export^\ast) = [\funcidx ~|~ \EDFUNC~\funcidx \in (\export.\EDESC)^\ast]`

* :math:`\edtables(\export^\ast) = [\tableidx ~|~ \EDTABLE~\tableidx \in (\export.\EDESC)^\ast]`

* :math:`\edmems(\export^\ast) = [\memidx ~|~ \EDMEM~\memidx \in (\export.\EDESC)^\ast]`

* :math:`\edglobals(\export^\ast) = [\globalidx ~|~ \EDGLOBAL~\globalidx \in (\export.\EDESC)^\ast]`

.. _syntax-importdesc:
.. _syntax-import:

Imports
~~~~~~~

.. math::
   \begin{array}{llll}
   \production{import} & \import &::=&
     \{ \IMODULE~\name, \INAME~\name, \IDESC~\importdesc \} \\
   \production{import description} & \importdesc &::=&
     \IDFUNC~\typeidx \\&&|&
     \IDTABLE~\tabletype \\&&|&
     \IDMEM~\memtype \\&&|&
     \IDGLOBAL~\globaltype \\
   \end{array}

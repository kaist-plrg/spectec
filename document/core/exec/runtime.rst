.. _syntax-runtime:

Runtime Structure
-----------------

.. _syntax-num:
.. _syntax-vecc:
.. _syntax-ref:
.. _syntax-ref.extern:
.. _syntax-val:

Values
~~~~~~

.. math::
   \begin{array}{llcl}
   \production{number} & \num &::=&
     \I32.\CONST~\i32 \\&&|&
     \I64.\CONST~\i64 \\&&|&
     \F32.\CONST~\f32 \\&&|&
     \F64.\CONST~\f64 \\
   \production{vector} & \vecc &::=&
     \V128.\CONST~\i128 \\
   \production{reference} & \reff &::=&
     \REFNULL~t \\&&|&
     \REFFUNCADDR~\funcaddr \\&&|&
     \REFEXTERNADDR~\externaddr \\
   \production{value} & \val &::=&
     \num ~|~ \vecc ~|~ \reff \\
   \end{array}

.. _default-val:

.. math::
   \begin{array}{lcl@{\qquad}l}
   \default_t &=& t{.}\CONST~0 & (\iff t = \numtype) \\
   \default_t &=& t{.}\CONST~0 & (\iff t = \vectype) \\
   \default_t &=& \REFNULL~t & (\iff t = \reftype) \\
   \end{array}

.. _syntax-result:

Results
~~~~~~~

.. math::
   \begin{array}{llcl}
   \production{result} & \result &::=&
     \val^\ast \\&&|&
     \TRAP
   \end{array}

.. _syntax-store:
.. _store:

Store
~~~~~

.. math::
   \begin{array}{llll}
   \production{store} & \store &::=& \{~
     \begin{array}[t]{l@{~}ll}
     \SFUNCS & \funcinst^\ast, \\
     \STABLES & \tableinst^\ast, \\
     \SMEMS & \meminst^\ast, \\
     \SGLOBALS & \globalinst^\ast, \\
     \SELEMS & \eleminst^\ast, \\
     \SDATAS & \datainst^\ast ~\} \\
     \end{array}
   \end{array}

.. _syntax-funcaddr:
.. _syntax-tableaddr:
.. _syntax-memaddr:
.. _syntax-globaladdr:
.. _syntax-elemaddr:
.. _syntax-dataaddr:
.. _syntax-externaddr:
.. _syntax-addr:

Addresses
~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{address} & \addr &::=&
     0 ~|~ 1 ~|~ 2 ~|~ \dots \\
   \production{function address} & \funcaddr &::=&
     \addr \\
   \production{table address} & \tableaddr &::=&
     \addr \\
   \production{memory address} & \memaddr &::=&
     \addr \\
   \production{global address} & \globaladdr &::=&
     \addr \\
   \production{element address} & \elemaddr &::=&
     \addr \\
   \production{data address} & \dataaddr &::=&
     \addr \\
   \production{extern address} & \externaddr &::=&
     \addr \\
   \end{array}

.. _syntax-moduleinst:

Module Instances
~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{module instance} & \moduleinst &::=& \{
     \begin{array}[t]{l@{~}ll}
     \MITYPES & \functype^\ast, \\
     \MIFUNCS & \funcaddr^\ast, \\
     \MITABLES & \tableaddr^\ast, \\
     \MIMEMS & \memaddr^\ast, \\
     \MIGLOBALS & \globaladdr^\ast, \\
     \MIELEMS & \elemaddr^\ast, \\
     \MIDATAS & \dataaddr^\ast, \\
     \MIEXPORTS & \exportinst^\ast ~\} \\
     \end{array}
   \end{array}

.. _syntax-hostfunc:
.. _syntax-funcinst:

Function Instances
~~~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{function instance} & \funcinst &::=&
     \{ \FITYPE~\functype, \FIMODULE~\moduleinst, \FICODE~\func \} \\ &&|&
     \{ \FITYPE~\functype, \FIHOSTCODE~\hostfunc \} \\
   \production{host function} & \hostfunc &::=& \dots \\
   \end{array}
.. _syntax-tableinst:

Table Instances
~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{table instance} & \tableinst &::=&
     \{ \TITYPE~\tabletype, \TIELEM~\vec(\reff) \} \\
   \end{array}

.. _page-size:
.. _syntax-meminst:

Memory Instances
~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{memory instance} & \meminst &::=&
     \{ \MITYPE~\memtype, \MIDATA~\vec(\byte) \} \\
   \end{array}

.. _syntax-globalinst:

Global Instances
~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{global instance} & \globalinst &::=&
     \{ \GITYPE~\globaltype, \GIVALUE~\val \} \\
   \end{array}

.. _syntax-eleminst:

Element Instances
~~~~~~~~~~~~~~~~~

.. math::
  \begin{array}{llll}
  \production{element instance} & \eleminst &::=&
    \{ \EITYPE~\reftype, \EIELEM~\vec(\reff) \} \\
  \end{array}

.. _syntax-datainst:

Data Instances
~~~~~~~~~~~~~~

.. math::
  \begin{array}{llll}
  \production{data instance} & \datainst &::=&
    \{ \DIDATA~\vec(\byte) \} \\
  \end{array}

.. _syntax-exportinst:

Export Instances
~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{export instance} & \exportinst &::=&
     \{ \EINAME~\name, \EIVALUE~\externval \} \\
   \end{array}

.. _syntax-externval:

External Values
~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llcl}
   \production{external value} & \externval &::=&
     \EVFUNC~\funcaddr \\&&|&
     \EVTABLE~\tableaddr \\&&|&
     \EVMEM~\memaddr \\&&|&
     \EVGLOBAL~\globaladdr \\
   \end{array}


Conventions
...........

* :math:`\evfuncs(\externval^\ast) = [\funcaddr ~|~ (\EVFUNC~\funcaddr) \in \externval^\ast]`

* :math:`\evtables(\externval^\ast) = [\tableaddr ~|~ (\EVTABLE~\tableaddr) \in \externval^\ast]`

* :math:`\evmems(\externval^\ast) = [\memaddr ~|~ (\EVMEM~\memaddr) \in \externval^\ast]`

* :math:`\evglobals(\externval^\ast) = [\globaladdr ~|~ (\EVGLOBAL~\globaladdr) \in \externval^\ast]`

.. _syntax-frame:
.. _syntax-label:
.. _frame:
.. _label:
.. _stack:

Stack
~~~~~

Values
......

Labels
......

.. math::
   \begin{array}{llll}
   \production{label} & \label &::=&
     \LABEL_n\{\instr^\ast\} \\
   \end{array}

Activation Frames
.................

.. math::
   \begin{array}{llll}
   \production{frame} & \frame &::=&
     \{ \ALOCALS~\val^\ast, \AMODULE~\moduleinst \} \\
   \end{array}

.. _exec-expand:

Conventions
...........

.. math::
   \begin{array}{lll}
   \expand_F(\typeidx) &=& F.\AMODULE.\MITYPES[\typeidx] \\
   \expand_F([\valtype^?]) &=& [] \to [\valtype^?] \\
   \end{array}

.. _syntax-trap:
.. _syntax-reffuncaddr:
.. _syntax-invoke:
.. _syntax-instr-admin:

Administrative Instructions
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llcl}
   \production{administrative instruction} & \instr &::=&
     \dots \\ &&|&
     \TRAP \\ &&|&
     \REFFUNCADDR~\funcaddr \\ &&|&
     \REFEXTERNADDR~\externaddr \\ &&|&
     \INVOKE~\funcaddr \\ &&|&
     \LABEL_n\{\instr^\ast\}~\instr^\ast~\END \\ &&|&
     \FRAME_n\{\frame\}~\instr^\ast~\END \\
   \end{array}

.. _syntax-thread:
.. _syntax-config:

Configurations
..............

.. math::
   \begin{array}{llcl}
   \production{configuration} & \config &::=&
     \store; \thread \\
   \production{thread} & \thread &::=&
     \frame; \instr^\ast \\
   \end{array}

.. _syntax-ctxt-eval:

Evaluation Contexts
...................

.. math::
   \begin{array}{llll}
   \production{evaluation contexts} & E &::=&
     [\_] ~|~
     \val^\ast~E~\instr^\ast ~|~
     \LABEL_n\{\instr^\ast\}~E~\END \\
   \end{array}

.. math::
   \begin{array}{rcl}
   S; F; E[\instr^\ast] &\stepto& S'; F'; E[{\instr'}^\ast] \\
     && (\iff S; F; \instr^\ast \stepto S'; F'; {\instr'}^\ast) \\
   S; F; \FRAME_n\{F'\}~\instr^\ast~\END &\stepto& S'; F; \FRAME_n\{F''\}~\instr'^\ast~\END \\
     && (\iff S; F'; \instr^\ast \stepto S'; F''; {\instr'}^\ast) \\[1ex]
   S; F; E[\TRAP] &\stepto& S; F; \TRAP
     \qquad (\iff E \neq [\_]) \\
   S; F; \FRAME_n\{F'\}~\TRAP~\END &\stepto& S; F; \TRAP \\
   \end{array}

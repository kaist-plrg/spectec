.. _syntax-type:

Types
-----

.. _syntax-numtype:

Number Types
~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{number type} & \numtype &::=&
     \I32 ~|~ \I64 ~|~ \F32 ~|~ \F64 \\
   \end{array}

.. _syntax-reftype:

Reference Types
~~~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{reference type} & \reftype &::=&
     \FUNCREF ~|~ \EXTERNREF \\
   \end{array}

.. _syntax-valtype:

Value Types
~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{value type} & \valtype &::=&
     \numtype ~|~ \vectype ~|~ \reftype \\
   \end{array}

.. _syntax-resulttype:

Result Types
~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{result type} & \resulttype &::=&
     [\vec(\valtype)] \\
   \end{array}
.. _syntax-functype:

Function Types
~~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{function type} & \functype &::=&
     \resulttype \to \resulttype \\
   \end{array}

.. _syntax-limits:

Limits
~~~~~~

.. math::
   \begin{array}{llll}
   \production{limits} & \limits &::=&
     \{ \LMIN~\u32, \LMAX~\u32^? \} \\
   \end{array}

.. _syntax-memtype:

Memory Types
~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{memory type} & \memtype &::=&
     \limits \\
   \end{array}

.. _syntax-tabletype:

Table Types
~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{table type} & \tabletype &::=&
     \limits~\reftype \\
   \end{array}

.. _syntax-mut:
.. _syntax-globaltype:

Global Types
~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{global type} & \globaltype &::=&
     \mut~\valtype \\
   \production{mutability} & \mut &::=&
     \MCONST ~|~
     \MVAR \\
   \end{array}

.. _syntax-externtype:

External Types
~~~~~~~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{external types} & \externtype &::=&
     \ETFUNC~\functype ~|~
     \ETTABLE~\tabletype ~|~
     \ETMEM~\memtype ~|~
     \ETGLOBAL~\globaltype \\
   \end{array}

* :math:`\etfuncs(\externtype^\ast) = [\functype ~|~ (\ETFUNC~\functype) \in \externtype^\ast]`

* :math:`\ettables(\externtype^\ast) = [\tabletype ~|~ (\ETTABLE~\tabletype) \in \externtype^\ast]`

* :math:`\etmems(\externtype^\ast) = [\memtype ~|~ (\ETMEM~\memtype) \in \externtype^\ast]`

* :math:`\etglobals(\externtype^\ast) = [\globaltype ~|~ (\ETGLOBAL~\globaltype) \in \externtype^\ast]`

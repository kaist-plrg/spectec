.. _type-system:

Conventions
-----------

.. _context:

Contexts
~~~~~~~~

.. math::
   \begin{array}{llll}
   \production{context} & C &::=&
     \begin{array}[t]{l@{~}ll}
     \{ & \CTYPES & \functype^\ast, \\
        & \CFUNCS & \functype^\ast, \\
        & \CTABLES & \tabletype^\ast, \\
        & \CMEMS & \memtype^\ast, \\
        & \CGLOBALS & \globaltype^\ast, \\
        & \CELEMS & \reftype^\ast, \\
        & \CDATAS & {\ok}^\ast, \\
        & \CLOCALS & \valtype^\ast, \\
        & \CLABELS & \resulttype^\ast, \\
        & \CRETURN & \resulttype^?, \\
        & \CREFS & \funcidx^\ast ~\} \\
     \end{array}
   \end{array}

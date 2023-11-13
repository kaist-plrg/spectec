Types
-----

.. _valid-limits:

Limits
~~~~~~

:math:`\{ \LMIN~n, \LMAX~m^? \}`
................................

.. math::
   \frac{
     n \leq k
     \qquad
     (m \leq k)^?
     \qquad
     (n \leq m)^?
   }{
     \vdashlimits \{ \LMIN~n, \LMAX~m^? \} : k
   }


.. _valid-blocktype:

Block Types
~~~~~~~~~~~

:math:`\typeidx`
................

.. math::
   \frac{
     C.\CTYPES[\typeidx] = \functype
   }{
     C \vdashblocktype \typeidx : \functype
   }


:math:`[\valtype^?]`
....................

.. math::
   \frac{
   }{
     C \vdashblocktype [\valtype^?] : [] \to [\valtype^?]
   }

.. _valid-functype:

Function Types
~~~~~~~~~~~~~~

:math:`[t_1^n] \to [t_2^m]`
...........................

.. math::
   \frac{
   }{
     \vdashfunctype [t_1^\ast] \to [t_2^\ast] \ok
   }

.. _valid-tabletype:

Table Types
~~~~~~~~~~~

:math:`\limits~\reftype`
........................

.. math::
   \frac{
     \vdashlimits \limits : 2^{32} - 1
   }{
     \vdashtabletype \limits~\reftype \ok
   }

.. _valid-memtype:

Memory Types
~~~~~~~~~~~~

:math:`\limits`
...............

.. math::
   \frac{
     \vdashlimits \limits : 2^{16}
   }{
     \vdashmemtype \limits \ok
   }

.. _valid-globaltype:

Global Types
~~~~~~~~~~~~

:math:`\mut~\valtype`
.....................

.. math::
   \frac{
   }{
     \vdashglobaltype \mut~\valtype \ok
   }

.. _valid-externtype:

External Types
~~~~~~~~~~~~~~

:math:`\ETFUNC~\functype`
.........................

.. math::
   \frac{
     \vdashfunctype \functype \ok
   }{
     \vdashexterntype \ETFUNC~\functype \ok
   }

:math:`\ETTABLE~\tabletype`
...........................

.. math::
   \frac{
     \vdashtabletype \tabletype \ok
   }{
     \vdashexterntype \ETTABLE~\tabletype \ok
   }

:math:`\ETMEM~\memtype`
.......................

.. math::
   \frac{
     \vdashmemtype \memtype \ok
   }{
     \vdashexterntype \ETMEM~\memtype \ok
   }

:math:`\ETGLOBAL~\globaltype`
.............................

.. math::
   \frac{
     \vdashglobaltype \globaltype \ok
   }{
     \vdashexterntype \ETGLOBAL~\globaltype \ok
   }

.. _exec-import:
.. _match:

Import Subtyping
~~~~~~~~~~~~~~~~

.. _match-limits:

Limits
......

.. math::
   ~\\[-1ex]
   \frac{
     n_1 \geq n_2
   }{
     \vdashlimitsmatch \{ \LMIN~n_1, \LMAX~m_1^? \} \matcheslimits \{ \LMIN~n_2, \LMAX~\epsilon \}
   }
   \quad
   \frac{
     n_1 \geq n_2
     \qquad
     m_1 \leq m_2
   }{
     \vdashlimitsmatch \{ \LMIN~n_1, \LMAX~m_1 \} \matcheslimits \{ \LMIN~n_2, \LMAX~m_2 \}
   }

.. _match-externtype:

.. _match-functype:

Functions
.........

.. math::
   ~\\[-1ex]
   \frac{
   }{
     \vdashexterntypematch \ETFUNC~\functype \matchesexterntype \ETFUNC~\functype
   }

.. _match-tabletype:

Tables
......

.. math::
   \frac{
     \vdashlimitsmatch \limits_1 \matcheslimits \limits_2
   }{
     \vdashexterntypematch \ETTABLE~(\limits_1~\reftype) \matchesexterntype \ETTABLE~(\limits_2~\reftype)
   }

.. _match-memtype:

Memories
........

.. math::
   \frac{
     \vdashlimitsmatch \limits_1 \matcheslimits \limits_2
   }{
     \vdashexterntypematch \ETMEM~\limits_1 \matchesexterntype \ETMEM~\limits_2
   }

.. _match-globaltype:

Globals
.......

.. math::
   ~\\[-1ex]
   \frac{
   }{
     \vdashexterntypematch \ETGLOBAL~\globaltype \matchesexterntype \ETGLOBAL~\globaltype
   }

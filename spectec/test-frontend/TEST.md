# Preview

```sh
$ (cd ../spec && dune exec ../src/exe-watsup/main.exe -- *.watsup -v -l)
watsup 0.3 generator
== Parsing...
== Multiplicity checking...
== Elaboration...
== Printing...

;; 1-syntax.watsup:3.1-3.15
syntax n = nat

;; 1-syntax.watsup:9.1-9.37
syntax name = text

;; 1-syntax.watsup:14.1-14.36
syntax byte = nat

;; 1-syntax.watsup:15.1-15.45
syntax u32 = nat

;; 1-syntax.watsup:22.1-22.36
syntax idx = nat

;; 1-syntax.watsup:23.1-23.49
syntax funcidx = idx

;; 1-syntax.watsup:24.1-24.49
syntax globalidx = idx

;; 1-syntax.watsup:25.1-25.47
syntax tableidx = idx

;; 1-syntax.watsup:26.1-26.46
syntax memidx = idx

;; 1-syntax.watsup:27.1-27.45
syntax elemidx = idx

;; 1-syntax.watsup:28.1-28.45
syntax dataidx = idx

;; 1-syntax.watsup:29.1-29.47
syntax labelidx = idx

;; 1-syntax.watsup:30.1-30.47
syntax localidx = idx

;; 1-syntax.watsup:39.1-40.22
syntax numtype =
  | I32
  | I64
  | F32
  | F64

;; 1-syntax.watsup:41.1-42.5
syntax vectype =
  | V128

;; 1-syntax.watsup:43.1-44.20
syntax reftype =
  | FUNCREF
  | EXTERNREF

;; 1-syntax.watsup:45.1-46.34
syntax valtype =
  | numtype
  | vectype
  | reftype
  | BOT

;; 1-syntax.watsup:48.1-48.39
syntax in =
  | I32
  | I64

;; 1-syntax.watsup:49.1-49.39
syntax fn =
  | F32
  | F64

;; 1-syntax.watsup:56.1-57.11
syntax resulttype = valtype*

;; 1-syntax.watsup:59.1-60.16
syntax limits = `[%..%]`(u32, u32)

;; 1-syntax.watsup:61.1-62.15
syntax globaltype = `MUT%?%`(()?, valtype)

;; 1-syntax.watsup:63.1-64.27
syntax functype = `%->%`(resulttype, resulttype)

;; 1-syntax.watsup:65.1-66.17
syntax tabletype = `%%`(limits, reftype)

;; 1-syntax.watsup:67.1-68.12
syntax memtype = `%I8`(limits)

;; 1-syntax.watsup:69.1-70.10
syntax elemtype = reftype

;; 1-syntax.watsup:71.1-72.5
syntax datatype = OK

;; 1-syntax.watsup:73.1-74.69
syntax externtype =
  | GLOBAL(globaltype)
  | FUNC(functype)
  | TABLE(tabletype)
  | MEMORY(memtype)

;; 1-syntax.watsup:86.1-86.44
syntax sx =
  | U
  | S

;; 1-syntax.watsup:88.1-88.39
syntax unop_IXX =
  | CLZ
  | CTZ
  | POPCNT

;; 1-syntax.watsup:89.1-89.70
syntax unop_FXX =
  | ABS
  | NEG
  | SQRT
  | CEIL
  | FLOOR
  | TRUNC
  | NEAREST

;; 1-syntax.watsup:91.1-93.62
syntax binop_IXX =
  | ADD
  | SUB
  | MUL
  | DIV(sx)
  | REM(sx)
  | AND
  | OR
  | XOR
  | SHL
  | SHR(sx)
  | ROTL
  | ROTR

;; 1-syntax.watsup:94.1-94.66
syntax binop_FXX =
  | ADD
  | SUB
  | MUL
  | DIV
  | MIN
  | MAX
  | COPYSIGN

;; 1-syntax.watsup:96.1-96.26
syntax testop_IXX =
  | EQZ

;; 1-syntax.watsup:97.1-97.22
syntax testop_FXX =
  |

;; 1-syntax.watsup:99.1-100.108
syntax relop_IXX =
  | EQ
  | NE
  | LT(sx)
  | GT(sx)
  | LE(sx)
  | GE(sx)

;; 1-syntax.watsup:101.1-101.49
syntax relop_FXX =
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE

;; 1-syntax.watsup:103.1-103.50
syntax unop_numtype =
  | _I(unop_IXX)
  | _F(unop_FXX)

;; 1-syntax.watsup:104.1-104.53
syntax binop_numtype =
  | _I(binop_IXX)
  | _F(binop_FXX)

;; 1-syntax.watsup:105.1-105.56
syntax testop_numtype =
  | _I(testop_IXX)
  | _F(testop_FXX)

;; 1-syntax.watsup:106.1-106.53
syntax relop_numtype =
  | _I(relop_IXX)
  | _F(relop_FXX)

;; 1-syntax.watsup:107.1-107.39
syntax cvtop =
  | CONVERT
  | REINTERPRET

;; 1-syntax.watsup:117.1-117.23
syntax c_numtype = nat

;; 1-syntax.watsup:118.1-118.23
syntax c_vectype = nat

;; 1-syntax.watsup:121.1-121.52
syntax blocktype = functype

;; 1-syntax.watsup:156.1-177.55
rec {

;; 1-syntax.watsup:156.1-177.55
syntax instr =
  | UNREACHABLE
  | NOP
  | DROP
  | SELECT(valtype?)
  | BLOCK(blocktype, instr*)
  | LOOP(blocktype, instr*)
  | IF(blocktype, instr*, instr*)
  | BR(labelidx)
  | BR_IF(labelidx)
  | BR_TABLE(labelidx*, labelidx)
  | CALL(funcidx)
  | CALL_INDIRECT(tableidx, functype)
  | RETURN
  | CONST(numtype, c_numtype)
  | UNOP(numtype, unop_numtype)
  | BINOP(numtype, binop_numtype)
  | TESTOP(numtype, testop_numtype)
  | RELOP(numtype, relop_numtype)
  | EXTEND(numtype, n)
  | CVTOP(numtype, cvtop, numtype, sx?)
  | REF.NULL(reftype)
  | REF.FUNC(funcidx)
  | REF.IS_NULL
  | LOCAL.GET(localidx)
  | LOCAL.SET(localidx)
  | LOCAL.TEE(localidx)
  | GLOBAL.GET(globalidx)
  | GLOBAL.SET(globalidx)
  | TABLE.GET(tableidx)
  | TABLE.SET(tableidx)
  | TABLE.SIZE(tableidx)
  | TABLE.GROW(tableidx)
  | TABLE.FILL(tableidx)
  | TABLE.COPY(tableidx, tableidx)
  | TABLE.INIT(tableidx, elemidx)
  | ELEM.DROP(elemidx)
  | MEMORY.SIZE
  | MEMORY.GROW
  | MEMORY.FILL
  | MEMORY.COPY
  | MEMORY.INIT(dataidx)
  | DATA.DROP(dataidx)
  | LOAD(numtype, (n, sx)?, nat, nat)
  | STORE(numtype, n?, nat, nat)
}

;; 1-syntax.watsup:179.1-180.9
syntax expr = instr*

;; 1-syntax.watsup:185.1-185.50
syntax elemmode =
  | TABLE(tableidx, expr)
  | DECLARE

;; 1-syntax.watsup:186.1-186.39
syntax datamode =
  | MEMORY(memidx, expr)

;; 1-syntax.watsup:188.1-189.30
syntax func = FUNC(functype, valtype*, expr)

;; 1-syntax.watsup:190.1-191.25
syntax global = GLOBAL(globaltype, expr)

;; 1-syntax.watsup:192.1-193.18
syntax table = TABLE(tabletype)

;; 1-syntax.watsup:194.1-195.17
syntax mem = MEMORY(memtype)

;; 1-syntax.watsup:196.1-197.31
syntax elem = ELEM(reftype, expr*, elemmode?)

;; 1-syntax.watsup:198.1-199.26
syntax data = DATA(byte**, datamode?)

;; 1-syntax.watsup:200.1-201.16
syntax start = START(funcidx)

;; 1-syntax.watsup:203.1-204.65
syntax externuse =
  | FUNC(funcidx)
  | GLOBAL(globalidx)
  | TABLE(tableidx)
  | MEMORY(memidx)

;; 1-syntax.watsup:205.1-206.24
syntax export = EXPORT(name, externuse)

;; 1-syntax.watsup:207.1-208.30
syntax import = IMPORT(name, name, externtype)

;; 1-syntax.watsup:210.1-211.70
syntax module = MODULE(import*, func*, global*, table*, mem*, elem*, data*, start*, export*)

;; 2-aux.watsup:5.1-5.41
def size : valtype -> nat
  ;; 2-aux.watsup:10.1-10.22
  def size(V128_valtype) = 128
  ;; 2-aux.watsup:9.1-9.20
  def size(F64_valtype) = 64
  ;; 2-aux.watsup:8.1-8.20
  def size(F32_valtype) = 32
  ;; 2-aux.watsup:7.1-7.20
  def size(I64_valtype) = 64
  ;; 2-aux.watsup:6.1-6.20
  def size(I32_valtype) = 32

;; 2-aux.watsup:15.1-15.40
def test_sub_ATOM_22 : n -> nat
  ;; 2-aux.watsup:16.1-16.38
  def {n_3_ATOM_y : n} test_sub_ATOM_22(n_3_ATOM_y) = 0

;; 2-aux.watsup:18.1-18.26
def curried_ : (n, n) -> nat
  ;; 2-aux.watsup:19.1-19.39
  def {n_1 : n, n_2 : n} curried_(n_1, n_2) = (n_1 + n_2)

;; 2-aux.watsup:21.1-30.39
syntax testfuse =
  | AB_(nat, nat, nat)
  | CD(nat, nat, nat)
  | EF(nat, nat, nat)
  | GH(nat, nat, nat)
  | IJ(nat, nat, nat)
  | KL(nat, nat, nat)
  | MN(nat, nat, nat)
  | OP(nat, nat, nat)
  | QR(nat, nat, nat)

;; 3-typing.watsup:3.1-6.60
syntax context = {FUNC functype*, GLOBAL globaltype*, TABLE tabletype*, MEM memtype*, ELEM elemtype*, DATA datatype*, LOCAL valtype*, LABEL resulttype*, RETURN resulttype?}

;; 3-typing.watsup:14.1-14.66
relation Limits_ok: `|-%:%`(limits, nat)
  ;; 3-typing.watsup:22.1-24.25
  rule _ {k : nat, n_1 : n, n_2 : n}:
    `|-%:%`(`[%..%]`(n_1, n_2), k)
    -- iff ((n_1 <= n_2) /\ (n_2 <= k))

;; 3-typing.watsup:15.1-15.64
relation Functype_ok: `|-%:OK`(functype)
  ;; 3-typing.watsup:26.1-27.13
  rule _ {ft : functype}:
    `|-%:OK`(ft)

;; 3-typing.watsup:16.1-16.66
relation Globaltype_ok: `|-%:OK`(globaltype)
  ;; 3-typing.watsup:29.1-30.13
  rule _ {gt : globaltype}:
    `|-%:OK`(gt)

;; 3-typing.watsup:17.1-17.65
relation Tabletype_ok: `|-%:OK`(tabletype)
  ;; 3-typing.watsup:32.1-34.35
  rule _ {lim : limits, rt : reftype}:
    `|-%:OK`(`%%`(lim, rt))
    -- Limits_ok: `|-%:%`(lim, ((2 ^ 32) - 1))

;; 3-typing.watsup:18.1-18.63
relation Memtype_ok: `|-%:OK`(memtype)
  ;; 3-typing.watsup:36.1-38.33
  rule _ {lim : limits}:
    `|-%:OK`(`%I8`(lim))
    -- Limits_ok: `|-%:%`(lim, (2 ^ 16))

;; 3-typing.watsup:19.1-19.66
relation Externtype_ok: `|-%:OK`(externtype)
  ;; 3-typing.watsup:53.1-55.33
  rule mem {memtype : memtype}:
    `|-%:OK`(MEMORY_externtype(memtype))
    -- Memtype_ok: `|-%:OK`(memtype)

  ;; 3-typing.watsup:49.1-51.37
  rule table {tabletype : tabletype}:
    `|-%:OK`(TABLE_externtype(tabletype))
    -- Tabletype_ok: `|-%:OK`(tabletype)

  ;; 3-typing.watsup:45.1-47.39
  rule global {globaltype : globaltype}:
    `|-%:OK`(GLOBAL_externtype(globaltype))
    -- Globaltype_ok: `|-%:OK`(globaltype)

  ;; 3-typing.watsup:41.1-43.35
  rule func {functype : functype}:
    `|-%:OK`(FUNC_externtype(functype))
    -- Functype_ok: `|-%:OK`(functype)

;; 3-typing.watsup:61.1-61.65
relation Valtype_sub: `|-%<:%`(valtype, valtype)
  ;; 3-typing.watsup:67.1-68.14
  rule bot {t : valtype}:
    `|-%<:%`(BOT_valtype, t)

  ;; 3-typing.watsup:64.1-65.12
  rule refl {t : valtype}:
    `|-%<:%`(t, t)

;; 3-typing.watsup:62.1-62.72
relation Resulttype_sub: `|-%<:%`(valtype*, valtype*)
  ;; 3-typing.watsup:70.1-72.35
  rule _ {t_1 : valtype, t_2 : valtype}:
    `|-%<:%`(t_1*, t_2*)
    -- (Valtype_sub: `|-%<:%`(t_1, t_2))*

;; 3-typing.watsup:75.1-75.75
relation Limits_sub: `|-%<:%`(limits, limits)
  ;; 3-typing.watsup:83.1-86.22
  rule _ {n_11 : n, n_12 : n, n_21 : n, n_22 : n}:
    `|-%<:%`(`[%..%]`(n_11, n_12), `[%..%]`(n_21, n_22))
    -- iff (n_11 >= n_21)
    -- iff (n_12 <= n_22)

;; 3-typing.watsup:76.1-76.73
relation Functype_sub: `|-%<:%`(functype, functype)
  ;; 3-typing.watsup:88.1-89.14
  rule _ {ft : functype}:
    `|-%<:%`(ft, ft)

;; 3-typing.watsup:77.1-77.75
relation Globaltype_sub: `|-%<:%`(globaltype, globaltype)
  ;; 3-typing.watsup:91.1-92.14
  rule _ {gt : globaltype}:
    `|-%<:%`(gt, gt)

;; 3-typing.watsup:78.1-78.74
relation Tabletype_sub: `|-%<:%`(tabletype, tabletype)
  ;; 3-typing.watsup:94.1-96.35
  rule _ {lim_1 : limits, lim_2 : limits, rt : reftype}:
    `|-%<:%`(`%%`(lim_1, rt), `%%`(lim_2, rt))
    -- Limits_sub: `|-%<:%`(lim_1, lim_2)

;; 3-typing.watsup:79.1-79.72
relation Memtype_sub: `|-%<:%`(memtype, memtype)
  ;; 3-typing.watsup:98.1-100.35
  rule _ {lim_1 : limits, lim_2 : limits}:
    `|-%<:%`(`%I8`(lim_1), `%I8`(lim_2))
    -- Limits_sub: `|-%<:%`(lim_1, lim_2)

;; 3-typing.watsup:80.1-80.75
relation Externtype_sub: `|-%<:%`(externtype, externtype)
  ;; 3-typing.watsup:115.1-117.34
  rule mem {mt_1 : memtype, mt_2 : memtype}:
    `|-%<:%`(MEMORY_externtype(mt_1), MEMORY_externtype(mt_2))
    -- Memtype_sub: `|-%<:%`(mt_1, mt_2)

  ;; 3-typing.watsup:111.1-113.36
  rule table {tt_1 : tabletype, tt_2 : tabletype}:
    `|-%<:%`(TABLE_externtype(tt_1), TABLE_externtype(tt_2))
    -- Tabletype_sub: `|-%<:%`(tt_1, tt_2)

  ;; 3-typing.watsup:107.1-109.37
  rule global {gt_1 : globaltype, gt_2 : globaltype}:
    `|-%<:%`(GLOBAL_externtype(gt_1), GLOBAL_externtype(gt_2))
    -- Globaltype_sub: `|-%<:%`(gt_1, gt_2)

  ;; 3-typing.watsup:103.1-105.35
  rule func {ft_1 : functype, ft_2 : functype}:
    `|-%<:%`(FUNC_externtype(ft_1), FUNC_externtype(ft_2))
    -- Functype_sub: `|-%<:%`(ft_1, ft_2)

;; 3-typing.watsup:172.1-172.76
relation Blocktype_ok: `%|-%:%`(context, blocktype, functype)
  ;; 3-typing.watsup:174.1-176.29
  rule _ {C : context, ft : functype}:
    `%|-%:%`(C, ft, ft)
    -- Functype_ok: `|-%:OK`(ft)

;; 3-typing.watsup:123.1-124.67
rec {

;; 3-typing.watsup:123.1-123.66
relation Instr_ok: `%|-%:%`(context, instr, functype)
  ;; 3-typing.watsup:357.1-362.33
  rule store {C : context, in : in, mt : memtype, n : n, n_A : n, n_O : n, nt : numtype, t : valtype}:
    `%|-%:%`(C, STORE_instr(nt, n?, n_A, n_O), `%->%`([I32_valtype (nt <: valtype)], []))
    -- iff (C.MEM[0] = mt)
    -- iff ((2 ^ n_A) <= ($size(t) / 8))
    -- (iff (((2 ^ n_A) <= (n / 8)) /\ ((n / 8) < ($size(t) / 8))))?
    -- iff ((n? = ?()) \/ (nt = (in <: numtype)))

  ;; 3-typing.watsup:350.1-355.33
  rule load {C : context, in : in, mt : memtype, n : n, n_A : n, n_O : n, nt : numtype, sx : sx, t : valtype}:
    `%|-%:%`(C, LOAD_instr(nt, ?((n, sx)), n_A, n_O), `%->%`([I32_valtype], [(nt <: valtype)]))
    -- iff (C.MEM[0] = mt)
    -- iff ((2 ^ n_A) <= ($size(t) / 8))
    -- (iff (((2 ^ n_A) <= (n / 8)) /\ ((n / 8) < ($size(t) / 8))))?
    -- iff ((n? = ?()) \/ (nt = (in <: numtype)))

  ;; 3-typing.watsup:346.1-348.24
  rule data.drop {C : context, x : idx}:
    `%|-%:%`(C, DATA.DROP_instr(x), `%->%`([], []))
    -- iff (C.DATA[x] = OK)

  ;; 3-typing.watsup:341.1-344.24
  rule memory.init {C : context, mt : memtype, x : idx}:
    `%|-%:%`(C, MEMORY.INIT_instr(x), `%->%`([I32_valtype I32_valtype I32_valtype], [I32_valtype]))
    -- iff (C.MEM[0] = mt)
    -- iff (C.DATA[x] = OK)

  ;; 3-typing.watsup:337.1-339.23
  rule memory.copy {C : context, mt : memtype}:
    `%|-%:%`(C, MEMORY.COPY_instr, `%->%`([I32_valtype I32_valtype I32_valtype], [I32_valtype]))
    -- iff (C.MEM[0] = mt)

  ;; 3-typing.watsup:333.1-335.23
  rule memory.fill {C : context, mt : memtype}:
    `%|-%:%`(C, MEMORY.FILL_instr, `%->%`([I32_valtype I32_valtype I32_valtype], [I32_valtype]))
    -- iff (C.MEM[0] = mt)

  ;; 3-typing.watsup:329.1-331.23
  rule memory.grow {C : context, mt : memtype}:
    `%|-%:%`(C, MEMORY.GROW_instr, `%->%`([I32_valtype], [I32_valtype]))
    -- iff (C.MEM[0] = mt)

  ;; 3-typing.watsup:325.1-327.23
  rule memory.size {C : context, mt : memtype}:
    `%|-%:%`(C, MEMORY.SIZE_instr, `%->%`([], [I32_valtype]))
    -- iff (C.MEM[0] = mt)

  ;; 3-typing.watsup:320.1-322.24
  rule elem.drop {C : context, rt : reftype, x : idx}:
    `%|-%:%`(C, ELEM.DROP_instr(x), `%->%`([], []))
    -- iff (C.ELEM[x] = rt)

  ;; 3-typing.watsup:315.1-318.26
  rule table.init {C : context, lim : limits, rt : reftype, x_1 : idx, x_2 : idx}:
    `%|-%:%`(C, TABLE.INIT_instr(x_1, x_2), `%->%`([I32_valtype I32_valtype I32_valtype], []))
    -- iff (C.TABLE[x_1] = `%%`(lim, rt))
    -- iff (C.ELEM[x_2] = rt)

  ;; 3-typing.watsup:310.1-313.33
  rule table.copy {C : context, lim_1 : limits, lim_2 : limits, rt : reftype, x_1 : idx, x_2 : idx}:
    `%|-%:%`(C, TABLE.COPY_instr(x_1, x_2), `%->%`([I32_valtype I32_valtype I32_valtype], []))
    -- iff (C.TABLE[x_1] = `%%`(lim_1, rt))
    -- iff (C.TABLE[x_2] = `%%`(lim_2, rt))

  ;; 3-typing.watsup:306.1-308.29
  rule table.fill {C : context, lim : limits, rt : reftype, x : idx}:
    `%|-%:%`(C, TABLE.FILL_instr(x), `%->%`([I32_valtype (rt <: valtype) I32_valtype], []))
    -- iff (C.TABLE[x] = `%%`(lim, rt))

  ;; 3-typing.watsup:302.1-304.29
  rule table.grow {C : context, lim : limits, rt : reftype, x : idx}:
    `%|-%:%`(C, TABLE.GROW_instr(x), `%->%`([(rt <: valtype) I32_valtype], [I32_valtype]))
    -- iff (C.TABLE[x] = `%%`(lim, rt))

  ;; 3-typing.watsup:298.1-300.25
  rule table.size {C : context, tt : tabletype, x : idx}:
    `%|-%:%`(C, TABLE.SIZE_instr(x), `%->%`([], [I32_valtype]))
    -- iff (C.TABLE[x] = tt)

  ;; 3-typing.watsup:294.1-296.29
  rule table.set {C : context, lim : limits, rt : reftype, x : idx}:
    `%|-%:%`(C, TABLE.SET_instr(x), `%->%`([I32_valtype (rt <: valtype)], []))
    -- iff (C.TABLE[x] = `%%`(lim, rt))

  ;; 3-typing.watsup:290.1-292.29
  rule table.get {C : context, lim : limits, rt : reftype, x : idx}:
    `%|-%:%`(C, TABLE.GET_instr(x), `%->%`([I32_valtype], [(rt <: valtype)]))
    -- iff (C.TABLE[x] = `%%`(lim, rt))

  ;; 3-typing.watsup:285.1-287.29
  rule global.set {C : context, t : valtype, x : idx}:
    `%|-%:%`(C, GLOBAL.SET_instr(x), `%->%`([t], []))
    -- iff (C.GLOBAL[x] = `MUT%?%`(?(()), t))

  ;; 3-typing.watsup:281.1-283.30
  rule global.get {C : context, t : valtype, x : idx}:
    `%|-%:%`(C, GLOBAL.GET_instr(x), `%->%`([], [t]))
    -- iff (C.GLOBAL[x] = `MUT%?%`(?(()), t))

  ;; 3-typing.watsup:276.1-278.24
  rule local.tee {C : context, t : valtype, x : idx}:
    `%|-%:%`(C, LOCAL.TEE_instr(x), `%->%`([t], [t]))
    -- iff (C.LOCAL[x] = t)

  ;; 3-typing.watsup:272.1-274.24
  rule local.set {C : context, t : valtype, x : idx}:
    `%|-%:%`(C, LOCAL.SET_instr(x), `%->%`([t], []))
    -- iff (C.LOCAL[x] = t)

  ;; 3-typing.watsup:268.1-270.24
  rule local.get {C : context, t : valtype, x : idx}:
    `%|-%:%`(C, LOCAL.GET_instr(x), `%->%`([], [t]))
    -- iff (C.LOCAL[x] = t)

  ;; 3-typing.watsup:264.1-265.31
  rule ref.is_null {C : context, rt : reftype}:
    `%|-%:%`(C, REF.IS_NULL_instr, `%->%`([(rt <: valtype)], [I32_valtype]))

  ;; 3-typing.watsup:260.1-262.24
  rule ref.func {C : context, ft : functype, x : idx}:
    `%|-%:%`(C, REF.FUNC_instr(x), `%->%`([], [FUNCREF_valtype]))
    -- iff (C.FUNC[x] = ft)

  ;; 3-typing.watsup:257.1-258.35
  rule ref.null {C : context, rt : reftype}:
    `%|-%:%`(C, REF.NULL_instr(rt), `%->%`([], [(rt <: valtype)]))

  ;; 3-typing.watsup:252.1-254.23
  rule convert-f {C : context, fn_1 : fn, fn_2 : fn}:
    `%|-%:%`(C, CVTOP_instr((fn_1 <: numtype), CONVERT_cvtop, (fn_2 <: numtype), ?()), `%->%`([(fn_2 <: valtype)], [(fn_1 <: valtype)]))
    -- iff (fn_1 =/= fn_2)

  ;; 3-typing.watsup:247.1-250.53
  rule convert-i {C : context, in_1 : in, in_2 : in, sx : sx}:
    `%|-%:%`(C, CVTOP_instr((in_1 <: numtype), CONVERT_cvtop, (in_2 <: numtype), sx?), `%->%`([(in_2 <: valtype)], [(in_1 <: valtype)]))
    -- iff (in_1 =/= in_2)
    -- iff ((sx? = ?()) <=> ($size(in_1 <: valtype) > $size(in_2 <: valtype)))

  ;; 3-typing.watsup:242.1-245.35
  rule reinterpret {C : context, nt_1 : numtype, nt_2 : numtype}:
    `%|-%:%`(C, CVTOP_instr(nt_1, REINTERPRET_cvtop, nt_2, ?()), `%->%`([(nt_2 <: valtype)], [(nt_1 <: valtype)]))
    -- iff (nt_1 =/= nt_2)
    -- iff ($size(nt_1 <: valtype) = $size(nt_2 <: valtype))

  ;; 3-typing.watsup:238.1-240.24
  rule extend {C : context, n : n, nt : numtype}:
    `%|-%:%`(C, EXTEND_instr(nt, n), `%->%`([(nt <: valtype)], [(nt <: valtype)]))
    -- iff (n <= $size(nt <: valtype))

  ;; 3-typing.watsup:234.1-235.37
  rule relop {C : context, nt : numtype, relop : relop_numtype}:
    `%|-%:%`(C, RELOP_instr(nt, relop), `%->%`([(nt <: valtype) (nt <: valtype)], [I32_valtype]))

  ;; 3-typing.watsup:231.1-232.36
  rule testop {C : context, nt : numtype, testop : testop_numtype}:
    `%|-%:%`(C, TESTOP_instr(nt, testop), `%->%`([(nt <: valtype)], [I32_valtype]))

  ;; 3-typing.watsup:228.1-229.36
  rule binop {C : context, binop : binop_numtype, nt : numtype}:
    `%|-%:%`(C, BINOP_instr(nt, binop), `%->%`([(nt <: valtype) (nt <: valtype)], [(nt <: valtype)]))

  ;; 3-typing.watsup:225.1-226.31
  rule unop {C : context, nt : numtype, unop : unop_numtype}:
    `%|-%:%`(C, UNOP_instr(nt, unop), `%->%`([(nt <: valtype)], [(nt <: valtype)]))

  ;; 3-typing.watsup:222.1-223.37
  rule const {C : context, c_nt : c_numtype, nt : numtype}:
    `%|-%:%`(C, CONST_instr(nt, c_nt), `%->%`([], [(nt <: valtype)]))

  ;; 3-typing.watsup:216.1-219.27
  rule call_indirect {C : context, ft : functype, lim : limits, t_1 : valtype, t_2 : valtype, x : idx}:
    `%|-%:%`(C, CALL_INDIRECT_instr(x, ft), `%->%`(t_1* :: [I32_valtype], t_2*))
    -- iff (C.TABLE[x] = `%%`(lim, FUNCREF_reftype))
    -- iff (ft = `%->%`(t_1*, t_2*))

  ;; 3-typing.watsup:212.1-214.34
  rule call {C : context, t_1 : valtype, t_2 : valtype, x : idx}:
    `%|-%:%`(C, CALL_instr(x), `%->%`(t_1*, t_2*))
    -- iff (C.FUNC[x] = `%->%`(t_1*, t_2*))

  ;; 3-typing.watsup:208.1-210.25
  rule return {C : context, t : valtype, t_1 : valtype, t_2 : valtype}:
    `%|-%:%`(C, RETURN_instr, `%->%`(t_1* :: t*, t_2*))
    -- iff (C.RETURN = ?(t*))

  ;; 3-typing.watsup:203.1-206.42
  rule br_table {C : context, l : labelidx, l' : labelidx, t : valtype, t_1 : valtype, t_2 : valtype}:
    `%|-%:%`(C, BR_TABLE_instr(l*, l'), `%->%`(t_1* :: t*, t_2*))
    -- (Resulttype_sub: `|-%<:%`(t*, C.LABEL[l]))*
    -- Resulttype_sub: `|-%<:%`(t*, C.LABEL[l'])

  ;; 3-typing.watsup:199.1-201.25
  rule br_if {C : context, l : labelidx, t : valtype}:
    `%|-%:%`(C, BR_IF_instr(l), `%->%`(t* :: [I32_valtype], t*))
    -- iff (C.LABEL[l] = t*)

  ;; 3-typing.watsup:195.1-197.25
  rule br {C : context, l : labelidx, t : valtype, t_1 : valtype, t_2 : valtype}:
    `%|-%:%`(C, BR_instr(l), `%->%`(t_1* :: t*, t_2*))
    -- iff (C.LABEL[l] = t*)

  ;; 3-typing.watsup:188.1-192.59
  rule if {C : context, bt : blocktype, instr_1 : instr, instr_2 : instr, t_1 : valtype, t_2 : valtype}:
    `%|-%:%`(C, IF_instr(bt, instr_1*, instr_2*), `%->%`(t_1*, [t_2]))
    -- Blocktype_ok: `%|-%:%`(C, bt, `%->%`(t_1*, [t_2]))
    -- InstrSeq_ok: `%|-%:%`(C ++ {FUNC [], GLOBAL [], TABLE [], MEM [], ELEM [], DATA [], LOCAL [], LABEL [t_2]*, RETURN ?()}, instr_1*, `%->%`(t_1*, t_2*))
    -- InstrSeq_ok: `%|-%:%`(C ++ {FUNC [], GLOBAL [], TABLE [], MEM [], ELEM [], DATA [], LOCAL [], LABEL [t_2]*, RETURN ?()}, instr_2*, `%->%`(t_1*, t_2*))

  ;; 3-typing.watsup:183.1-186.56
  rule loop {C : context, bt : blocktype, instr : instr, t_1 : valtype, t_2 : valtype}:
    `%|-%:%`(C, LOOP_instr(bt, instr*), `%->%`(t_1*, t_2*))
    -- Blocktype_ok: `%|-%:%`(C, bt, `%->%`(t_1*, t_2*))
    -- InstrSeq_ok: `%|-%:%`(C ++ {FUNC [], GLOBAL [], TABLE [], MEM [], ELEM [], DATA [], LOCAL [], LABEL [t_1]*, RETURN ?()}, instr*, `%->%`(t_1*, [t_2]))

  ;; 3-typing.watsup:178.1-181.57
  rule block {C : context, bt : blocktype, instr : instr, t_1 : valtype, t_2 : valtype}:
    `%|-%:%`(C, BLOCK_instr(bt, instr*), `%->%`(t_1*, t_2*))
    -- Blocktype_ok: `%|-%:%`(C, bt, `%->%`(t_1*, t_2*))
    -- InstrSeq_ok: `%|-%:%`(C ++ {FUNC [], GLOBAL [], TABLE [], MEM [], ELEM [], DATA [], LOCAL [], LABEL [t_2]*, RETURN ?()}, instr*, `%->%`(t_1*, t_2*))

  ;; 3-typing.watsup:166.1-169.38
  rule select-impl {C : context, numtype : numtype, t : valtype, t' : valtype, vectype : vectype}:
    `%|-%:%`(C, SELECT_instr(?()), `%->%`([t t I32_valtype], [t]))
    -- Valtype_sub: `|-%<:%`(t, t')
    -- iff ((t' = (numtype <: valtype)) \/ (t' = (vectype <: valtype)))

  ;; 3-typing.watsup:163.1-164.31
  rule select-expl {C : context, t : valtype}:
    `%|-%:%`(C, SELECT_instr(?(t)), `%->%`([t t I32_valtype], [t]))

  ;; 3-typing.watsup:159.1-160.27
  rule drop {C : context, t : valtype}:
    `%|-%:%`(C, DROP_instr, `%->%`([t], []))

  ;; 3-typing.watsup:156.1-157.32
  rule nop {C : context}:
    `%|-%:%`(C, NOP_instr, `%->%`([], []))

  ;; 3-typing.watsup:153.1-154.34
  rule unreachable {C : context, t_1 : valtype, t_2 : valtype}:
    `%|-%:%`(C, UNREACHABLE_instr, `%->%`(t_1*, t_2*))

;; 3-typing.watsup:124.1-124.67
relation InstrSeq_ok: `%|-%:%`(context, instr*, functype)
  ;; 3-typing.watsup:148.1-150.45
  rule frame {C : context, instr : instr, t : valtype, t_1 : valtype, t_2 : valtype}:
    `%|-%:%`(C, instr*, `%->%`(t* :: t_1*, t* :: t_2*))
    -- InstrSeq_ok: `%|-%:%`(C, instr*, `%->%`(t_1*, t_2*))

  ;; 3-typing.watsup:141.1-146.38
  rule weak {C : context, instr : instr, t'_1 : valtype, t'_2 : valtype, t_1 : valtype, t_2 : valtype}:
    `%|-%:%`(C, instr*, `%->%`([t'_1], t'_2*))
    -- InstrSeq_ok: `%|-%:%`(C, instr*, `%->%`(t_1*, t_2*))
    -- Resulttype_sub: `|-%<:%`(t'_1*, t_1*)
    -- Resulttype_sub: `|-%<:%`(t_2*, t'_2*)

  ;; 3-typing.watsup:136.1-139.46
  rule seq {C : context, instr_1 : instr, instr_2 : instr, t_1 : valtype, t_2 : valtype, t_3 : valtype}:
    `%|-%:%`(C, [instr_1] :: instr_2*, `%->%`(t_1*, t_3*))
    -- Instr_ok: `%|-%:%`(C, instr_1, `%->%`(t_1*, t_2*))
    -- InstrSeq_ok: `%|-%:%`(C, [instr_2], `%->%`(t_2*, t_3*))

  ;; 3-typing.watsup:133.1-134.36
  rule empty {C : context}:
    `%|-%:%`(C, [], `%->%`([], []))
}

;; 3-typing.watsup:125.1-125.71
relation Expr_ok: `%|-%:%`(context, expr, resulttype)
  ;; 3-typing.watsup:128.1-130.46
  rule _ {C : context, instr : instr, t : valtype}:
    `%|-%:%`(C, instr*, t*)
    -- InstrSeq_ok: `%|-%:%`(C, instr*, `%->%`([], t*))

;; 3-typing.watsup:367.1-367.78
relation Instr_const: `%|-%CONST`(context, instr)
  ;; 3-typing.watsup:380.1-382.33
  rule global.get {C : context, t : valtype, x : idx}:
    `%|-%CONST`(C, GLOBAL.GET_instr(x))
    -- iff (C.GLOBAL[x] = `MUT%?%`(?(), t))

  ;; 3-typing.watsup:377.1-378.26
  rule ref.func {C : context, x : idx}:
    `%|-%CONST`(C, REF.FUNC_instr(x))

  ;; 3-typing.watsup:374.1-375.27
  rule ref.null {C : context, rt : reftype}:
    `%|-%CONST`(C, REF.NULL_instr(rt))

  ;; 3-typing.watsup:371.1-372.26
  rule const {C : context, c : c_numtype, nt : numtype}:
    `%|-%CONST`(C, CONST_instr(nt, c))

;; 3-typing.watsup:368.1-368.77
relation Expr_const: `%|-%CONST`(context, expr)
  ;; 3-typing.watsup:385.1-386.38
  rule _ {C : context, instr : instr}:
    `%|-%CONST`(C, instr*)
    -- (Instr_const: `%|-%CONST`(C, instr))*

;; 3-typing.watsup:369.1-369.78
relation Expr_ok_const: `%|-%:%CONST`(context, expr, valtype)
  ;; 3-typing.watsup:389.1-392.33
  rule _ {C : context, expr : expr, t : valtype}:
    `%|-%:%CONST`(C, expr, t)
    -- Expr_ok: `%|-%:%`(C, expr, [t])
    -- Expr_const: `%|-%CONST`(C, expr)

;; 3-typing.watsup:397.1-397.73
relation Func_ok: `%|-%:%`(context, func, functype)
  ;; 3-typing.watsup:408.1-412.75
  rule _ {C : context, expr : expr, ft : functype, t : valtype, t_1 : valtype, t_2 : valtype}:
    `%|-%:%`(C, FUNC(ft, t*, expr), ft)
    -- iff (ft = `%->%`(t_1*, t_2*))
    -- Functype_ok: `|-%:OK`(ft)
    -- Expr_ok: `%|-%:%`(C ++ {FUNC [], GLOBAL [], TABLE [], MEM [], ELEM [], DATA [], LOCAL t_1* :: t*, LABEL [], RETURN ?()} ++ {FUNC [], GLOBAL [], TABLE [], MEM [], ELEM [], DATA [], LOCAL [], LABEL [t_2*], RETURN ?()} ++ {FUNC [], GLOBAL [], TABLE [], MEM [], ELEM [], DATA [], LOCAL [], LABEL [], RETURN ?(t_2*)}, expr, t_2*)

;; 3-typing.watsup:398.1-398.75
relation Global_ok: `%|-%:%`(context, global, globaltype)
  ;; 3-typing.watsup:414.1-418.40
  rule _ {C : context, expr : expr, gt : globaltype, t : valtype}:
    `%|-%:%`(C, GLOBAL(gt, expr), gt)
    -- Globaltype_ok: `|-%:OK`(gt)
    -- iff (gt = `MUT%?%`(?(()), t))
    -- Expr_ok_const: `%|-%:%CONST`(C, expr, t)

;; 3-typing.watsup:399.1-399.74
relation Table_ok: `%|-%:%`(context, table, tabletype)
  ;; 3-typing.watsup:420.1-422.30
  rule _ {C : context, tt : tabletype}:
    `%|-%:%`(C, TABLE(tt), tt)
    -- Tabletype_ok: `|-%:OK`(tt)

;; 3-typing.watsup:400.1-400.72
relation Mem_ok: `%|-%:%`(context, mem, memtype)
  ;; 3-typing.watsup:424.1-426.28
  rule _ {C : context, mt : memtype}:
    `%|-%:%`(C, MEMORY(mt), mt)
    -- Memtype_ok: `|-%:OK`(mt)

;; 3-typing.watsup:403.1-403.77
relation Elemmode_ok: `%|-%:%`(context, elemmode, reftype)
  ;; 3-typing.watsup:443.1-444.20
  rule declare {C : context, rt : reftype}:
    `%|-%:%`(C, DECLARE_elemmode, rt)

  ;; 3-typing.watsup:438.1-441.45
  rule active {C : context, expr : expr, lim : limits, rt : reftype, x : idx}:
    `%|-%:%`(C, TABLE_elemmode(x, expr), rt)
    -- iff (C.TABLE[x] = `%%`(lim, rt))
    -- (Expr_ok_const: `%|-%:%CONST`(C, expr, I32_valtype))*

;; 3-typing.watsup:401.1-401.73
relation Elem_ok: `%|-%:%`(context, elem, reftype)
  ;; 3-typing.watsup:428.1-431.40
  rule _ {C : context, elemmode : elemmode, expr : expr, rt : reftype}:
    `%|-%:%`(C, ELEM(rt, expr*, elemmode?), rt)
    -- (Expr_ok: `%|-%:%`(C, expr, [(rt <: valtype)]))*
    -- (Elemmode_ok: `%|-%:%`(C, elemmode, rt))?

;; 3-typing.watsup:404.1-404.77
relation Datamode_ok: `%|-%:OK`(context, datamode)
  ;; 3-typing.watsup:446.1-449.45
  rule _ {C : context, expr : expr, mt : memtype}:
    `%|-%:OK`(C, MEMORY_datamode(0, expr))
    -- iff (C.MEM[0] = mt)
    -- (Expr_ok_const: `%|-%:%CONST`(C, expr, I32_valtype))*

;; 3-typing.watsup:402.1-402.73
relation Data_ok: `%|-%:OK`(context, data)
  ;; 3-typing.watsup:434.1-436.40
  rule _ {C : context, b : byte, datamode : datamode}:
    `%|-%:OK`(C, DATA(b**, datamode?))
    -- (Datamode_ok: `%|-%:OK`(C, datamode))?

;; 3-typing.watsup:405.1-405.74
relation Start_ok: `%|-%:OK`(context, start)
  ;; 3-typing.watsup:451.1-453.40
  rule _ {C : context, x : idx}:
    `%|-%:OK`(C, START(x))
    -- iff (C.FUNC[x] = `%->%`([], []))

;; 3-typing.watsup:456.1-456.80
relation Import_ok: `%|-%:%`(context, import, externtype)
  ;; 3-typing.watsup:460.1-462.31
  rule _ {C : context, name_1 : name, name_2 : name, xt : externtype}:
    `%|-%:%`(C, IMPORT(name_1, name_2, xt), xt)
    -- Externtype_ok: `|-%:OK`(xt)

;; 3-typing.watsup:458.1-458.83
relation Externuse_ok: `%|-%:%`(context, externuse, externtype)
  ;; 3-typing.watsup:480.1-482.23
  rule mem {C : context, mt : memtype, x : idx}:
    `%|-%:%`(C, MEMORY_externuse(x), MEMORY_externtype(mt))
    -- iff (C.MEM[x] = mt)

  ;; 3-typing.watsup:476.1-478.25
  rule table {C : context, tt : tabletype, x : idx}:
    `%|-%:%`(C, TABLE_externuse(x), TABLE_externtype(tt))
    -- iff (C.TABLE[x] = tt)

  ;; 3-typing.watsup:472.1-474.26
  rule global {C : context, gt : globaltype, x : idx}:
    `%|-%:%`(C, GLOBAL_externuse(x), GLOBAL_externtype(gt))
    -- iff (C.GLOBAL[x] = gt)

  ;; 3-typing.watsup:468.1-470.24
  rule func {C : context, ft : functype, x : idx}:
    `%|-%:%`(C, FUNC_externuse(x), FUNC_externtype(ft))
    -- iff (C.FUNC[x] = ft)

;; 3-typing.watsup:457.1-457.80
relation Export_ok: `%|-%:%`(context, export, externtype)
  ;; 3-typing.watsup:464.1-466.39
  rule _ {C : context, externuse : externuse, name : name, xt : externtype}:
    `%|-%:%`(C, EXPORT(name, externuse), xt)
    -- Externuse_ok: `%|-%:%`(C, externuse, xt)

;; 3-typing.watsup:485.1-485.62
relation Module_ok: `|-%:OK`(module)
  ;; 3-typing.watsup:487.1-499.23
  rule _ {C : context, data : data, elem : elem, export : export, ft : functype, func : func, global : global, gt : globaltype, import : import, mem : mem, mt : memtype, n : n, rt : reftype, start : start, table : table, tt : tabletype}:
    `|-%:OK`(MODULE(import*, func*, global*, table*, mem*, elem*, data^n, start*, export*))
    -- (Func_ok: `%|-%:%`(C, func, ft))*
    -- (Global_ok: `%|-%:%`(C, global, gt))*
    -- (Table_ok: `%|-%:%`(C, table, tt))*
    -- (Mem_ok: `%|-%:%`(C, mem, mt))*
    -- (Elem_ok: `%|-%:%`(C, elem, rt))*
    -- (Data_ok: `%|-%:OK`(C, data))^n
    -- (Start_ok: `%|-%:OK`(C, start))*
    -- iff (C = {FUNC ft*, GLOBAL gt*, TABLE tt*, MEM mt*, ELEM rt*, DATA OK^n, LOCAL [], LABEL [], RETURN ?()})
    -- iff (|mem*| <= 1)
    -- iff (|start*| <= 1)

;; 4-runtime.watsup:3.1-3.39
syntax addr = nat

;; 4-runtime.watsup:4.1-4.53
syntax funcaddr = addr

;; 4-runtime.watsup:5.1-5.53
syntax globaladdr = addr

;; 4-runtime.watsup:6.1-6.51
syntax tableaddr = addr

;; 4-runtime.watsup:7.1-7.50
syntax memaddr = addr

;; 4-runtime.watsup:8.1-8.49
syntax elemaddr = addr

;; 4-runtime.watsup:9.1-9.49
syntax dataaddr = addr

;; 4-runtime.watsup:10.1-10.51
syntax labeladdr = addr

;; 4-runtime.watsup:11.1-11.49
syntax hostaddr = addr

;; 4-runtime.watsup:24.1-25.24
syntax num =
  | CONST(numtype, c_numtype)

;; 4-runtime.watsup:26.1-27.67
syntax ref =
  | REF.NULL(reftype)
  | REF.FUNC_ADDR(funcaddr)
  | REF.HOST_ADDR(hostaddr)

;; 4-runtime.watsup:28.1-29.10
syntax val =
  | num
  | ref

;; 4-runtime.watsup:31.1-32.18
syntax result =
  | _VALS(val*)
  | TRAP

;; 4-runtime.watsup:38.1-39.66
syntax externval =
  | FUNC(funcaddr)
  | GLOBAL(globaladdr)
  | TABLE(tableaddr)
  | MEM(memaddr)

;; 4-runtime.watsup:44.1-44.29
def default_ : valtype -> val
  ;; 4-runtime.watsup:49.1-49.34
  def {rt : reftype} default_(rt <: valtype) = REF.NULL_val(rt)
  ;; 4-runtime.watsup:48.1-48.35
  def default_(F64_valtype) = CONST_val(F64_numtype, 0)
  ;; 4-runtime.watsup:47.1-47.35
  def default_(F32_valtype) = CONST_val(F32_numtype, 0)
  ;; 4-runtime.watsup:46.1-46.35
  def default_(I64_valtype) = CONST_val(I64_numtype, 0)
  ;; 4-runtime.watsup:45.1-45.35
  def default_(I32_valtype) = CONST_val(I32_numtype, 0)

;; 4-runtime.watsup:60.1-60.71
syntax exportinst = EXPORT(name, externval)

;; 4-runtime.watsup:70.1-77.25
syntax moduleinst = {FUNC funcaddr*, GLOBAL globaladdr*, TABLE tableaddr*, MEM memaddr*, ELEM elemaddr*, DATA dataaddr*, EXPORT exportinst*}

;; 4-runtime.watsup:54.1-54.66
syntax funcinst = `%;%`(moduleinst, func)

;; 4-runtime.watsup:55.1-55.53
syntax globalinst = val

;; 4-runtime.watsup:56.1-56.52
syntax tableinst = ref*

;; 4-runtime.watsup:57.1-57.52
syntax meminst = byte*

;; 4-runtime.watsup:58.1-58.53
syntax eleminst = ref*

;; 4-runtime.watsup:59.1-59.51
syntax datainst = byte*

;; 4-runtime.watsup:62.1-68.21
syntax store = {FUNC funcinst*, GLOBAL globalinst*, TABLE tableinst*, MEM meminst*, ELEM eleminst*, DATA datainst*}

;; 4-runtime.watsup:79.1-81.24
syntax frame = {LOCAL val*, MODULE moduleinst}

;; 4-runtime.watsup:82.1-82.47
syntax state = `%;%`(store, frame)

;; 4-runtime.watsup:130.1-137.5
rec {

;; 4-runtime.watsup:130.1-137.5
syntax admininstr =
  | instr
  | REF.FUNC_ADDR(funcaddr)
  | REF.HOST_ADDR(hostaddr)
  | CALL_ADDR(funcaddr)
  | LABEL_(n, instr*, admininstr*)
  | FRAME_(n, frame, admininstr*)
  | TRAP
}

;; 4-runtime.watsup:83.1-83.62
syntax config = `%;%`(state, admininstr*)

;; 4-runtime.watsup:98.1-98.59
def funcaddr : state -> funcaddr*
  ;; 4-runtime.watsup:99.1-99.38
  def {f : frame, s : store} funcaddr(`%;%`(s, f)) = f.MODULE.FUNC

;; 4-runtime.watsup:101.1-101.52
def funcinst : state -> funcinst*
  ;; 4-runtime.watsup:102.1-102.31
  def {f : frame, s : store} funcinst(`%;%`(s, f)) = s.FUNC

;; 4-runtime.watsup:104.1-104.61
def func : (state, funcidx) -> funcinst
  ;; 4-runtime.watsup:105.1-105.48
  def {f : frame, s : store, x : idx} func(`%;%`(s, f), x) = s.FUNC[f.MODULE.FUNC[x]]

;; 4-runtime.watsup:107.1-107.59
def local : (state, localidx) -> val
  ;; 4-runtime.watsup:108.1-108.35
  def {f : frame, s : store, x : idx} local(`%;%`(s, f), x) = f.LOCAL[x]

;; 4-runtime.watsup:110.1-110.69
def global : (state, globalidx) -> globalinst
  ;; 4-runtime.watsup:111.1-111.54
  def {f : frame, s : store, x : idx} global(`%;%`(s, f), x) = s.GLOBAL[f.MODULE.GLOBAL[x]]

;; 4-runtime.watsup:113.1-113.65
def table : (state, tableidx) -> tableinst
  ;; 4-runtime.watsup:114.1-114.51
  def {f : frame, s : store, x : idx} table(`%;%`(s, f), x) = s.TABLE[f.MODULE.TABLE[x]]

;; 4-runtime.watsup:116.1-116.62
def elem : (state, tableidx) -> eleminst
  ;; 4-runtime.watsup:117.1-117.48
  def {f : frame, s : store, x : idx} elem(`%;%`(s, f), x) = s.ELEM[f.MODULE.ELEM[x]]

;; 4-runtime.watsup:119.1-119.76
def with_local : (state, localidx, val) -> state
  ;; 4-runtime.watsup:120.1-120.50
  def {f : frame, s : store, v : val, x : idx} with_local(`%;%`(s, f), x, v) = `%;%`(s, f[LOCAL[x] = v])

;; 4-runtime.watsup:122.1-122.79
def with_global : (state, globalidx, val) -> state
  ;; 4-runtime.watsup:123.1-123.69
  def {f : frame, s : store, v : val, x : idx} with_global(`%;%`(s, f), x, v) = `%;%`(s[GLOBAL[f.MODULE.GLOBAL[x]] = v], f)

;; 4-runtime.watsup:125.1-125.84
def with_table : (state, tableidx, n, ref) -> state
  ;; 4-runtime.watsup:126.1-126.72
  def {f : frame, i : nat, r : ref, s : store, x : idx} with_table(`%;%`(s, f), x, i, r) = `%;%`(s[TABLE[f.MODULE.TABLE[x]][i] = r], f)

;; 4-runtime.watsup:139.1-142.21
rec {

;; 4-runtime.watsup:139.1-142.21
syntax E =
  | _HOLE
  | _SEQ(val*, E, instr*)
  | LABEL_(n, instr*, E)
}

;; 5-reduction.watsup:4.1-4.63
relation Step_pure: `%~>%`(admininstr*, admininstr*)
  ;; 5-reduction.watsup:86.1-88.19
  rule br_table-ge {i : nat, l : labelidx, l' : labelidx}:
    `%~>%`([CONST_admininstr(I32_numtype, i) BR_TABLE_admininstr(l*, l')], [BR_admininstr(l')])
    -- iff (i >= |l*|)

  ;; 5-reduction.watsup:82.1-84.18
  rule br_table-lt {i : nat, l : labelidx, l' : labelidx}:
    `%~>%`([CONST_admininstr(I32_numtype, i) BR_TABLE_admininstr(l*, l')], [BR_admininstr(l*[i])])
    -- iff (i < |l*|)

  ;; 5-reduction.watsup:77.1-79.15
  rule br_if-false {c : c_numtype, l : labelidx}:
    `%~>%`([CONST_admininstr(I32_numtype, c) BR_IF_admininstr(l)], [])
    -- iff (c = 0)

  ;; 5-reduction.watsup:73.1-75.17
  rule br_if-true {c : c_numtype, l : labelidx}:
    `%~>%`([CONST_admininstr(I32_numtype, c) BR_IF_admininstr(l)], [BR_admininstr(l)])
    -- iff (c =/= 0)

  ;; 5-reduction.watsup:69.1-70.65
  rule br-succ {instr : instr, instr' : instr, l : labelidx, n : n, val : val}:
    `%~>%`([LABEL__admininstr(n, instr'*, (val <: admininstr)* :: [BR_admininstr(l + 1)] :: (instr <: admininstr)*)], (val <: admininstr)* :: [BR_admininstr(l)])

  ;; 5-reduction.watsup:66.1-67.69
  rule br-zero {instr : instr, instr' : instr, n : n, val : val, val' : val}:
    `%~>%`([LABEL__admininstr(n, instr'*, (val' <: admininstr)* :: (val <: admininstr)^n :: [BR_admininstr(0)] :: (instr <: admininstr)*)], (val <: admininstr)^n :: (instr' <: admininstr)*)

  ;; 5-reduction.watsup:61.1-63.15
  rule if-false {bt : blocktype, c : c_numtype, instr_1 : instr, instr_2 : instr}:
    `%~>%`([CONST_admininstr(I32_numtype, c) IF_admininstr(bt, instr_1*, instr_2*)], [BLOCK_admininstr(bt, instr_2*)])
    -- iff (c = 0)

  ;; 5-reduction.watsup:57.1-59.17
  rule if-true {bt : blocktype, c : c_numtype, instr_1 : instr, instr_2 : instr}:
    `%~>%`([CONST_admininstr(I32_numtype, c) IF_admininstr(bt, instr_1*, instr_2*)], [BLOCK_admininstr(bt, instr_1*)])
    -- iff (c =/= 0)

  ;; 5-reduction.watsup:53.1-55.29
  rule loop {bt : blocktype, instr : instr, k : nat, n : n, t_1 : valtype, t_2 : valtype, val : val}:
    `%~>%`((val <: admininstr)^k :: [LOOP_admininstr(bt, instr*)], [LABEL__admininstr(n, [LOOP_instr(bt, instr*)], (val <: admininstr)^k :: (instr <: admininstr)*)])
    -- iff (bt = `%->%`(t_1^k, t_2^n))

  ;; 5-reduction.watsup:49.1-51.29
  rule block {bt : blocktype, instr : instr, k : nat, n : n, t_1 : valtype, t_2 : valtype, val : val}:
    `%~>%`((val <: admininstr)^k :: [BLOCK_admininstr(bt, instr*)], [LABEL__admininstr(n, [], (val <: admininstr)^k :: (instr <: admininstr)*)])
    -- iff (bt = `%->%`(t_1^k, t_2^n))

  ;; 5-reduction.watsup:46.1-47.47
  rule local.tee {val : val, x : idx}:
    `%~>%`([(val <: admininstr) LOCAL.TEE_admininstr(x)], [(val <: admininstr) (val <: admininstr) LOCAL.SET_admininstr(x)])

  ;; 5-reduction.watsup:42.1-44.15
  rule select-false {c : c_numtype, t : valtype, val_1 : val, val_2 : val}:
    `%~>%`([(val_1 <: admininstr) (val_2 <: admininstr) CONST_admininstr(I32_numtype, c) SELECT_admininstr(t?)], [(val_2 <: admininstr)])
    -- iff (c = 0)

  ;; 5-reduction.watsup:38.1-40.17
  rule select-true {c : c_numtype, t : valtype, val_1 : val, val_2 : val}:
    `%~>%`([(val_1 <: admininstr) (val_2 <: admininstr) CONST_admininstr(I32_numtype, c) SELECT_admininstr(t?)], [(val_1 <: admininstr)])
    -- iff (c =/= 0)

  ;; 5-reduction.watsup:35.1-36.24
  rule drop {val : val}:
    `%~>%`([(val <: admininstr) DROP_admininstr], [])

  ;; 5-reduction.watsup:32.1-33.19
  rule nop:
    `%~>%`([NOP_admininstr], [])

  ;; 5-reduction.watsup:29.1-30.24
  rule unreachable:
    `%~>%`([UNREACHABLE_admininstr], [TRAP_admininstr])

  ;; 5-reduction.watsup:25.1-27.15
  rule ref.is_null-false {val : val}:
    `%~>%`([(val <: admininstr) REF.IS_NULL_admininstr], [CONST_admininstr(I32_numtype, 0)])
    -- otherwise

  ;; 5-reduction.watsup:21.1-23.27
  rule ref.is_null-true {rt : reftype, val : val}:
    `%~>%`([(val <: admininstr) REF.IS_NULL_admininstr], [CONST_admininstr(I32_numtype, 1)])
    -- iff (val = REF.NULL_val(rt))

;; 5-reduction.watsup:5.1-5.63
relation Step_read: `%~>%`(config, admininstr*)
  ;; 5-reduction.watsup:165.1-167.62
  rule call_addr {a : addr, instr : instr, k : nat, m : moduleinst, n : n, t : valtype, t_1 : valtype, t_2 : valtype, val : val, z : state}:
    `%~>%`(`%;%`(z, (val <: admininstr)^k :: [CALL_ADDR_admininstr(a)]), [FRAME__admininstr(n, {LOCAL val^k :: $default_(t)*, MODULE m}, [LABEL__admininstr(n, [], (instr <: admininstr)*)])])
    -- iff ($funcinst(z)[a] = `%;%`(m, FUNC(`%->%`(t_1^k, t_2^n), t*, instr*)))

  ;; 5-reduction.watsup:161.1-163.15
  rule call_indirect-trap {ft : functype, i : nat, x : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, i) CALL_INDIRECT_admininstr(x, ft)]), [TRAP_admininstr])
    -- otherwise

  ;; 5-reduction.watsup:156.1-159.35
  rule call_indirect-call {a : addr, ft : functype, func : func, i : nat, m : moduleinst, x : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, i) CALL_INDIRECT_admininstr(x, ft)]), [CALL_ADDR_admininstr(a)])
    -- iff ($table(z, x)[i] = REF.FUNC_ADDR_ref(a))
    -- iff ($funcinst(z)[a] = `%;%`(m, func))

  ;; 5-reduction.watsup:153.1-154.47
  rule call {x : idx, z : state}:
    `%~>%`(`%;%`(z, [CALL_admininstr(x)]), [CALL_ADDR_admininstr($funcaddr(z)[x])])

  ;; 5-reduction.watsup:147.1-151.15
  rule table.init-le {i : nat, j : nat, n : n, x : idx, y : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, j) CONST_admininstr(I32_numtype, i) CONST_admininstr(I32_numtype, (n + 1)) TABLE.INIT_admininstr(x, y)]), [CONST_admininstr(I32_numtype, j) ($elem(z, y)[i] <: admininstr) TABLE.SET_admininstr(x) CONST_admininstr(I32_numtype, (j + 1)) CONST_admininstr(I32_numtype, (i + 1)) CONST_admininstr(I32_numtype, n) TABLE.INIT_admininstr(x, y)])
    -- otherwise

  ;; 5-reduction.watsup:144.1-146.15
  rule table.init-zero {i : nat, j : nat, x : idx, y : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, j) CONST_admininstr(I32_numtype, i) CONST_admininstr(I32_numtype, 0) TABLE.INIT_admininstr(x, y)]), [])
    -- otherwise

  ;; 5-reduction.watsup:141.1-143.63
  rule table.init-trap {i : nat, j : nat, n : n, x : idx, y : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, j) CONST_admininstr(I32_numtype, i) CONST_admininstr(I32_numtype, n) TABLE.INIT_admininstr(x, y)]), [TRAP_admininstr])
    -- iff (((i + n) > |$elem(z, y)|) \/ ((j + n) > |$table(z, x)|))

  ;; 5-reduction.watsup:134.1-139.15
  rule table.copy-gt {i : nat, j : nat, n : n, x : idx, y : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, j) CONST_admininstr(I32_numtype, i) CONST_admininstr(I32_numtype, (n + 1)) TABLE.COPY_admininstr(x, y)]), [CONST_admininstr(I32_numtype, (j + n)) CONST_admininstr(I32_numtype, (i + n)) TABLE.GET_admininstr(y) TABLE.SET_admininstr(x) CONST_admininstr(I32_numtype, (j + 1)) CONST_admininstr(I32_numtype, (i + 1)) CONST_admininstr(I32_numtype, n) TABLE.COPY_admininstr(x, y)])
    -- iff (i > i)

  ;; 5-reduction.watsup:128.1-133.16
  rule table.copy-le {i : nat, j : nat, n : n, x : idx, y : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, j) CONST_admininstr(I32_numtype, i) CONST_admininstr(I32_numtype, (n + 1)) TABLE.COPY_admininstr(x, y)]), [CONST_admininstr(I32_numtype, j) CONST_admininstr(I32_numtype, i) TABLE.GET_admininstr(y) TABLE.SET_admininstr(x) CONST_admininstr(I32_numtype, (j + 1)) CONST_admininstr(I32_numtype, (i + 1)) CONST_admininstr(I32_numtype, n) TABLE.COPY_admininstr(x, y)])
    -- iff (j <= i)

  ;; 5-reduction.watsup:125.1-127.15
  rule table.copy-zero {i : nat, j : nat, x : idx, y : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, j) CONST_admininstr(I32_numtype, i) CONST_admininstr(I32_numtype, 0) TABLE.COPY_admininstr(x, y)]), [])
    -- otherwise

  ;; 5-reduction.watsup:122.1-124.64
  rule table.copy-trap {i : nat, j : nat, n : n, x : idx, y : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, j) CONST_admininstr(I32_numtype, i) CONST_admininstr(I32_numtype, n) TABLE.COPY_admininstr(x, y)]), [TRAP_admininstr])
    -- iff (((i + n) > |$table(z, y)|) \/ ((j + n) > |$table(z, x)|))

  ;; 5-reduction.watsup:117.1-120.15
  rule table.fill-succ {i : nat, n : n, val : val, x : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, i) (val <: admininstr) CONST_admininstr(I32_numtype, (n + 1)) TABLE.FILL_admininstr(x)]), [CONST_admininstr(I32_numtype, i) (val <: admininstr) TABLE.SET_admininstr(x) CONST_admininstr(I32_numtype, (i + 1)) (val <: admininstr) CONST_admininstr(I32_numtype, n) TABLE.FILL_admininstr(x)])
    -- otherwise

  ;; 5-reduction.watsup:114.1-116.15
  rule table.fill-zero {i : nat, val : val, x : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, i) (val <: admininstr) CONST_admininstr(I32_numtype, 0) TABLE.FILL_admininstr(x)]), [])
    -- otherwise

  ;; 5-reduction.watsup:111.1-113.35
  rule table.fill-trap {i : nat, n : n, val : val, x : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, i) (val <: admininstr) CONST_admininstr(I32_numtype, n) TABLE.FILL_admininstr(x)]), [TRAP_admininstr])
    -- iff ((i + n) > |$table(z, x)|)

  ;; 5-reduction.watsup:107.1-109.28
  rule table.size {n : n, x : idx, z : state}:
    `%~>%`(`%;%`(z, [TABLE.SIZE_admininstr(x)]), [CONST_admininstr(I32_numtype, n)])
    -- iff (|$table(z, x)| = n)

  ;; 5-reduction.watsup:103.1-105.28
  rule table.get-lt {i : nat, x : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, i) TABLE.GET_admininstr(x)]), [($table(z, x)[i] <: admininstr)])
    -- iff (i < |$table(z, x)|)

  ;; 5-reduction.watsup:99.1-101.29
  rule table.get-ge {i : nat, x : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, i) TABLE.GET_admininstr(x)]), [TRAP_admininstr])
    -- iff (i >= |$table(z, x)|)

  ;; 5-reduction.watsup:96.1-97.37
  rule global.get {x : idx, z : state}:
    `%~>%`(`%;%`(z, [GLOBAL.GET_admininstr(x)]), [($global(z, x) <: admininstr)])

  ;; 5-reduction.watsup:93.1-94.35
  rule local.get {x : idx, z : state}:
    `%~>%`(`%;%`(z, [LOCAL.GET_admininstr(x)]), [($local(z, x) <: admininstr)])

  ;; 5-reduction.watsup:90.1-91.53
  rule ref.func {x : idx, z : state}:
    `%~>%`(`%;%`(z, [REF.FUNC_admininstr(x)]), [REF.FUNC_ADDR_admininstr($funcaddr(z)[x])])

;; 5-reduction.watsup:6.1-6.63
relation Step_write: `%~>%`(config, config)
  ;; 5-reduction.watsup:179.1-181.29
  rule table.set-ge {i : nat, ref : ref, x : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, i) (ref <: admininstr) TABLE.GET_admininstr(x)]), `%;%`(z, [TRAP_admininstr]))
    -- iff (i >= |$table(z, x)|)

  ;; 5-reduction.watsup:175.1-177.28
  rule table.set-lt {i : nat, ref : ref, x : idx, z : state}:
    `%~>%`(`%;%`(z, [CONST_admininstr(I32_numtype, i) (ref <: admininstr) TABLE.GET_admininstr(x)]), `%;%`($with_table(z, x, i, ref), []))
    -- iff (i < |$table(z, x)|)

  ;; 5-reduction.watsup:172.1-173.62
  rule global.set {val : val, x : idx, z : state}:
    `%~>%`(`%;%`(z, [(val <: admininstr) GLOBAL.SET_admininstr(x)]), `%;%`($with_global(z, x, val), []))

  ;; 5-reduction.watsup:169.1-170.60
  rule local.set {val : val, x : idx, z : state}:
    `%~>%`(`%;%`(z, [(val <: admininstr) LOCAL.SET_admininstr(x)]), `%;%`($with_local(z, x, val), []))

;; 5-reduction.watsup:3.1-3.63
relation Step: `%~>%`(config, config)
  ;; 5-reduction.watsup:16.1-18.42
  rule write {instr : instr, instr' : instr, z : state, z' : state}:
    `%~>%`(`%;%`(z, (instr <: admininstr)*), `%;%`(z', (instr' <: admininstr)*))
    -- Step_write: `%~>%`(`%;%`(z, (instr <: admininstr)*), `%;%`(z', (instr' <: admininstr)*))

  ;; 5-reduction.watsup:12.1-14.37
  rule read {instr : instr, instr' : instr, z : state}:
    `%~>%`(`%;%`(z, (instr <: admininstr)*), `%;%`(z, (instr' <: admininstr)*))
    -- Step_read: `%~>%`(`%;%`(z, (instr <: admininstr)*), (instr' <: admininstr)*)

  ;; 5-reduction.watsup:8.1-10.34
  rule pure {instr : instr, instr' : instr, z : state}:
    `%~>%`(`%;%`(z, (instr <: admininstr)*), `%;%`(z, (instr' <: admininstr)*))
    -- Step_pure: `%~>%`((instr <: admininstr)*, (instr' <: admininstr)*)

== IL Validation...
== Latex Generation...
$$
\begin{array}{@{}lrrl@{}}
& \mathit{n} &::=& \mathit{nat} \\
\end{array}
$$

$$
\begin{array}{@{}lrrl@{}}
\mbox{(name)} & \mathit{name} &::=& \mathit{text} \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}lrrl@{}}
\mbox{(byte)} & \mathit{byte} &::=& \mathit{nat} \\
\mbox{(32-bit integer)} & \mathit{u{\scriptstyle32}} &::=& \mathit{nat} \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}lrrl@{}}
\mbox{(index)} & \mathit{idx} &::=& \mathit{nat} \\
\mbox{(function index)} & \mathit{funcidx} &::=& \mathit{idx} \\
\mbox{(global index)} & \mathit{globalidx} &::=& \mathit{idx} \\
\mbox{(table index)} & \mathit{tableidx} &::=& \mathit{idx} \\
\mbox{(memory index)} & \mathit{memidx} &::=& \mathit{idx} \\
\mbox{(elem index)} & \mathit{elemidx} &::=& \mathit{idx} \\
\mbox{(data index)} & \mathit{dataidx} &::=& \mathit{idx} \\
\mbox{(label index)} & \mathit{labelidx} &::=& \mathit{idx} \\
\mbox{(local index)} & \mathit{localidx} &::=& \mathit{idx} \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}lrrl@{}}
\mbox{(number type)} & \mathit{numtype} &::=& \mathsf{i{\scriptstyle32}} ~|~ \mathsf{i{\scriptstyle64}} ~|~ \mathsf{f{\scriptstyle32}} ~|~ \mathsf{f{\scriptstyle64}} \\
\mbox{(vector type)} & \mathit{vectype} &::=& \mathsf{v{\scriptstyle128}} \\
\mbox{(reference type)} & \mathit{reftype} &::=& \mathsf{funcref} ~|~ \mathsf{externref} \\
\mbox{(value type)} & \mathit{valtype} &::=& \mathit{numtype} ~|~ \mathit{vectype} ~|~ \mathit{reftype} ~|~ \mathsf{bot} \\
& {\mathsf{i}}{\mathit{n}} &::=& \mathsf{i{\scriptstyle32}} ~|~ \mathsf{i{\scriptstyle64}} \\
& {\mathsf{f}}{\mathit{n}} &::=& \mathsf{f{\scriptstyle32}} ~|~ \mathsf{f{\scriptstyle64}} \\
\end{array}
$$

$$
\begin{array}{@{}lrrl@{}}
\mbox{(result type)} & \mathit{resulttype} &::=& {\mathit{valtype}^\ast} \\
\mbox{(limits)} & \mathit{limits} &::=& [\mathit{u{\scriptstyle32}} .. \mathit{u{\scriptstyle32}}] \\
\mbox{(global type)} & \mathit{globaltype} &::=& {\mathsf{mut}^?}~\mathit{valtype} \\
\mbox{(function type)} & \mathit{functype} &::=& \mathit{resulttype} \rightarrow \mathit{resulttype} \\
\mbox{(table type)} & \mathit{tabletype} &::=& \mathit{limits}~\mathit{reftype} \\
\mbox{(memory type)} & \mathit{memtype} &::=& \mathit{limits}~\mathsf{i{\scriptstyle8}} \\
\mbox{(element type)} & \mathit{elemtype} &::=& \mathit{reftype} \\
\mbox{(data type)} & \mathit{datatype} &::=& \mathsf{ok} \\
\mbox{(external type)} & \mathit{externtype} &::=& \mathsf{global}~\mathit{globaltype} ~|~ \mathsf{func}~\mathit{functype} ~|~ \mathsf{table}~\mathit{tabletype} ~|~ \mathsf{memory}~\mathit{memtype} \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}lrrl@{}}
\mbox{(signedness)} & \mathit{sx} &::=& \mathsf{u} ~|~ \mathsf{s} \\
& \mathit{unop}_{\mathsf{ixx}} &::=& \mathsf{clz} ~|~ \mathsf{ctz} ~|~ \mathsf{popcnt} \\
& \mathit{unop}_{\mathsf{fxx}} &::=& \mathsf{abs} ~|~ \mathsf{neg} ~|~ \mathsf{sqrt} ~|~ \mathsf{ceil} ~|~ \mathsf{floor} ~|~ \mathsf{trunc} ~|~ \mathsf{nearest} \\
& \mathit{binop}_{\mathsf{ixx}} &::=& \mathsf{add} ~|~ \mathsf{sub} ~|~ \mathsf{mul} ~|~ {\mathsf{div\_}}{\mathsf{\mathit{sx}}} ~|~ {\mathsf{rem\_}}{\mathsf{\mathit{sx}}} \\ &&|&
\mathsf{and} ~|~ \mathsf{or} ~|~ \mathsf{xor} ~|~ \mathsf{shl} ~|~ {\mathsf{shr\_}}{\mathsf{\mathit{sx}}} ~|~ \mathsf{rotl} ~|~ \mathsf{rotr} \\
& \mathit{binop}_{\mathsf{fxx}} &::=& \mathsf{add} ~|~ \mathsf{sub} ~|~ \mathsf{mul} ~|~ \mathsf{div} ~|~ \mathsf{min} ~|~ \mathsf{max} ~|~ \mathsf{copysign} \\
& \mathit{testop}_{\mathsf{ixx}} &::=& \mathsf{eqz} \\
& \mathit{testop}_{\mathsf{fxx}} &::=&  \\
& \mathit{relop}_{\mathsf{ixx}} &::=& \mathsf{eq} ~|~ \mathsf{ne} ~|~ {\mathsf{lt\_}}{\mathsf{\mathit{sx}}} ~|~ {\mathsf{gt\_}}{\mathsf{\mathit{sx}}} ~|~ {\mathsf{le\_}}{\mathsf{\mathit{sx}}} ~|~ {\mathsf{ge\_}}{\mathsf{\mathit{sx}}} \\
& \mathit{relop}_{\mathsf{fxx}} &::=& \mathsf{eq} ~|~ \mathsf{ne} ~|~ \mathsf{lt} ~|~ \mathsf{gt} ~|~ \mathsf{le} ~|~ \mathsf{ge} \\
& \mathit{unop}_{\mathit{numtype}} &::=& \mathit{unop}_{\mathsf{ixx}} ~|~ \mathit{unop}_{\mathsf{fxx}} \\
& \mathit{binop}_{\mathit{numtype}} &::=& \mathit{binop}_{\mathsf{ixx}} ~|~ \mathit{binop}_{\mathsf{fxx}} \\
& \mathit{testop}_{\mathit{numtype}} &::=& \mathit{testop}_{\mathsf{ixx}} ~|~ \mathit{testop}_{\mathsf{fxx}} \\
& \mathit{relop}_{\mathit{numtype}} &::=& \mathit{relop}_{\mathsf{ixx}} ~|~ \mathit{relop}_{\mathsf{fxx}} \\
& \mathit{cvtop} &::=& \mathsf{convert} ~|~ \mathsf{reinterpret} \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}lrrl@{}}
& \mathit{c}_{\mathit{numtype}} &::=& \mathit{nat} \\
& \mathit{c}_{\mathit{vectype}} &::=& \mathit{nat} \\
\end{array}
$$

$$
\begin{array}{@{}lrrl@{}}
\mbox{(block type)} & \mathit{blocktype} &::=& \mathit{functype} \\
\end{array}
$$

$$
\begin{array}{@{}lrrl@{}}
& \mathit{instr} &::=& \mathsf{unreachable} \\ &&|&
\mathsf{nop} \\ &&|&
\mathsf{drop} \\ &&|&
\mathsf{select}~{\mathit{valtype}^?} \\ &&|&
\mathsf{block}~\mathit{blocktype}~{\mathit{instr}^\ast} \\ &&|&
\mathsf{loop}~\mathit{blocktype}~{\mathit{instr}^\ast} \\ &&|&
\mathsf{if}~\mathit{blocktype}~{\mathit{instr}^\ast}~\mathsf{else}~{\mathit{instr}^\ast} \\ &&|&
\mathsf{br}~\mathit{labelidx} \\ &&|&
\mathsf{br\_if}~\mathit{labelidx} \\ &&|&
\mathsf{br\_table}~{\mathit{labelidx}^\ast}~\mathit{labelidx} \\ &&|&
\mathsf{call}~\mathit{funcidx} \\ &&|&
\mathsf{call\_indirect}~\mathit{tableidx}~\mathit{functype} \\ &&|&
\mathsf{return} \\ &&|&
\mathsf{\mathit{numtype}}.\mathsf{const}~\mathsf{\mathit{c}\_{\mathit{numtype}}} \\ &&|&
\mathsf{\mathit{numtype}} . \mathsf{\mathit{unop}\_{\mathit{numtype}}} \\ &&|&
\mathsf{\mathit{numtype}} . \mathsf{\mathit{binop}\_{\mathit{numtype}}} \\ &&|&
\mathsf{\mathit{numtype}} . \mathsf{\mathit{testop}\_{\mathit{numtype}}} \\ &&|&
\mathsf{\mathit{numtype}} . \mathsf{\mathit{relop}\_{\mathit{numtype}}} \\ &&|&
{\mathsf{\mathit{numtype}}.\mathsf{extend}}{\mathsf{\mathit{n}}} \\ &&|&
\mathsf{\mathit{numtype}} . {{{{\mathsf{\mathit{cvtop}}}{\mathsf{\_}}}{\mathsf{\mathit{numtype}}}}{\mathsf{\_}}}{\mathsf{{\mathit{sx}^?}}} \\ &&|&
\mathsf{ref.null}~\mathit{reftype} \\ &&|&
\mathsf{ref.func}~\mathit{funcidx} \\ &&|&
\mathsf{ref.is\_null} \\ &&|&
\mathsf{local.get}~\mathit{localidx} \\ &&|&
\mathsf{local.set}~\mathit{localidx} \\ &&|&
\mathsf{local.tee}~\mathit{localidx} \\ &&|&
\mathsf{global.get}~\mathit{globalidx} \\ &&|&
\mathsf{global.set}~\mathit{globalidx} \\ &&|&
\mathsf{table.get}~\mathit{tableidx} \\ &&|&
\mathsf{table.set}~\mathit{tableidx} \\ &&|&
\mathsf{table.size}~\mathit{tableidx} \\ &&|&
\mathsf{table.grow}~\mathit{tableidx} \\ &&|&
\mathsf{table.fill}~\mathit{tableidx} \\ &&|&
\mathsf{table.copy}~\mathit{tableidx}~\mathit{tableidx} \\ &&|&
\mathsf{table.init}~\mathit{tableidx}~\mathit{elemidx} \\ &&|&
\mathsf{elem.drop}~\mathit{elemidx} \\ &&|&
\mathsf{memory.size} \\ &&|&
\mathsf{memory.grow} \\ &&|&
\mathsf{memory.fill} \\ &&|&
\mathsf{memory.copy} \\ &&|&
\mathsf{memory.init}~\mathit{dataidx} \\ &&|&
\mathsf{data.drop}~\mathit{dataidx} \\ &&|&
{\mathsf{\mathit{numtype}}.\mathsf{load}}{\mathsf{{(\mathit{n}~\mathit{sx})^?}}~\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}} \\ &&|&
{\mathsf{\mathit{numtype}}.\mathsf{store}}{\mathsf{{\mathit{n}^?}}~\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}} \\
\mbox{(expression)} & \mathit{expr} &::=& {\mathit{instr}^\ast} \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}lrrl@{}}
& \mathit{elemmode} &::=& \mathsf{table}~\mathit{tableidx}~\mathit{expr} ~|~ \mathsf{declare} \\
& \mathit{datamode} &::=& \mathsf{memory}~\mathit{memidx}~\mathit{expr} \\
\mbox{(function)} & \mathit{func} &::=& \mathsf{func}~\mathit{functype}~{\mathit{valtype}^\ast}~\mathit{expr} \\
\mbox{(global)} & \mathit{global} &::=& \mathsf{global}~\mathit{globaltype}~\mathit{expr} \\
\mbox{(table)} & \mathit{table} &::=& \mathsf{table}~\mathit{tabletype} \\
\mbox{(memory)} & \mathit{mem} &::=& \mathsf{memory}~\mathit{memtype} \\
\mbox{(table segment)} & \mathit{elem} &::=& \mathsf{elem}~\mathit{reftype}~{\mathit{expr}^\ast}~{\mathit{elemmode}^?} \\
\mbox{(memory segment)} & \mathit{data} &::=& \mathsf{data}~{({\mathit{byte}^\ast})^\ast}~{\mathit{datamode}^?} \\
\mbox{(start function)} & \mathit{start} &::=& \mathsf{start}~\mathit{funcidx} \\
\mbox{(external use)} & \mathit{externuse} &::=& \mathsf{func}~\mathit{funcidx} ~|~ \mathsf{global}~\mathit{globalidx} ~|~ \mathsf{table}~\mathit{tableidx} ~|~ \mathsf{memory}~\mathit{memidx} \\
\mbox{(export)} & \mathit{export} &::=& \mathsf{export}~\mathit{name}~\mathit{externuse} \\
\mbox{(import)} & \mathit{import} &::=& \mathsf{import}~\mathit{name}~\mathit{name}~\mathit{externtype} \\
\mbox{(module)} & \mathit{module} &::=& \mathsf{module}~{\mathit{import}^\ast}~{\mathit{func}^\ast}~{\mathit{global}^\ast}~{\mathit{table}^\ast}~{\mathit{mem}^\ast}~{\mathit{elem}^\ast}~{\mathit{data}^\ast}~{\mathit{start}^\ast}~{\mathit{export}^\ast} \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{|\mathsf{i{\scriptstyle32}}|} &=& 32 &  \\
{|\mathsf{i{\scriptstyle64}}|} &=& 64 &  \\
{|\mathsf{f{\scriptstyle32}}|} &=& 32 &  \\
{|\mathsf{f{\scriptstyle64}}|} &=& 64 &  \\
{|\mathsf{v{\scriptstyle128}}|} &=& 128 &  \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}lcl@{}l@{}}
\mathrm{test}_{\mathit{sub}_{\mathsf{atom}_{22}}}(\mathit{n}_{3_{\mathsf{atom}_{\mathit{y}}}}) &=& 0 &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{\mathrm{curried}}_{\mathit{n}_{1}}(\mathit{n}_{2}) &=& \mathit{n}_{1} + \mathit{n}_{2} &  \\
\end{array}
$$

$$
\begin{array}{@{}lrrl@{}}
& \mathit{testfuse} &::=& {\mathsf{ab}}_{\mathit{nat}}\,\,\mathit{nat}~\mathit{nat} \\ &&|&
{\mathsf{cd}}_{\mathsf{\mathit{nat}}}\,\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}} \\ &&|&
{\mathsf{ef\_}}{\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}} \\ &&|&
{{\mathsf{gh}}_{\mathsf{\mathit{nat}}}}{\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}} \\ &&|&
{{\mathsf{ij}}_{\mathsf{\mathit{nat}}}}{\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}} \\ &&|&
{\mathsf{kl\_ab}}{\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}} \\ &&|&
{\mathsf{mn\_}}{\mathsf{ab}~\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}} \\ &&|&
{{\mathsf{op\_}}{\mathsf{ab}}}{\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}} \\ &&|&
{{\mathsf{qr}}_{\mathsf{\mathit{nat}}}}{\mathsf{ab}~\mathsf{\mathit{nat}}~\mathsf{\mathit{nat}}} \\
\mbox{(context)} & \mathit{context} &::=& \{\; \begin{array}[t]{@{}l@{}}
\mathsf{func}~{\mathit{functype}^\ast},\; \mathsf{global}~{\mathit{globaltype}^\ast},\; \mathsf{table}~{\mathit{tabletype}^\ast},\; \mathsf{mem}~{\mathit{memtype}^\ast},\; \\
  \mathsf{elem}~{\mathit{elemtype}^\ast},\; \mathsf{data}~{\mathit{datatype}^\ast},\; \\
  \mathsf{local}~{\mathit{valtype}^\ast},\; \mathsf{label}~{\mathit{resulttype}^\ast},\; \mathsf{return}~{\mathit{resulttype}^?} \;\}\end{array} \\
\end{array}
$$

\vspace{1ex}

$\boxed{{ \vdash }\;\mathit{limits} : \mathit{nat}}$

$\boxed{{ \vdash }\;\mathit{functype} : \mathsf{ok}}$

$\boxed{{ \vdash }\;\mathit{globaltype} : \mathsf{ok}}$

$\boxed{{ \vdash }\;\mathit{tabletype} : \mathsf{ok}}$

$\boxed{{ \vdash }\;\mathit{memtype} : \mathsf{ok}}$

$\boxed{{ \vdash }\;\mathit{externtype} : \mathsf{ok}}$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{n}_{1} \leq \mathit{n}_{2} \leq \mathit{k}
}{
{ \vdash }\;[\mathit{n}_{1} .. \mathit{n}_{2}] : \mathit{k}
} \, {[\textsc{\scriptsize K{-}limits}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
{ \vdash }\;\mathit{ft} : \mathsf{ok}
} \, {[\textsc{\scriptsize K{-}func}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
{ \vdash }\;\mathit{gt} : \mathsf{ok}
} \, {[\textsc{\scriptsize K{-}global}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{lim} : {2^{32}} - 1
}{
{ \vdash }\;\mathit{lim}~\mathit{rt} : \mathsf{ok}
} \, {[\textsc{\scriptsize K{-}table}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{lim} : {2^{16}}
}{
{ \vdash }\;\mathit{lim}~\mathsf{i{\scriptstyle8}} : \mathsf{ok}
} \, {[\textsc{\scriptsize K{-}mem}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{functype} : \mathsf{ok}
}{
{ \vdash }\;\mathsf{func}~\mathit{functype} : \mathsf{ok}
} \, {[\textsc{\scriptsize K{-}extern{-}func}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{globaltype} : \mathsf{ok}
}{
{ \vdash }\;\mathsf{global}~\mathit{globaltype} : \mathsf{ok}
} \, {[\textsc{\scriptsize K{-}extern{-}global}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{tabletype} : \mathsf{ok}
}{
{ \vdash }\;\mathsf{table}~\mathit{tabletype} : \mathsf{ok}
} \, {[\textsc{\scriptsize K{-}extern{-}table}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{memtype} : \mathsf{ok}
}{
{ \vdash }\;\mathsf{memory}~\mathit{memtype} : \mathsf{ok}
} \, {[\textsc{\scriptsize K{-}extern{-}mem}]}
\qquad
\end{array}
$$

\vspace{1ex}

$\boxed{{ \vdash }\;\mathit{valtype} \leq \mathit{valtype}}$

$\boxed{{ \vdash }\;{\mathit{valtype}^\ast} \leq {\mathit{valtype}^\ast}}$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
{ \vdash }\;\mathit{t} \leq \mathit{t}
} \, {[\textsc{\scriptsize S{-}refl}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
{ \vdash }\;\mathsf{bot} \leq \mathit{t}
} \, {[\textsc{\scriptsize S{-}bot}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
({ \vdash }\;\mathit{t}_{1} \leq \mathit{t}_{2})^\ast
}{
{ \vdash }\;{\mathit{t}_{1}^\ast} \leq {\mathit{t}_{2}^\ast}
} \, {[\textsc{\scriptsize S{-}result}]}
\qquad
\end{array}
$$

\vspace{1ex}

$\boxed{{ \vdash }\;\mathit{limits} \leq \mathit{limits}}$

$\boxed{{ \vdash }\;\mathit{functype} \leq \mathit{functype}}$

$\boxed{{ \vdash }\;\mathit{globaltype} \leq \mathit{globaltype}}$

$\boxed{{ \vdash }\;\mathit{tabletype} \leq \mathit{tabletype}}$

$\boxed{{ \vdash }\;\mathit{memtype} \leq \mathit{memtype}}$

$\boxed{{ \vdash }\;\mathit{externtype} \leq \mathit{externtype}}$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{n}_{11} \geq \mathit{n}_{21}
 \qquad
\mathit{n}_{12} \leq \mathit{n}_{22}
}{
{ \vdash }\;[\mathit{n}_{11} .. \mathit{n}_{12}] \leq [\mathit{n}_{21} .. \mathit{n}_{22}]
} \, {[\textsc{\scriptsize S{-}limits}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
{ \vdash }\;\mathit{ft} \leq \mathit{ft}
} \, {[\textsc{\scriptsize S{-}func}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
{ \vdash }\;\mathit{gt} \leq \mathit{gt}
} \, {[\textsc{\scriptsize S{-}global}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{lim}_{1} \leq \mathit{lim}_{2}
}{
{ \vdash }\;\mathit{lim}_{1}~\mathit{rt} \leq \mathit{lim}_{2}~\mathit{rt}
} \, {[\textsc{\scriptsize S{-}table}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{lim}_{1} \leq \mathit{lim}_{2}
}{
{ \vdash }\;\mathit{lim}_{1}~\mathsf{i{\scriptstyle8}} \leq \mathit{lim}_{2}~\mathsf{i{\scriptstyle8}}
} \, {[\textsc{\scriptsize S{-}mem}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{ft}_{1} \leq \mathit{ft}_{2}
}{
{ \vdash }\;\mathsf{func}~\mathit{ft}_{1} \leq \mathsf{func}~\mathit{ft}_{2}
} \, {[\textsc{\scriptsize S{-}extern{-}func}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{gt}_{1} \leq \mathit{gt}_{2}
}{
{ \vdash }\;\mathsf{global}~\mathit{gt}_{1} \leq \mathsf{global}~\mathit{gt}_{2}
} \, {[\textsc{\scriptsize S{-}extern{-}global}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{tt}_{1} \leq \mathit{tt}_{2}
}{
{ \vdash }\;\mathsf{table}~\mathit{tt}_{1} \leq \mathsf{table}~\mathit{tt}_{2}
} \, {[\textsc{\scriptsize S{-}extern{-}table}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{mt}_{1} \leq \mathit{mt}_{2}
}{
{ \vdash }\;\mathsf{memory}~\mathit{mt}_{1} \leq \mathsf{memory}~\mathit{mt}_{2}
} \, {[\textsc{\scriptsize S{-}extern{-}mem}]}
\qquad
\end{array}
$$

\vspace{1ex}

$\boxed{\mathit{context} \vdash \mathit{instr} : \mathit{functype}}$

$\boxed{\mathit{context} \vdash {\mathit{instr}^\ast} : \mathit{functype}}$

$\boxed{\mathit{context} \vdash \mathit{expr} : \mathit{resulttype}}$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C} \vdash {\mathit{instr}^\ast} : \epsilon \rightarrow {\mathit{t}^\ast}
}{
\mathit{C} \vdash {\mathit{instr}^\ast} : {\mathit{t}^\ast}
} \, {[\textsc{\scriptsize T{-}expr}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \epsilon : \epsilon \rightarrow \epsilon
} \, {[\textsc{\scriptsize T*{-}empty}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C} \vdash \mathit{instr}_{1} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
 \qquad
\mathit{C} \vdash \mathit{instr}_{2} : {\mathit{t}_{2}^\ast} \rightarrow {\mathit{t}_{3}^\ast}
}{
\mathit{C} \vdash \mathit{instr}_{1}~{\mathit{instr}_{2}^\ast} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{3}^\ast}
} \, {[\textsc{\scriptsize T*{-}seq}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\begin{array}{@{}c@{}}
\mathit{C} \vdash {\mathit{instr}^\ast} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
 \\
{ \vdash }\;{{\mathit{t}'}_{1}^\ast} \leq {\mathit{t}_{1}^\ast}
 \qquad
{ \vdash }\;{\mathit{t}_{2}^\ast} \leq {{\mathit{t}'}_{2}^\ast}
\end{array}
}{
\mathit{C} \vdash {\mathit{instr}^\ast} : {\mathit{t}'}_{1} \rightarrow {{\mathit{t}'}_{2}^\ast}
} \, {[\textsc{\scriptsize T*{-}weak}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C} \vdash {\mathit{instr}^\ast} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
}{
\mathit{C} \vdash {\mathit{instr}^\ast} : {\mathit{t}^\ast}~{\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}^\ast}~{\mathit{t}_{2}^\ast}
} \, {[\textsc{\scriptsize T*{-}frame}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathsf{unreachable} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
} \, {[\textsc{\scriptsize T{-}unreachable}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathsf{nop} : \epsilon \rightarrow \epsilon
} \, {[\textsc{\scriptsize T{-}nop}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathsf{drop} : \mathit{t} \rightarrow \epsilon
} \, {[\textsc{\scriptsize T{-}drop}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathsf{select}~\mathit{t} : \mathit{t}~\mathit{t}~\mathsf{i{\scriptstyle32}} \rightarrow \mathit{t}
} \, {[\textsc{\scriptsize T{-}select{-}expl}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{t} \leq {\mathit{t}'}
 \qquad
{\mathit{t}'} = \mathit{numtype} \lor {\mathit{t}'} = \mathit{vectype}
}{
\mathit{C} \vdash \mathsf{select} : \mathit{t}~\mathit{t}~\mathsf{i{\scriptstyle32}} \rightarrow \mathit{t}
} \, {[\textsc{\scriptsize T{-}select{-}impl}]}
\qquad
\end{array}
$$

\vspace{1ex}

$\boxed{\mathit{context} \vdash \mathit{blocktype} : \mathit{functype}}$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{ft} : \mathsf{ok}
}{
\mathit{C} \vdash \mathit{ft} : \mathit{ft}
} \, {[\textsc{\scriptsize K{-}block}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C} \vdash \mathit{bt} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
 \qquad
\mathit{C}, \mathsf{label}~{\mathit{t}_{2}^\ast} \vdash {\mathit{instr}^\ast} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
}{
\mathit{C} \vdash \mathsf{block}~\mathit{bt}~{\mathit{instr}^\ast} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
} \, {[\textsc{\scriptsize T{-}block}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C} \vdash \mathit{bt} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
 \qquad
\mathit{C}, \mathsf{label}~{\mathit{t}_{1}^\ast} \vdash {\mathit{instr}^\ast} : {\mathit{t}_{1}^\ast} \rightarrow \mathit{t}_{2}
}{
\mathit{C} \vdash \mathsf{loop}~\mathit{bt}~{\mathit{instr}^\ast} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
} \, {[\textsc{\scriptsize T{-}loop}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C} \vdash \mathit{bt} : {\mathit{t}_{1}^\ast} \rightarrow \mathit{t}_{2}
 \qquad
\mathit{C}, \mathsf{label}~{\mathit{t}_{2}^\ast} \vdash {\mathit{instr}_{1}^\ast} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
 \qquad
\mathit{C}, \mathsf{label}~{\mathit{t}_{2}^\ast} \vdash {\mathit{instr}_{2}^\ast} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
}{
\mathit{C} \vdash \mathsf{if}~\mathit{bt}~{\mathit{instr}_{1}^\ast}~\mathsf{else}~{\mathit{instr}_{2}^\ast} : {\mathit{t}_{1}^\ast} \rightarrow \mathit{t}_{2}
} \, {[\textsc{\scriptsize T{-}if}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{label}[\mathit{l}] = {\mathit{t}^\ast}
}{
\mathit{C} \vdash \mathsf{br}~\mathit{l} : {\mathit{t}_{1}^\ast}~{\mathit{t}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
} \, {[\textsc{\scriptsize T{-}br}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{label}[\mathit{l}] = {\mathit{t}^\ast}
}{
\mathit{C} \vdash \mathsf{br\_if}~\mathit{l} : {\mathit{t}^\ast}~\mathsf{i{\scriptstyle32}} \rightarrow {\mathit{t}^\ast}
} \, {[\textsc{\scriptsize T{-}br\_if}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
({ \vdash }\;{\mathit{t}^\ast} \leq \mathit{C}.\mathsf{label}[\mathit{l}])^\ast
 \qquad
{ \vdash }\;{\mathit{t}^\ast} \leq \mathit{C}.\mathsf{label}[{\mathit{l}'}]
}{
\mathit{C} \vdash \mathsf{br\_table}~{\mathit{l}^\ast}~{\mathit{l}'} : {\mathit{t}_{1}^\ast}~{\mathit{t}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
} \, {[\textsc{\scriptsize T{-}br\_table}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{return} = ({\mathit{t}^\ast})
}{
\mathit{C} \vdash \mathsf{return} : {\mathit{t}_{1}^\ast}~{\mathit{t}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
} \, {[\textsc{\scriptsize T{-}return}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{func}[\mathit{x}] = {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
}{
\mathit{C} \vdash \mathsf{call}~\mathit{x} : {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
} \, {[\textsc{\scriptsize T{-}call}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{table}[\mathit{x}] = \mathit{lim}~\mathsf{funcref}
 \qquad
\mathit{ft} = {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
}{
\mathit{C} \vdash \mathsf{call\_indirect}~\mathit{x}~\mathit{ft} : {\mathit{t}_{1}^\ast}~\mathsf{i{\scriptstyle32}} \rightarrow {\mathit{t}_{2}^\ast}
} \, {[\textsc{\scriptsize T{-}call\_indirect}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathit{nt}.\mathsf{const}~\mathit{c}_{\mathit{nt}} : \epsilon \rightarrow \mathit{nt}
} \, {[\textsc{\scriptsize T{-}const}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathit{nt} . \mathit{unop} : \mathit{nt} \rightarrow \mathit{nt}
} \, {[\textsc{\scriptsize T{-}unop}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathit{nt} . \mathit{binop} : \mathit{nt}~\mathit{nt} \rightarrow \mathit{nt}
} \, {[\textsc{\scriptsize T{-}binop}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathit{nt} . \mathit{testop} : \mathit{nt} \rightarrow \mathsf{i{\scriptstyle32}}
} \, {[\textsc{\scriptsize T{-}testop}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathit{nt} . \mathit{relop} : \mathit{nt}~\mathit{nt} \rightarrow \mathsf{i{\scriptstyle32}}
} \, {[\textsc{\scriptsize T{-}relop}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{n} \leq {|\mathit{nt}|}
}{
\mathit{C} \vdash {\mathit{nt}.\mathsf{extend}}{\mathit{n}} : \mathit{nt} \rightarrow \mathit{nt}
} \, {[\textsc{\scriptsize T{-}extend}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{nt}_{1} \neq \mathit{nt}_{2}
 \qquad
{|\mathit{nt}_{1}|} = {|\mathit{nt}_{2}|}
}{
\mathit{C} \vdash \mathsf{cvtop}~\mathit{nt}_{1}~\mathsf{reinterpret}~\mathit{nt}_{2} : \mathit{nt}_{2} \rightarrow \mathit{nt}_{1}
} \, {[\textsc{\scriptsize T{-}reinterpret}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{\mathsf{i}}{\mathit{n}}_{1} \neq {\mathsf{i}}{\mathit{n}}_{2}
 \qquad
{\mathit{sx}^?} = \epsilon \Leftrightarrow {|{\mathsf{i}}{\mathit{n}}_{1}|} > {|{\mathsf{i}}{\mathit{n}}_{2}|}
}{
\mathit{C} \vdash {\mathsf{i}}{\mathit{n}}_{1} . {{{{\mathsf{convert}}{\mathsf{\_}}}{{\mathsf{i}}{\mathit{n}}_{2}}}{\mathsf{\_}}}{{\mathit{sx}^?}} : {\mathsf{i}}{\mathit{n}}_{2} \rightarrow {\mathsf{i}}{\mathit{n}}_{1}
} \, {[\textsc{\scriptsize T{-}convert{-}i}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{\mathsf{f}}{\mathit{n}}_{1} \neq {\mathsf{f}}{\mathit{n}}_{2}
}{
\mathit{C} \vdash \mathsf{cvtop}~{\mathsf{f}}{\mathit{n}}_{1}~\mathsf{convert}~{\mathsf{f}}{\mathit{n}}_{2} : {\mathsf{f}}{\mathit{n}}_{2} \rightarrow {\mathsf{f}}{\mathit{n}}_{1}
} \, {[\textsc{\scriptsize T{-}convert{-}f}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathsf{ref.null}~\mathit{rt} : \epsilon \rightarrow \mathit{rt}
} \, {[\textsc{\scriptsize T{-}ref.null}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{func}[\mathit{x}] = \mathit{ft}
}{
\mathit{C} \vdash \mathsf{ref.func}~\mathit{x} : \epsilon \rightarrow \mathsf{funcref}
} \, {[\textsc{\scriptsize T{-}ref.func}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathsf{ref.is\_null} : \mathit{rt} \rightarrow \mathsf{i{\scriptstyle32}}
} \, {[\textsc{\scriptsize T{-}ref.is\_null}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{local}[\mathit{x}] = \mathit{t}
}{
\mathit{C} \vdash \mathsf{local.get}~\mathit{x} : \epsilon \rightarrow \mathit{t}
} \, {[\textsc{\scriptsize T{-}local.get}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{local}[\mathit{x}] = \mathit{t}
}{
\mathit{C} \vdash \mathsf{local.set}~\mathit{x} : \mathit{t} \rightarrow \epsilon
} \, {[\textsc{\scriptsize T{-}local.set}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{local}[\mathit{x}] = \mathit{t}
}{
\mathit{C} \vdash \mathsf{local.tee}~\mathit{x} : \mathit{t} \rightarrow \mathit{t}
} \, {[\textsc{\scriptsize T{-}local.tee}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{global}[\mathit{x}] = {\mathsf{mut}^?}~\mathit{t}
}{
\mathit{C} \vdash \mathsf{global.get}~\mathit{x} : \epsilon \rightarrow \mathit{t}
} \, {[\textsc{\scriptsize T{-}global.get}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{global}[\mathit{x}] = \mathsf{mut}~\mathit{t}
}{
\mathit{C} \vdash \mathsf{global.set}~\mathit{x} : \mathit{t} \rightarrow \epsilon
} \, {[\textsc{\scriptsize T{-}global.set}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{table}[\mathit{x}] = \mathit{lim}~\mathit{rt}
}{
\mathit{C} \vdash \mathsf{table.get}~\mathit{x} : \mathsf{i{\scriptstyle32}} \rightarrow \mathit{rt}
} \, {[\textsc{\scriptsize T{-}table.get}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{table}[\mathit{x}] = \mathit{lim}~\mathit{rt}
}{
\mathit{C} \vdash \mathsf{table.set}~\mathit{x} : \mathsf{i{\scriptstyle32}}~\mathit{rt} \rightarrow \epsilon
} \, {[\textsc{\scriptsize T{-}table.set}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{table}[\mathit{x}] = \mathit{tt}
}{
\mathit{C} \vdash \mathsf{table.size}~\mathit{x} : \epsilon \rightarrow \mathsf{i{\scriptstyle32}}
} \, {[\textsc{\scriptsize T{-}table.size}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{table}[\mathit{x}] = \mathit{lim}~\mathit{rt}
}{
\mathit{C} \vdash \mathsf{table.grow}~\mathit{x} : \mathit{rt}~\mathsf{i{\scriptstyle32}} \rightarrow \mathsf{i{\scriptstyle32}}
} \, {[\textsc{\scriptsize T{-}table.grow}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{table}[\mathit{x}] = \mathit{lim}~\mathit{rt}
}{
\mathit{C} \vdash \mathsf{table.fill}~\mathit{x} : \mathsf{i{\scriptstyle32}}~\mathit{rt}~\mathsf{i{\scriptstyle32}} \rightarrow \epsilon
} \, {[\textsc{\scriptsize T{-}table.fill}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{table}[\mathit{x}_{1}] = \mathit{lim}_{1}~\mathit{rt}
 \qquad
\mathit{C}.\mathsf{table}[\mathit{x}_{2}] = \mathit{lim}_{2}~\mathit{rt}
}{
\mathit{C} \vdash \mathsf{table.copy}~\mathit{x}_{1}~\mathit{x}_{2} : \mathsf{i{\scriptstyle32}}~\mathsf{i{\scriptstyle32}}~\mathsf{i{\scriptstyle32}} \rightarrow \epsilon
} \, {[\textsc{\scriptsize T{-}table.copy}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{table}[\mathit{x}_{1}] = \mathit{lim}~\mathit{rt}
 \qquad
\mathit{C}.\mathsf{elem}[\mathit{x}_{2}] = \mathit{rt}
}{
\mathit{C} \vdash \mathsf{table.init}~\mathit{x}_{1}~\mathit{x}_{2} : \mathsf{i{\scriptstyle32}}~\mathsf{i{\scriptstyle32}}~\mathsf{i{\scriptstyle32}} \rightarrow \epsilon
} \, {[\textsc{\scriptsize T{-}table.init}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{elem}[\mathit{x}] = \mathit{rt}
}{
\mathit{C} \vdash \mathsf{elem.drop}~\mathit{x} : \epsilon \rightarrow \epsilon
} \, {[\textsc{\scriptsize T{-}elem.drop}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{mem}[0] = \mathit{mt}
}{
\mathit{C} \vdash \mathsf{memory.size} : \epsilon \rightarrow \mathsf{i{\scriptstyle32}}
} \, {[\textsc{\scriptsize T{-}memory.size}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{mem}[0] = \mathit{mt}
}{
\mathit{C} \vdash \mathsf{memory.grow} : \mathsf{i{\scriptstyle32}} \rightarrow \mathsf{i{\scriptstyle32}}
} \, {[\textsc{\scriptsize T{-}memory.grow}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{mem}[0] = \mathit{mt}
}{
\mathit{C} \vdash \mathsf{memory.fill} : \mathsf{i{\scriptstyle32}}~\mathsf{i{\scriptstyle32}}~\mathsf{i{\scriptstyle32}} \rightarrow \mathsf{i{\scriptstyle32}}
} \, {[\textsc{\scriptsize T{-}memory.fill}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{mem}[0] = \mathit{mt}
}{
\mathit{C} \vdash \mathsf{memory.copy} : \mathsf{i{\scriptstyle32}}~\mathsf{i{\scriptstyle32}}~\mathsf{i{\scriptstyle32}} \rightarrow \mathsf{i{\scriptstyle32}}
} \, {[\textsc{\scriptsize T{-}memory.copy}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{mem}[0] = \mathit{mt}
 \qquad
\mathit{C}.\mathsf{data}[\mathit{x}] = \mathsf{ok}
}{
\mathit{C} \vdash \mathsf{memory.init}~\mathit{x} : \mathsf{i{\scriptstyle32}}~\mathsf{i{\scriptstyle32}}~\mathsf{i{\scriptstyle32}} \rightarrow \mathsf{i{\scriptstyle32}}
} \, {[\textsc{\scriptsize T{-}memory.init}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{data}[\mathit{x}] = \mathsf{ok}
}{
\mathit{C} \vdash \mathsf{data.drop}~\mathit{x} : \epsilon \rightarrow \epsilon
} \, {[\textsc{\scriptsize T{-}data.drop}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{mem}[0] = \mathit{mt}
 \qquad
{2^{\mathit{n}_{\mathsf{a}}}} \leq {|\mathit{t}|} / 8
 \qquad
({2^{\mathit{n}_{\mathsf{a}}}} \leq \mathit{n} / 8 < {|\mathit{t}|} / 8)^?
 \qquad
{\mathit{n}^?} = \epsilon \lor \mathit{nt} = {\mathsf{i}}{\mathit{n}}
}{
\mathit{C} \vdash {\mathit{nt}.\mathsf{load}}{{(\mathit{n}~\mathit{sx})^?}~\mathit{n}_{\mathsf{a}}~\mathit{n}_{\mathsf{o}}} : \mathsf{i{\scriptstyle32}} \rightarrow \mathit{nt}
} \, {[\textsc{\scriptsize T{-}load}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{mem}[0] = \mathit{mt}
 \qquad
{2^{\mathit{n}_{\mathsf{a}}}} \leq {|\mathit{t}|} / 8
 \qquad
({2^{\mathit{n}_{\mathsf{a}}}} \leq \mathit{n} / 8 < {|\mathit{t}|} / 8)^?
 \qquad
{\mathit{n}^?} = \epsilon \lor \mathit{nt} = {\mathsf{i}}{\mathit{n}}
}{
\mathit{C} \vdash {\mathit{nt}.\mathsf{store}}{{\mathit{n}^?}~\mathit{n}_{\mathsf{a}}~\mathit{n}_{\mathsf{o}}} : \mathsf{i{\scriptstyle32}}~\mathit{nt} \rightarrow \epsilon
} \, {[\textsc{\scriptsize T{-}store}]}
\qquad
\end{array}
$$

\vspace{1ex}

$\boxed{\mathit{context} \vdash \mathit{instr}~\mathsf{const}}$

$\boxed{\mathit{context} \vdash \mathit{expr}~\mathsf{const}}$

$\boxed{\mathit{context} \vdash \mathit{expr} : \mathit{valtype}~\mathsf{const}}$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash (\mathit{nt}.\mathsf{const}~\mathit{c})~\mathsf{const}
} \, {[\textsc{\scriptsize C{-}instr{-}const}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash (\mathsf{ref.null}~\mathit{rt})~\mathsf{const}
} \, {[\textsc{\scriptsize C{-}instr{-}ref.null}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash (\mathsf{ref.func}~\mathit{x})~\mathsf{const}
} \, {[\textsc{\scriptsize C{-}instr{-}ref.func}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{global}[\mathit{x}] = \epsilon~\mathit{t}
}{
\mathit{C} \vdash (\mathsf{global.get}~\mathit{x})~\mathsf{const}
} \, {[\textsc{\scriptsize C{-}instr{-}global.get}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
(\mathit{C} \vdash \mathit{instr}~\mathsf{const})^\ast
}{
\mathit{C} \vdash {\mathit{instr}^\ast}~\mathsf{const}
} \, {[\textsc{\scriptsize C{-}expr}]}
\qquad
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C} \vdash \mathit{expr} : \mathit{t}
 \qquad
\mathit{C} \vdash \mathit{expr}~\mathsf{const}
}{
\mathit{C} \vdash \mathit{expr} : \mathit{t}~\mathsf{const}
} \, {[\textsc{\scriptsize TC{-}expr}]}
\qquad
\end{array}
$$

\vspace{1ex}

$\boxed{\mathit{context} \vdash \mathit{func} : \mathit{functype}}$

$\boxed{\mathit{context} \vdash \mathit{global} : \mathit{globaltype}}$

$\boxed{\mathit{context} \vdash \mathit{table} : \mathit{tabletype}}$

$\boxed{\mathit{context} \vdash \mathit{mem} : \mathit{memtype}}$

$\boxed{\mathit{context} \vdash \mathit{elem} : \mathit{reftype}}$

$\boxed{\mathit{context} \vdash \mathit{data} : \mathsf{ok}}$

$\boxed{\mathit{context} \vdash \mathit{elemmode} : \mathit{reftype}}$

$\boxed{\mathit{context} \vdash \mathit{datamode} : \mathsf{ok}}$

$\boxed{\mathit{context} \vdash \mathit{start} : \mathsf{ok}}$

\vspace{1ex}

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{ft} = {\mathit{t}_{1}^\ast} \rightarrow {\mathit{t}_{2}^\ast}
 \qquad
{ \vdash }\;\mathit{ft} : \mathsf{ok}
 \qquad
\mathit{C}, \mathsf{local}~{\mathit{t}_{1}^\ast}~{\mathit{t}^\ast}, \mathsf{label}~({\mathit{t}_{2}^\ast}), \mathsf{return}~({\mathit{t}_{2}^\ast}) \vdash \mathit{expr} : {\mathit{t}_{2}^\ast}
}{
\mathit{C} \vdash \mathsf{func}~\mathit{ft}~{\mathit{t}^\ast}~\mathit{expr} : \mathit{ft}
} \, {[\textsc{\scriptsize T{-}func}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{gt} : \mathsf{ok}
 \qquad
\mathit{gt} = {\mathsf{mut}^?}~\mathit{t}
 \qquad
\mathit{C} \vdash \mathit{expr} : \mathit{t}~\mathsf{const}
}{
\mathit{C} \vdash \mathsf{global}~\mathit{gt}~\mathit{expr} : \mathit{gt}
} \, {[\textsc{\scriptsize T{-}global}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{tt} : \mathsf{ok}
}{
\mathit{C} \vdash \mathsf{table}~\mathit{tt} : \mathit{tt}
} \, {[\textsc{\scriptsize T{-}table}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{mt} : \mathsf{ok}
}{
\mathit{C} \vdash \mathsf{memory}~\mathit{mt} : \mathit{mt}
} \, {[\textsc{\scriptsize T{-}mem}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
(\mathit{C} \vdash \mathit{expr} : \mathit{rt})^\ast
 \qquad
(\mathit{C} \vdash \mathit{elemmode} : \mathit{rt})^?
}{
\mathit{C} \vdash \mathsf{elem}~\mathit{rt}~{\mathit{expr}^\ast}~{\mathit{elemmode}^?} : \mathit{rt}
} \, {[\textsc{\scriptsize T{-}elem}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
(\mathit{C} \vdash \mathit{datamode} : \mathsf{ok})^?
}{
\mathit{C} \vdash \mathsf{data}~{{\mathit{b}^\ast}^\ast}~{\mathit{datamode}^?} : \mathsf{ok}
} \, {[\textsc{\scriptsize T{-}data}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{table}[\mathit{x}] = \mathit{lim}~\mathit{rt}
 \qquad
(\mathit{C} \vdash \mathit{expr} : \mathsf{i{\scriptstyle32}}~\mathsf{const})^\ast
}{
\mathit{C} \vdash \mathsf{table}~\mathit{x}~\mathit{expr} : \mathit{rt}
} \, {[\textsc{\scriptsize T{-}elemmode{-}active}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
\mathit{C} \vdash \mathsf{declare} : \mathit{rt}
} \, {[\textsc{\scriptsize T{-}elemmode{-}declare}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{mem}[0] = \mathit{mt}
 \qquad
(\mathit{C} \vdash \mathit{expr} : \mathsf{i{\scriptstyle32}}~\mathsf{const})^\ast
}{
\mathit{C} \vdash \mathsf{memory}~0~\mathit{expr} : \mathsf{ok}
} \, {[\textsc{\scriptsize T{-}datamode}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{func}[\mathit{x}] = \epsilon \rightarrow \epsilon
}{
\mathit{C} \vdash \mathsf{start}~\mathit{x} : \mathsf{ok}
} \, {[\textsc{\scriptsize T{-}start}]}
\qquad
\end{array}
$$

\vspace{1ex}

$\boxed{\mathit{context} \vdash \mathit{import} : \mathit{externtype}}$

$\boxed{\mathit{context} \vdash \mathit{export} : \mathit{externtype}}$

$\boxed{\mathit{context} \vdash \mathit{externuse} : \mathit{externtype}}$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{ \vdash }\;\mathit{xt} : \mathsf{ok}
}{
\mathit{C} \vdash \mathsf{import}~\mathit{name}_{1}~\mathit{name}_{2}~\mathit{xt} : \mathit{xt}
} \, {[\textsc{\scriptsize T{-}import}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C} \vdash \mathit{externuse} : \mathit{xt}
}{
\mathit{C} \vdash \mathsf{export}~\mathit{name}~\mathit{externuse} : \mathit{xt}
} \, {[\textsc{\scriptsize T{-}export}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{func}[\mathit{x}] = \mathit{ft}
}{
\mathit{C} \vdash \mathsf{func}~\mathit{x} : \mathsf{func}~\mathit{ft}
} \, {[\textsc{\scriptsize T{-}externuse{-}func}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{global}[\mathit{x}] = \mathit{gt}
}{
\mathit{C} \vdash \mathsf{global}~\mathit{x} : \mathsf{global}~\mathit{gt}
} \, {[\textsc{\scriptsize T{-}externuse{-}global}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{table}[\mathit{x}] = \mathit{tt}
}{
\mathit{C} \vdash \mathsf{table}~\mathit{x} : \mathsf{table}~\mathit{tt}
} \, {[\textsc{\scriptsize T{-}externuse{-}table}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
\mathit{C}.\mathsf{mem}[\mathit{x}] = \mathit{mt}
}{
\mathit{C} \vdash \mathsf{memory}~\mathit{x} : \mathsf{memory}~\mathit{mt}
} \, {[\textsc{\scriptsize T{-}externuse{-}mem}]}
\qquad
\end{array}
$$

\vspace{1ex}

$\boxed{{ \vdash }\;\mathit{module} : \mathsf{ok}}$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
(\mathit{C} \vdash \mathit{func} : \mathit{ft})^\ast
 \qquad
(\mathit{C} \vdash \mathit{global} : \mathit{gt})^\ast
 \qquad
(\mathit{C} \vdash \mathit{table} : \mathit{tt})^\ast
 \qquad
(\mathit{C} \vdash \mathit{mem} : \mathit{mt})^\ast
 \qquad
(\mathit{C} \vdash \mathit{elem} : \mathit{rt})^\ast
 \qquad
(\mathit{C} \vdash \mathit{data} : \mathsf{ok})^{\mathit{n}}
 \qquad
(\mathit{C} \vdash \mathit{start} : \mathsf{ok})^\ast
 \qquad
\mathit{C} = \{ \begin{array}[t]{@{}l@{}}
\mathsf{func}~{\mathit{ft}^\ast},\; \mathsf{global}~{\mathit{gt}^\ast},\; \mathsf{table}~{\mathit{tt}^\ast},\; \mathsf{mem}~{\mathit{mt}^\ast},\; \mathsf{elem}~{\mathit{rt}^\ast},\; \mathsf{data}~{\mathsf{ok}^{\mathit{n}}} \}\end{array}
 \qquad
{|{\mathit{mem}^\ast}|} \leq 1
 \qquad
{|{\mathit{start}^\ast}|} \leq 1
}{
{ \vdash }\;\mathsf{module}~{\mathit{import}^\ast}~{\mathit{func}^\ast}~{\mathit{global}^\ast}~{\mathit{table}^\ast}~{\mathit{mem}^\ast}~{\mathit{elem}^\ast}~{\mathit{data}^{\mathit{n}}}~{\mathit{start}^\ast}~{\mathit{export}^\ast} : \mathsf{ok}
} \, {[\textsc{\scriptsize T{-}module}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}lrrl@{}}
\mbox{(address)} & \mathit{addr} &::=& \mathit{nat} \\
\mbox{(function address)} & \mathit{funcaddr} &::=& \mathit{addr} \\
\mbox{(global address)} & \mathit{globaladdr} &::=& \mathit{addr} \\
\mbox{(table address)} & \mathit{tableaddr} &::=& \mathit{addr} \\
\mbox{(memory address)} & \mathit{memaddr} &::=& \mathit{addr} \\
\mbox{(elem address)} & \mathit{elemaddr} &::=& \mathit{addr} \\
\mbox{(data address)} & \mathit{dataaddr} &::=& \mathit{addr} \\
\mbox{(label address)} & \mathit{labeladdr} &::=& \mathit{addr} \\
\mbox{(host address)} & \mathit{hostaddr} &::=& \mathit{addr} \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}lrrl@{}}
\mbox{(number)} & \mathit{num} &::=& \mathsf{\mathit{numtype}}.\mathsf{const}~\mathsf{\mathit{c}\_{\mathit{numtype}}} \\
\mbox{(reference)} & \mathit{ref} &::=& \mathsf{ref.null}~\mathit{reftype} ~|~ \mathsf{ref.func}~\mathsf{\mathit{funcaddr}} ~|~ \mathsf{ref.extern}~\mathsf{\mathit{hostaddr}} \\
\mbox{(value)} & \mathit{val} &::=& \mathit{num} ~|~ \mathit{ref} \\
\mbox{(result)} & \mathit{result} &::=& {\mathit{val}^\ast} ~|~ \mathsf{trap} \\
\end{array}
$$

$$
\begin{array}{@{}lrrl@{}}
\mbox{(external value)} & \mathit{externval} &::=& \mathsf{func}~\mathit{funcaddr} ~|~ \mathsf{global}~\mathit{globaladdr} ~|~ \mathsf{table}~\mathit{tableaddr} ~|~ \mathsf{mem}~\mathit{memaddr} \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}lcl@{}l@{}}
{\mathrm{default}}_{\mathsf{i{\scriptstyle32}}} &=& (\mathsf{i{\scriptstyle32}}.\mathsf{const}~0) &  \\
{\mathrm{default}}_{\mathsf{i{\scriptstyle64}}} &=& (\mathsf{i{\scriptstyle64}}.\mathsf{const}~0) &  \\
{\mathrm{default}}_{\mathsf{f{\scriptstyle32}}} &=& (\mathsf{f{\scriptstyle32}}.\mathsf{const}~0) &  \\
{\mathrm{default}}_{\mathsf{f{\scriptstyle64}}} &=& (\mathsf{f{\scriptstyle64}}.\mathsf{const}~0) &  \\
{\mathrm{default}}_{\mathit{rt}} &=& (\mathsf{ref.null}~\mathit{rt}) &  \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}lrrl@{}}
\mbox{(function instance)} & \mathit{funcinst} &::=& \mathit{moduleinst} ; \mathit{func} \\
\mbox{(global instance)} & \mathit{globalinst} &::=& \mathit{val} \\
\mbox{(table instance)} & \mathit{tableinst} &::=& {\mathit{ref}^\ast} \\
\mbox{(memory instance)} & \mathit{meminst} &::=& {\mathit{byte}^\ast} \\
\mbox{(element instance)} & \mathit{eleminst} &::=& {\mathit{ref}^\ast} \\
\mbox{(data instance)} & \mathit{datainst} &::=& {\mathit{byte}^\ast} \\
\mbox{(export instance)} & \mathit{exportinst} &::=& \mathsf{export}~\mathit{name}~\mathit{externval} \\
\mbox{(store)} & \mathit{store} &::=& \{\; \begin{array}[t]{@{}l@{}}
\mathsf{func}~{\mathit{funcinst}^\ast},\; \\
  \mathsf{global}~{\mathit{globalinst}^\ast},\; \\
  \mathsf{table}~{\mathit{tableinst}^\ast},\; \\
  \mathsf{mem}~{\mathit{meminst}^\ast},\; \\
  \mathsf{elem}~{\mathit{eleminst}^\ast},\; \\
  \mathsf{data}~{\mathit{datainst}^\ast} \;\}\end{array} \\
\mbox{(module instance)} & \mathit{moduleinst} &::=& \{\; \begin{array}[t]{@{}l@{}}
\mathsf{func}~{\mathit{funcaddr}^\ast},\; \\
  \mathsf{global}~{\mathit{globaladdr}^\ast},\; \\
  \mathsf{table}~{\mathit{tableaddr}^\ast},\; \\
  \mathsf{mem}~{\mathit{memaddr}^\ast},\; \\
  \mathsf{elem}~{\mathit{elemaddr}^\ast},\; \\
  \mathsf{data}~{\mathit{dataaddr}^\ast},\; \\
  \mathsf{export}~{\mathit{exportinst}^\ast} \;\}\end{array} \\
\mbox{(frame)} & \mathit{frame} &::=& \{\; \begin{array}[t]{@{}l@{}}
\mathsf{local}~{\mathit{val}^\ast},\; \\
  \mathsf{module}~\mathit{moduleinst} \;\}\end{array} \\
\mbox{(state)} & \mathit{state} &::=& \mathit{store} ; \mathit{frame} \\
\mbox{(configuration)} & \mathit{config} &::=& \mathit{state} ; {\mathit{instr}^\ast} \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
(\mathit{s} ; \mathit{f}).\mathsf{module}.\mathsf{func} &=& \mathit{f}.\mathsf{module}.\mathsf{func} &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
(\mathit{s} ; \mathit{f}).\mathsf{func} &=& \mathit{s}.\mathsf{func} &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{(\mathit{s} ; \mathit{f}).\mathsf{func}}{[\mathit{x}]} &=& \mathit{s}.\mathsf{func}[\mathit{f}.\mathsf{module}.\mathsf{func}[\mathit{x}]] &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{(\mathit{s} ; \mathit{f}).\mathsf{local}}{[\mathit{x}]} &=& \mathit{f}.\mathsf{local}[\mathit{x}] &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{(\mathit{s} ; \mathit{f}).\mathsf{global}}{[\mathit{x}]} &=& \mathit{s}.\mathsf{global}[\mathit{f}.\mathsf{module}.\mathsf{global}[\mathit{x}]] &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{(\mathit{s} ; \mathit{f}).\mathsf{table}}{[\mathit{x}]} &=& \mathit{s}.\mathsf{table}[\mathit{f}.\mathsf{module}.\mathsf{table}[\mathit{x}]] &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{(\mathit{s} ; \mathit{f}).\mathsf{elem}}{[\mathit{x}]} &=& \mathit{s}.\mathsf{elem}[\mathit{f}.\mathsf{module}.\mathsf{elem}[\mathit{x}]] &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{(\mathit{s} ; \mathit{f})}{[{\mathsf{local}}{[\mathit{x}]} = \mathit{v}]} &=& \mathit{s} ; \mathit{f}[\mathsf{local}[\mathit{x}] = \mathit{v}] &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{(\mathit{s} ; \mathit{f})}{[{\mathsf{global}}{[\mathit{x}]} = \mathit{v}]} &=& \mathit{s}[\mathsf{global}[\mathit{f}.\mathsf{module}.\mathsf{global}[\mathit{x}]] = \mathit{v}] ; \mathit{f} &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{(\mathit{s} ; \mathit{f})}{[{{\mathsf{table}}{[\mathit{x}]}}{[\mathit{i}]} = \mathit{r}]} &=& \mathit{s}[\mathsf{table}[\mathit{f}.\mathsf{module}.\mathsf{table}[\mathit{x}]][\mathit{i}] = \mathit{r}] ; \mathit{f} &  \\
\end{array}
$$

$$
\begin{array}{@{}lrrl@{}}
\mbox{(administrative instruction)} & \mathit{instr} &::=& \mathit{instr} \\ &&|&
\mathsf{ref.func}~\mathsf{\mathit{funcaddr}} \\ &&|&
\mathsf{ref.extern}~\mathsf{\mathit{hostaddr}} \\ &&|&
\mathsf{call}~\mathsf{\mathit{funcaddr}} \\ &&|&
{{\mathsf{label}}_{\mathsf{\mathit{n}}}}{\mathsf{\{{\mathit{instr}^\ast}\}}~\mathsf{{\mathit{instr}^\ast}}} \\ &&|&
{{\mathsf{frame}}_{\mathsf{\mathit{n}}}}{\mathsf{\{\mathit{frame}\}}~\mathsf{{\mathit{instr}^\ast}}} \\ &&|&
\mathsf{trap} \\
\mbox{(evaluation context)} & \mathit{E} &::=& [\mathsf{\_}] \\ &&|&
{\mathit{val}^\ast}~\mathit{E}~{\mathit{instr}^\ast} \\ &&|&
{{\mathsf{label}}_{\mathsf{\mathit{n}}}}{\mathsf{\{{\mathit{instr}^\ast}\}}~\mathsf{\mathit{e}}} \\
\end{array}
$$

$\boxed{\mathit{config} \hookrightarrow \mathit{config}}$

$\boxed{{\mathit{instr}^\ast} \hookrightarrow {\mathit{instr}^\ast}}$

$\boxed{\mathit{config} \hookrightarrow {\mathit{instr}^\ast}}$

$\boxed{\mathit{config} \hookrightarrow \mathit{config}}$

$$
\begin{array}{@{}l@{}lcl@{}l@{}}
{[\textsc{\scriptsize E{-}pure}]} \quad & \mathit{z} ; {\mathit{instr}^\ast} &\hookrightarrow& \mathit{z} ; {{\mathit{instr}'}^\ast} &\quad
  \mbox{if}~{\mathit{instr}^\ast} \hookrightarrow {{\mathit{instr}'}^\ast} \\
{[\textsc{\scriptsize E{-}read}]} \quad & \mathit{z} ; {\mathit{instr}^\ast} &\hookrightarrow& \mathit{z} ; {{\mathit{instr}'}^\ast} &\quad
  \mbox{if}~\mathit{z} ; {\mathit{instr}^\ast} \hookrightarrow {{\mathit{instr}'}^\ast} \\
{[\textsc{\scriptsize E{-}write}]} \quad & \mathit{z} ; {\mathit{instr}^\ast} &\hookrightarrow& {\mathit{z}'} ; {{\mathit{instr}'}^\ast} &\quad
  \mbox{if}~\mathit{z} ; {\mathit{instr}^\ast} \hookrightarrow {\mathit{z}'} ; {{\mathit{instr}'}^\ast} \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}l@{}lcl@{}l@{}}
{[\textsc{\scriptsize E{-}ref.is\_null{-}true}]} \quad & \mathit{val}~\mathsf{ref.is\_null} &\hookrightarrow& (\mathsf{i{\scriptstyle32}}.\mathsf{const}~1) &\quad
  \mbox{if}~\mathit{val} = \mathsf{ref.null}~\mathit{rt} \\
{[\textsc{\scriptsize E{-}ref.is\_null{-}false}]} \quad & \mathit{val}~\mathsf{ref.is\_null} &\hookrightarrow& (\mathsf{i{\scriptstyle32}}.\mathsf{const}~0) &\quad
  \mbox{otherwise} \\
{[\textsc{\scriptsize E{-}unreachable}]} \quad & \mathsf{unreachable} &\hookrightarrow& \mathsf{trap} &  \\
{[\textsc{\scriptsize E{-}nop}]} \quad & \mathsf{nop} &\hookrightarrow& \epsilon &  \\
{[\textsc{\scriptsize E{-}drop}]} \quad & \mathit{val}~\mathsf{drop} &\hookrightarrow& \epsilon &  \\
{[\textsc{\scriptsize E{-}select{-}true}]} \quad & \mathit{val}_{1}~\mathit{val}_{2}~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{c})~(\mathsf{select}~{\mathit{t}^?}) &\hookrightarrow& \mathit{val}_{1} &\quad
  \mbox{if}~\mathit{c} \neq 0 \\
{[\textsc{\scriptsize E{-}select{-}false}]} \quad & \mathit{val}_{1}~\mathit{val}_{2}~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{c})~(\mathsf{select}~{\mathit{t}^?}) &\hookrightarrow& \mathit{val}_{2} &\quad
  \mbox{if}~\mathit{c} = 0 \\
{[\textsc{\scriptsize E{-}local.tee}]} \quad & \mathit{val}~(\mathsf{local.tee}~\mathit{x}) &\hookrightarrow& \mathit{val}~\mathit{val}~(\mathsf{local.set}~\mathit{x}) &  \\
{[\textsc{\scriptsize E{-}block}]} \quad & {\mathit{val}^{\mathit{k}}}~(\mathsf{block}~\mathit{bt}~{\mathit{instr}^\ast}) &\hookrightarrow& ({{\mathsf{label}}_{\mathit{n}}}{\{\epsilon\}~{\mathit{val}^{\mathit{k}}}~{\mathit{instr}^\ast}}) &\quad
  \mbox{if}~\mathit{bt} = {\mathit{t}_{1}^{\mathit{k}}} \rightarrow {\mathit{t}_{2}^{\mathit{n}}} \\
{[\textsc{\scriptsize E{-}loop}]} \quad & {\mathit{val}^{\mathit{k}}}~(\mathsf{loop}~\mathit{bt}~{\mathit{instr}^\ast}) &\hookrightarrow& ({{\mathsf{label}}_{\mathit{n}}}{\{\mathsf{loop}~\mathit{bt}~{\mathit{instr}^\ast}\}~{\mathit{val}^{\mathit{k}}}~{\mathit{instr}^\ast}}) &\quad
  \mbox{if}~\mathit{bt} = {\mathit{t}_{1}^{\mathit{k}}} \rightarrow {\mathit{t}_{2}^{\mathit{n}}} \\
{[\textsc{\scriptsize E{-}if{-}true}]} \quad & (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{c})~(\mathsf{if}~\mathit{bt}~{\mathit{instr}_{1}^\ast}~\mathsf{else}~{\mathit{instr}_{2}^\ast}) &\hookrightarrow& (\mathsf{block}~\mathit{bt}~{\mathit{instr}_{1}^\ast}) &\quad
  \mbox{if}~\mathit{c} \neq 0 \\
{[\textsc{\scriptsize E{-}if{-}false}]} \quad & (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{c})~(\mathsf{if}~\mathit{bt}~{\mathit{instr}_{1}^\ast}~\mathsf{else}~{\mathit{instr}_{2}^\ast}) &\hookrightarrow& (\mathsf{block}~\mathit{bt}~{\mathit{instr}_{2}^\ast}) &\quad
  \mbox{if}~\mathit{c} = 0 \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}l@{}lcl@{}l@{}}
{[\textsc{\scriptsize E{-}br{-}zero}]} \quad & ({{\mathsf{label}}_{\mathit{n}}}{\{{{\mathit{instr}'}^\ast}\}~{{\mathit{val}'}^\ast}~{\mathit{val}^{\mathit{n}}}~(\mathsf{br}~0)~{\mathit{instr}^\ast}}) &\hookrightarrow& {\mathit{val}^{\mathit{n}}}~{{\mathit{instr}'}^\ast} &  \\
{[\textsc{\scriptsize E{-}br{-}succ}]} \quad & ({{\mathsf{label}}_{\mathit{n}}}{\{{{\mathit{instr}'}^\ast}\}~{\mathit{val}^\ast}~(\mathsf{br}~\mathit{l} + 1)~{\mathit{instr}^\ast}}) &\hookrightarrow& {\mathit{val}^\ast}~(\mathsf{br}~\mathit{l}) &  \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}l@{}lcl@{}l@{}}
{[\textsc{\scriptsize E{-}br\_if{-}true}]} \quad & (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{c})~(\mathsf{br\_if}~\mathit{l}) &\hookrightarrow& (\mathsf{br}~\mathit{l}) &\quad
  \mbox{if}~\mathit{c} \neq 0 \\
{[\textsc{\scriptsize E{-}br\_if{-}false}]} \quad & (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{c})~(\mathsf{br\_if}~\mathit{l}) &\hookrightarrow& \epsilon &\quad
  \mbox{if}~\mathit{c} = 0 \\
\end{array}
$$

\vspace{1ex}

$$
\begin{array}{@{}l@{}lcl@{}l@{}}
{[\textsc{\scriptsize E{-}br\_table{-}lt}]} \quad & (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{br\_table}~{\mathit{l}^\ast}~{\mathit{l}'}) &\hookrightarrow& (\mathsf{br}~{\mathit{l}^\ast}[\mathit{i}]) &\quad
  \mbox{if}~\mathit{i} < {|{\mathit{l}^\ast}|} \\
{[\textsc{\scriptsize E{-}br\_table{-}ge}]} \quad & (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{br\_table}~{\mathit{l}^\ast}~{\mathit{l}'}) &\hookrightarrow& (\mathsf{br}~{\mathit{l}'}) &\quad
  \mbox{if}~\mathit{i} \geq {|{\mathit{l}^\ast}|} \\
\end{array}
$$

$$
\begin{array}{@{}l@{}lcl@{}l@{}}
{[\textsc{\scriptsize E{-}ref.func}]} \quad & \mathit{z} ; (\mathsf{ref.func}~\mathit{x}) &\hookrightarrow& (\mathsf{ref.func}~\mathit{z}.\mathsf{module}.\mathsf{func}[\mathit{x}]) &  \\
{[\textsc{\scriptsize E{-}local.get}]} \quad & \mathit{z} ; (\mathsf{local.get}~\mathit{x}) &\hookrightarrow& {\mathit{z}.\mathsf{local}}{[\mathit{x}]} &  \\
{[\textsc{\scriptsize E{-}global.get}]} \quad & \mathit{z} ; (\mathsf{global.get}~\mathit{x}) &\hookrightarrow& {\mathit{z}.\mathsf{global}}{[\mathit{x}]} &  \\
{[\textsc{\scriptsize E{-}table.get{-}ge}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{table.get}~\mathit{x}) &\hookrightarrow& \mathsf{trap} &\quad
  \mbox{if}~\mathit{i} \geq {|{\mathit{z}.\mathsf{table}}{[\mathit{x}]}|} \\
{[\textsc{\scriptsize E{-}table.get{-}lt}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{table.get}~\mathit{x}) &\hookrightarrow& {\mathit{z}.\mathsf{table}}{[\mathit{x}]}[\mathit{i}] &\quad
  \mbox{if}~\mathit{i} < {|{\mathit{z}.\mathsf{table}}{[\mathit{x}]}|} \\
{[\textsc{\scriptsize E{-}table.size}]} \quad & \mathit{z} ; (\mathsf{table.size}~\mathit{x}) &\hookrightarrow& (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n}) &\quad
  \mbox{if}~{|{\mathit{z}.\mathsf{table}}{[\mathit{x}]}|} = \mathit{n} \\
{[\textsc{\scriptsize E{-}table.fill{-}trap}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~\mathit{val}~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n})~(\mathsf{table.fill}~\mathit{x}) &\hookrightarrow& \mathsf{trap} &\quad
  \mbox{if}~\mathit{i} + \mathit{n} > {|{\mathit{z}.\mathsf{table}}{[\mathit{x}]}|} \\
{[\textsc{\scriptsize E{-}table.fill{-}zero}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~\mathit{val}~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~0)~(\mathsf{table.fill}~\mathit{x}) &\hookrightarrow& \epsilon &\quad
  \mbox{otherwise} \\
{[\textsc{\scriptsize E{-}table.fill{-}succ}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~\mathit{val}~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n} + 1)~(\mathsf{table.fill}~\mathit{x}) &\hookrightarrow& (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~\mathit{val}~(\mathsf{table.set}~\mathit{x})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i} + 1)~\mathit{val}~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n})~(\mathsf{table.fill}~\mathit{x}) &\quad
  \mbox{otherwise} \\
{[\textsc{\scriptsize E{-}table.copy{-}trap}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n})~(\mathsf{table.copy}~\mathit{x}~\mathit{y}) &\hookrightarrow& \mathsf{trap} &\quad
  \mbox{if}~\mathit{i} + \mathit{n} > {|{\mathit{z}.\mathsf{table}}{[\mathit{y}]}|} \lor \mathit{j} + \mathit{n} > {|{\mathit{z}.\mathsf{table}}{[\mathit{x}]}|} \\
{[\textsc{\scriptsize E{-}table.copy{-}zero}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~0)~(\mathsf{table.copy}~\mathit{x}~\mathit{y}) &\hookrightarrow& \epsilon &\quad
  \mbox{otherwise} \\
{[\textsc{\scriptsize E{-}table.copy{-}le}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n} + 1)~(\mathsf{table.copy}~\mathit{x}~\mathit{y}) &\hookrightarrow& (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{table.get}~\mathit{y})~(\mathsf{table.set}~\mathit{x})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j} + 1)~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i} + 1)~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n})~(\mathsf{table.copy}~\mathit{x}~\mathit{y}) &\quad
  \mbox{if}~\mathit{j} \leq \mathit{i} \\
{[\textsc{\scriptsize E{-}table.copy{-}gt}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n} + 1)~(\mathsf{table.copy}~\mathit{x}~\mathit{y}) &\hookrightarrow& (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j} + \mathit{n})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i} + \mathit{n})~(\mathsf{table.get}~\mathit{y})~(\mathsf{table.set}~\mathit{x})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j} + 1)~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i} + 1)~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n})~(\mathsf{table.copy}~\mathit{x}~\mathit{y}) &\quad
  \mbox{if}~\mathit{i} > \mathit{i} \\
{[\textsc{\scriptsize E{-}table.init{-}trap}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n})~(\mathsf{table.init}~\mathit{x}~\mathit{y}) &\hookrightarrow& \mathsf{trap} &\quad
  \mbox{if}~\mathit{i} + \mathit{n} > {|{\mathit{z}.\mathsf{elem}}{[\mathit{y}]}|} \lor \mathit{j} + \mathit{n} > {|{\mathit{z}.\mathsf{table}}{[\mathit{x}]}|} \\
{[\textsc{\scriptsize E{-}table.init{-}zero}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~0)~(\mathsf{table.init}~\mathit{x}~\mathit{y}) &\hookrightarrow& \epsilon &\quad
  \mbox{otherwise} \\
{[\textsc{\scriptsize E{-}table.init{-}le}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n} + 1)~(\mathsf{table.init}~\mathit{x}~\mathit{y}) &\hookrightarrow& (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j})~{\mathit{z}.\mathsf{elem}}{[\mathit{y}]}[\mathit{i}]~(\mathsf{table.set}~\mathit{x})~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{j} + 1)~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i} + 1)~(\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{n})~(\mathsf{table.init}~\mathit{x}~\mathit{y}) &\quad
  \mbox{otherwise} \\
{[\textsc{\scriptsize E{-}call}]} \quad & \mathit{z} ; (\mathsf{call}~\mathit{x}) &\hookrightarrow& (\mathsf{call}~\mathit{z}.\mathsf{module}.\mathsf{func}[\mathit{x}]) &  \\
{[\textsc{\scriptsize E{-}call\_indirect{-}call}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{call\_indirect}~\mathit{x}~\mathit{ft}) &\hookrightarrow& (\mathsf{call}~\mathit{a}) &\quad
  \mbox{if}~{\mathit{z}.\mathsf{table}}{[\mathit{x}]}[\mathit{i}] = (\mathsf{ref.func}~\mathit{a}) \\
 &&&\quad {\land}~\mathit{z}.\mathsf{func}[\mathit{a}] = \mathit{m} ; \mathit{func} \\
{[\textsc{\scriptsize E{-}call\_indirect{-}trap}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~(\mathsf{call\_indirect}~\mathit{x}~\mathit{ft}) &\hookrightarrow& \mathsf{trap} &\quad
  \mbox{otherwise} \\
{[\textsc{\scriptsize E{-}call\_addr}]} \quad & \mathit{z} ; {\mathit{val}^{\mathit{k}}}~(\mathsf{call}~\mathit{a}) &\hookrightarrow& ({{\mathsf{frame}}_{\mathit{n}}}{\{\{ \begin{array}[t]{@{}l@{}}
\mathsf{local}~{\mathit{val}^{\mathit{k}}}~{({\mathrm{default}}_{\mathit{t}})^\ast},\; \mathsf{module}~\mathit{m} \}\end{array}\}~({{\mathsf{label}}_{\mathit{n}}}{\{\epsilon\}~{\mathit{instr}^\ast}})}) &\quad
  \mbox{if}~\mathit{z}.\mathsf{func}[\mathit{a}] = \mathit{m} ; \mathsf{func}~({\mathit{t}_{1}^{\mathit{k}}} \rightarrow {\mathit{t}_{2}^{\mathit{n}}})~{\mathit{t}^\ast}~{\mathit{instr}^\ast} \\
\end{array}
$$

$$
\begin{array}{@{}l@{}lcl@{}l@{}}
{[\textsc{\scriptsize E{-}local.set}]} \quad & \mathit{z} ; \mathit{val}~(\mathsf{local.set}~\mathit{x}) &\hookrightarrow& {\mathit{z}}{[{\mathsf{local}}{[\mathit{x}]} = \mathit{val}]} ; \epsilon &  \\
{[\textsc{\scriptsize E{-}global.set}]} \quad & \mathit{z} ; \mathit{val}~(\mathsf{global.set}~\mathit{x}) &\hookrightarrow& {\mathit{z}}{[{\mathsf{global}}{[\mathit{x}]} = \mathit{val}]} ; \epsilon &  \\
{[\textsc{\scriptsize E{-}table.set{-}lt}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~\mathit{ref}~(\mathsf{table.get}~\mathit{x}) &\hookrightarrow& {\mathit{z}}{[{{\mathsf{table}}{[\mathit{x}]}}{[\mathit{i}]} = \mathit{ref}]} ; \epsilon &\quad
  \mbox{if}~\mathit{i} < {|{\mathit{z}.\mathsf{table}}{[\mathit{x}]}|} \\
{[\textsc{\scriptsize E{-}table.set{-}ge}]} \quad & \mathit{z} ; (\mathsf{i{\scriptstyle32}}.\mathsf{const}~\mathit{i})~\mathit{ref}~(\mathsf{table.get}~\mathit{x}) &\hookrightarrow& \mathit{z} ; \mathsf{trap} &\quad
  \mbox{if}~\mathit{i} \geq {|{\mathit{z}.\mathsf{table}}{[\mathit{x}]}|} \\
\end{array}
$$


== Prose Generation...
{val REF.IS_NULL} ~> ({CONST I32 1})
    -- iff val = {REF.NULL rt}
{val REF.IS_NULL} ~> ({CONST I32 0})
    -- otherwise
1. Assert: Due to validation, a value is on the top of the stack.
2. Pop the value val from the stack.
3. If val = {REF.NULL rt}, then:
  1) Push the value CONST I32 1 to the stack.
4. If otherwise, then:
  1) Push the value CONST I32 0 to the stack.

UNREACHABLE ~> TRAP
1. Trap.

NOP ~> epsilon
1. Do nothing.

{val DROP} ~> epsilon
1. Assert: Due to validation, a value is on the top of the stack.
2. Pop the value val from the stack.

{val_1 val_2 ({CONST I32 c}) ({SELECT t?})} ~> val_1
    -- iff c =/= 0
{val_1 val_2 ({CONST I32 c}) ({SELECT t?})} ~> val_2
    -- iff c = 0
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 c}) from the stack.
3. Assert: Due to validation, a value is on the top of the stack.
4. Pop the value val_2 from the stack.
5. Assert: Due to validation, a value is on the top of the stack.
6. Pop the value val_1 from the stack.
7. If c =/= 0, then:
  1) Push the value val_1 to the stack.
8. If c = 0, then:
  1) Push the value val_2 to the stack.

{val ({LOCAL.TEE x})} ~> {val val ({LOCAL.SET x})}
1. Assert: Due to validation, a value is on the top of the stack.
2. Pop the value val from the stack.
3. Push the value val to the stack.
4. Push the value val to the stack.
5. Execute the instruction LOCAL.SET x.

{val^k ({BLOCK bt instr*})} ~> ({LABEL_ n `{epsilon} val^k instr*})
    -- iff bt = t_1^k -> t_2^n
1. Assert: Due to validation, a value is on the top of the stack.
2. Pop the value val^k from the stack.
3. Let bt = t_1^k -> t_2^n.
4. Let L be the label whose arity is n and whose continuation is the end of this instruction.
5. Enter the block val^k instr* with label L.

{val^k ({LOOP bt instr*})} ~> ({LABEL_ n `{{LOOP bt instr*}} val^k instr*})
    -- iff bt = t_1^k -> t_2^n
1. Assert: Due to validation, a value is on the top of the stack.
2. Pop the value val^k from the stack.
3. Let bt = t_1^k -> t_2^n.
4. Let L be the label whose arity is n and whose continuation is the start of this instruction.
5. Enter the block val^k instr* with label L.

{({CONST I32 c}) ({IF bt instr_1* ELSE instr_2*})} ~> ({BLOCK bt instr_1*})
    -- iff c =/= 0
{({CONST I32 c}) ({IF bt instr_1* ELSE instr_2*})} ~> ({BLOCK bt instr_2*})
    -- iff c = 0
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 c}) from the stack.
3. If c =/= 0, then:
  1) Execute the instruction BLOCK bt instr_1*.
4. If c = 0, then:
  1) Execute the instruction BLOCK bt instr_2*.

({LABEL_ n `{instr'*} val'* val^n ({BR 0}) instr*}) ~> {val^n instr'*}
1. YET: Bubble-up semantics.
2. Push the values val^n to the stack.
3. Push the values instr'* to the stack.

({LABEL_ n `{instr'*} val* ({BR l + 1}) instr*}) ~> {val* ({BR l})}
1. YET: Bubble-up semantics.
2. Push the values val* to the stack.
3. Execute the instruction BR l.

{({CONST I32 c}) ({BR_IF l})} ~> ({BR l})
    -- iff c =/= 0
{({CONST I32 c}) ({BR_IF l})} ~> epsilon
    -- iff c = 0
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 c}) from the stack.
3. If c =/= 0, then:
  1) Execute the instruction BR l.
4. If c = 0, then:
  1) Do nothing.

{({CONST I32 i}) ({BR_TABLE l* l'})} ~> ({BR l*[i]})
    -- iff i < |l*|
{({CONST I32 i}) ({BR_TABLE l* l'})} ~> ({BR l'})
    -- iff i >= |l*|
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 i}) from the stack.
3. If i < |l*|, then:
  1) Let tmp0 be l*[i].
  2) Execute the instruction BR tmp0.
4. If i >= |l*|, then:
  1) Execute the instruction BR l'.

z ; ({REF.FUNC x}) ~> ({REF.FUNC_ADDR $funcaddr(z)[x]})
1. Let tmp0 be the result of computing $funcaddr(z).
2. Let tmp1 be tmp0[x].
3. Push the value REF.FUNC_ADDR tmp1 to the stack.

z ; ({LOCAL.GET x}) ~> $local(z, x)
1. Let tmp0 be the result of computing $local(z, x).
2. Push the value tmp0 to the stack.

z ; ({GLOBAL.GET x}) ~> $global(z, x)
1. Let tmp0 be the result of computing $global(z, x).
2. Push the value tmp0 to the stack.

z ; {({CONST I32 i}) ({TABLE.GET x})} ~> TRAP
    -- iff i >= |$table(z, x)|
z ; {({CONST I32 i}) ({TABLE.GET x})} ~> $table(z, x)[i]
    -- iff i < |$table(z, x)|
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 i}) from the stack.
3. If i >= |$table(z, x)|, then:
  1) Trap.
4. If i < |$table(z, x)|, then:
  1) Let tmp0 be the result of computing $table(z, x).
  2) Let tmp1 be tmp0[i].
  3) Push the value tmp1 to the stack.

z ; ({TABLE.SIZE x}) ~> ({CONST I32 n})
    -- iff |$table(z, x)| = n
1. Let |$table(z, x)| = n.
2. Push the value CONST I32 n to the stack.

z ; {({CONST I32 i}) val ({CONST I32 n}) ({TABLE.FILL x})} ~> TRAP
    -- iff i + n > |$table(z, x)|
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 n}) from the stack.
3. Assert: Due to validation, a value is on the top of the stack.
4. Pop the value val from the stack.
5. Assert: Due to validation, a value of value type i32 is on the top of the stack.
6. Pop the value ({CONST I32 i}) from the stack.
7. Let i + n > |$table(z, x)|.
8. Trap.

z ; {({CONST I32 i}) val ({CONST I32 0}) ({TABLE.FILL x})} ~> epsilon
    -- otherwise
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 0}) from the stack.
3. Assert: Due to validation, a value is on the top of the stack.
4. Pop the value val from the stack.
5. Assert: Due to validation, a value of value type i32 is on the top of the stack.
6. Pop the value ({CONST I32 i}) from the stack.

z ; {({CONST I32 i}) val ({CONST I32 n + 1}) ({TABLE.FILL x})} ~> {({CONST I32 i}) val ({TABLE.SET x}) ({CONST I32 i + 1}) val ({CONST I32 n}) ({TABLE.FILL x})}
    -- otherwise
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 n + 1}) from the stack.
3. Assert: Due to validation, a value is on the top of the stack.
4. Pop the value val from the stack.
5. Assert: Due to validation, a value of value type i32 is on the top of the stack.
6. Pop the value ({CONST I32 i}) from the stack.
7. Push the value CONST I32 i to the stack.
8. Push the value val to the stack.
9. Execute the instruction TABLE.SET x.
10. Push the value CONST I32 (i + 1) to the stack.
11. Push the value val to the stack.
12. Push the value CONST I32 n to the stack.
13. Execute the instruction TABLE.FILL x.

z ; {({CONST I32 j}) ({CONST I32 i}) ({CONST I32 n}) ({TABLE.COPY x y})} ~> TRAP
    -- iff i + n > |$table(z, y)| \/ j + n > |$table(z, x)|
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 n}) from the stack.
3. Assert: Due to validation, a value of value type i32 is on the top of the stack.
4. Pop the value ({CONST I32 i}) from the stack.
5. Assert: Due to validation, a value of value type i32 is on the top of the stack.
6. Pop the value ({CONST I32 j}) from the stack.
7. Let i + n > |$table(z, y)| \/ j + n > |$table(z, x)|.
8. Trap.

z ; {({CONST I32 j}) ({CONST I32 i}) ({CONST I32 0}) ({TABLE.COPY x y})} ~> epsilon
    -- otherwise
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 0}) from the stack.
3. Assert: Due to validation, a value of value type i32 is on the top of the stack.
4. Pop the value ({CONST I32 i}) from the stack.
5. Assert: Due to validation, a value of value type i32 is on the top of the stack.
6. Pop the value ({CONST I32 j}) from the stack.

z ; {({CONST I32 j}) ({CONST I32 i}) ({CONST I32 n + 1}) ({TABLE.COPY x y})} ~> {({CONST I32 j}) ({CONST I32 i}) ({TABLE.GET y}) ({TABLE.SET x}) ({CONST I32 j + 1}) ({CONST I32 i + 1}) ({CONST I32 n}) ({TABLE.COPY x y})}
    -- iff j <= i
z ; {({CONST I32 j}) ({CONST I32 i}) ({CONST I32 n + 1}) ({TABLE.COPY x y})} ~> {({CONST I32 j + n}) ({CONST I32 i + n}) ({TABLE.GET y}) ({TABLE.SET x}) ({CONST I32 j + 1}) ({CONST I32 i + 1}) ({CONST I32 n}) ({TABLE.COPY x y})}
    -- iff i > i
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 n + 1}) from the stack.
3. Assert: Due to validation, a value of value type i32 is on the top of the stack.
4. Pop the value ({CONST I32 i}) from the stack.
5. Assert: Due to validation, a value of value type i32 is on the top of the stack.
6. Pop the value ({CONST I32 j}) from the stack.
7. If j <= i, then:
  1) Push the value CONST I32 j to the stack.
  2) Push the value CONST I32 i to the stack.
  3) Execute the instruction TABLE.GET y.
  4) Execute the instruction TABLE.SET x.
  5) Push the value CONST I32 (j + 1) to the stack.
  6) Push the value CONST I32 (i + 1) to the stack.
  7) Push the value CONST I32 n to the stack.
  8) Execute the instruction TABLE.COPY x y.
8. If i > i, then:
  1) Push the value CONST I32 (j + n) to the stack.
  2) Push the value CONST I32 (i + n) to the stack.
  3) Execute the instruction TABLE.GET y.
  4) Execute the instruction TABLE.SET x.
  5) Push the value CONST I32 (j + 1) to the stack.
  6) Push the value CONST I32 (i + 1) to the stack.
  7) Push the value CONST I32 n to the stack.
  8) Execute the instruction TABLE.COPY x y.

z ; {({CONST I32 j}) ({CONST I32 i}) ({CONST I32 n}) ({TABLE.INIT x y})} ~> TRAP
    -- iff i + n > |$elem(z, y)| \/ j + n > |$table(z, x)|
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 n}) from the stack.
3. Assert: Due to validation, a value of value type i32 is on the top of the stack.
4. Pop the value ({CONST I32 i}) from the stack.
5. Assert: Due to validation, a value of value type i32 is on the top of the stack.
6. Pop the value ({CONST I32 j}) from the stack.
7. Let i + n > |$elem(z, y)| \/ j + n > |$table(z, x)|.
8. Trap.

z ; {({CONST I32 j}) ({CONST I32 i}) ({CONST I32 0}) ({TABLE.INIT x y})} ~> epsilon
    -- otherwise
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 0}) from the stack.
3. Assert: Due to validation, a value of value type i32 is on the top of the stack.
4. Pop the value ({CONST I32 i}) from the stack.
5. Assert: Due to validation, a value of value type i32 is on the top of the stack.
6. Pop the value ({CONST I32 j}) from the stack.

z ; {({CONST I32 j}) ({CONST I32 i}) ({CONST I32 n + 1}) ({TABLE.INIT x y})} ~> {({CONST I32 j}) $elem(z, y)[i] ({TABLE.SET x}) ({CONST I32 j + 1}) ({CONST I32 i + 1}) ({CONST I32 n}) ({TABLE.INIT x y})}
    -- otherwise
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 n + 1}) from the stack.
3. Assert: Due to validation, a value of value type i32 is on the top of the stack.
4. Pop the value ({CONST I32 i}) from the stack.
5. Assert: Due to validation, a value of value type i32 is on the top of the stack.
6. Pop the value ({CONST I32 j}) from the stack.
7. Push the value CONST I32 j to the stack.
8. Let tmp0 be the result of computing $elem(z, y).
9. Let tmp1 be tmp0[i].
10. Push the value tmp1 to the stack.
11. Execute the instruction TABLE.SET x.
12. Push the value CONST I32 (j + 1) to the stack.
13. Push the value CONST I32 (i + 1) to the stack.
14. Push the value CONST I32 n to the stack.
15. Execute the instruction TABLE.INIT x y.

z ; ({CALL x}) ~> ({CALL_ADDR $funcaddr(z)[x]})
1. Let tmp0 be the result of computing $funcaddr(z).
2. Let tmp1 be tmp0[x].
3. Execute the instruction CALL_ADDR tmp1.

z ; {({CONST I32 i}) ({CALL_INDIRECT x ft})} ~> ({CALL_ADDR a})
    -- iff $table(z, x)[i] = ({REF.FUNC_ADDR a})
    -- iff $funcinst(z)[a] = m ; func
z ; {({CONST I32 i}) ({CALL_INDIRECT x ft})} ~> TRAP
    -- otherwise
1. Assert: Due to validation, a value of value type i32 is on the top of the stack.
2. Pop the value ({CONST I32 i}) from the stack.
3. If $table(z, x)[i] = ({REF.FUNC_ADDR a}) and $funcinst(z)[a] = m ; func, then:
  1) Execute the instruction CALL_ADDR a.
4. If otherwise, then:
  1) Trap.

z ; {val^k ({CALL_ADDR a})} ~> ({FRAME_ n `{{LOCAL {val^k ($default_(t))*}, MODULE m}} ({LABEL_ n `{epsilon} instr*})})
    -- iff $funcinst(z)[a] = m ; {FUNC (t_1^k -> t_2^n) t* instr*}
1. Assert: Due to validation, a value is on the top of the stack.
2. Pop the value val^k from the stack.
3. Let $funcinst(z)[a] = m ; {FUNC (t_1^k -> t_2^n) t* instr*}.
4. Let F be the frame `{{LOCAL {val^k ($default_(t))*}, MODULE m}}.
5. Push the activation of F with the arity n to the stack.
6. Let L be the label whose arity is n and whose continuation is the end of this instruction.
7. Enter the block instr* with label L.

z ; {val ({LOCAL.SET x})} ~> $with_local(z, x, val) ; epsilon
1. Assert: Due to validation, a value is on the top of the stack.
2. Pop the value val from the stack.
3. Let tmp0 be $with_local(z, x, val) ; epsilon.
4. Push the value tmp0 to the stack.

z ; {val ({GLOBAL.SET x})} ~> $with_global(z, x, val) ; epsilon
1. Assert: Due to validation, a value is on the top of the stack.
2. Pop the value val from the stack.
3. Let tmp0 be $with_global(z, x, val) ; epsilon.
4. Push the value tmp0 to the stack.

z ; {({CONST I32 i}) ref ({TABLE.GET x})} ~> $with_table(z, x, i, ref) ; epsilon
    -- iff i < |$table(z, x)|
z ; {({CONST I32 i}) ref ({TABLE.GET x})} ~> z ; TRAP
    -- iff i >= |$table(z, x)|
1. Assert: Due to validation, a value is on the top of the stack.
2. Pop the value ref from the stack.
3. Assert: Due to validation, a value of value type i32 is on the top of the stack.
4. Pop the value ({CONST I32 i}) from the stack.
5. If i < |$table(z, x)|, then:
  1) Let tmp0 be $with_table(z, x, i, ref) ; epsilon.
  2) Push the value tmp0 to the stack.
6. If i >= |$table(z, x)|, then:
  1) Let tmp1 be z ; TRAP.
  2) Push the value tmp1 to the stack.


== Complete.
```

# Bugs of wasmtime found by bottom-up west

* runtime info
    - version: 30.0.2 (398694a59 2025-02-25)
    - system: Ubuntu 20.04.6 LTS, x86\_64

## 1. Thread panic when v128 array reference is used as table initializer

When the reference for v128 array is used as as table initializer,
the instantiation results in a thread panic.

* fuzzer info
    - approach: bottom-up
    - commit: 6a5095741cf0708a934714983c292768b1393bea
    - seed: 547, 576, ...
* [minimal](v128_array_ref_table_initializer.wast)
```wat
(module
  (type $t (array v128))
  (table 0 (ref array)
    (i32.const 0)
    (array.new_default $t)
  )
)
```

## 2. Out of bounds table access does not trap for none reference

Accessing out of bounds element from the table should result in trap,
but if the table uses `none` reference type,
the access does not trap.

* fuzzer info
    - approach: bottom-up
    - commit: 6a5095741cf0708a934714983c292768b1393bea
    - seed: 4213, ...
* [minimal](out_of_bound_table_none_reference.wast)
```wat
(module
  (table $t 10 (ref null none))
  (func (export "f") (result (ref null none))
    (i32.const 99)
    (table.get $t)
  )
)
(assert_trap (invoke "f") "out of bounds table access")
```

# Nondeterminism

## Nondeterministic `relaxed_laneselect`

* fuzzer info
    - approach: bottom-up
    - commit: 6a5095741cf0708a934714983c292768b1393bea
    - seed: 1159, ...
* [minimal](relaxed_laneselect.wast)
```wat
(module
  (func (export "f") (result v128)
    (v128.const i64x2 0 0)
    (v128.const i64x2 1 0)
    (v128.const i64x2 1 0)
    (v128.not)
    ;; 00000000   (select this if flag is 1)
    ;; 00000001   (select this if flag is 0)
    ;; 11111110   (this is flag)
    (i64x2.relaxed_laneselect)
    ;; either
    ;; 00000001   (normal bitwise select)
    ;; 00000000   (select first entirely)
  )
)
(assert_return
  (invoke "f")
  (v128.const i64x2 1 0)
)
```

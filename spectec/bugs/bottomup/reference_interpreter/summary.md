# Bugs of reference interpreter found by bottom-up west

* runtime info
    - commit: spec/wasm-3.0 b72187b3c

## 1. Validation error for return call indirect

When the wasm instruction `return_call_indirect` uses a non-function reference type from the table,
the module is expected to be invalid.
However, the reference interpreter incorrectly considers it a valid input.

Note: this bug was discovered by accident. West tries to automatically generate **valid** module,
but sometimes, it fails to do so.
As a safeguard, west uses reference interpreter as an oracle, for determining if the generated module is valid.
In this case, this test was supposed to be filtered, but the wrong reference interpreter thought that this module was valid,
resulting in falied test in the other runtimes.
If west were *perfect*, then this bug would not have been caught.

* fuzzer info
    - approach: bottom-up
    - commit: 6a5095741cf0708a934714983c292768b1393bea
    - seed: 2547, 116122, ...
* [minimal](invalid_return_call_indirect.wast)
```wat
(assert_invalid
  (module
    (table 0 (ref null i31))
    (func
      (i32.const 0)
      (return_call_indirect)
    )
  )
  ""
)
```

## 2. `array.new_data` on short data results in un uncaught exception

When the wasm instruction `array.new_data`is performed on a short data,
the instruction should result in a trap,
but it results in a 

* fuzzer info
    - approach: bottom-up
    - commit: 6a5095741cf0708a934714983c292768b1393bea
    - seed: 676, ...
* [minimal](array.new_data_short.wast)
```wat
(module
  (type $v128_array (array v128))
  (func (export "f") (result (ref $v128_array))
    (i32.const 0)
    (i32.const 1)
    (array.new_data $v128_array $short_data)
  )
  (data $short_data "0123456789ABCDE")
  ;;(data $long_data "0123456789ABCDEF") : normal
)
(assert_trap (invoke "f") "")
```

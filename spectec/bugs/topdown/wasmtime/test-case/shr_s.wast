(module
  (func (export "shr_r") (result v128)
    (v128.const i64x2 0 0x200000000)
    (i32.const 33)
    (i64x2.shr_s)
  )
)
(assert_return (invoke "shr_r") (v128.const i64x2 0 1))

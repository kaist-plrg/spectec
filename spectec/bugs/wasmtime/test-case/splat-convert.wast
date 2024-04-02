(module
  (func (export "foo") (result v128)
    (i32.const 0)
    (i32x4.splat)
    (f64x2.convert_low_i32x4_u)
  )
)
(assert_return (invoke "foo") (v128.const i16x8 0 0 0 0 0 0 0 0))

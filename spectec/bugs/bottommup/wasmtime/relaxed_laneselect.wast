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
    ;; 00000001   (bitwise select)
    ;; 00000000   (lanewise select, based on top bit)
  )
)
(assert_return
  (invoke "f")
  (v128.const i64x2 1 0)
)

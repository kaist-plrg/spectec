(module $spectest
    (global $global_i32 (export "global_i32") i32 (i32.const 666))
    (global $global_i64 (export "global_i64") i64 (i64.const 666))
    (global $global_f32 (export "global_f32") f32 (f32.const 666.6))
    (global $global_f64 (export "global_f64") f64 (f64.const 666.6))

    (func (export "print_i32") (param i32) (nop))
    (func (export "print_i64") (param i64) (nop))
    (func (export "print_f32") (param f32) (nop))
    (func (export "print_f64") (param f64) (nop))
)
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

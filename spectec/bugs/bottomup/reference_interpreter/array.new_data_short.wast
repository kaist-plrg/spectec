(module
  (type $arr (array v128))
  (func (export "f") (result (ref $arr))
    (i32.const 0)
    (i32.const 1)
    (array.new_data $arr $short_data)
  )
  (data $short_data "123")
  ;;(data $long_data "1234") : normal
)
(assert_trap (invoke "f") "")

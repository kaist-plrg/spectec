(module
  (type $t (array v128))
  (table 0 (ref array)
    (i32.const 0)
    (array.new_default $t)
  )
)

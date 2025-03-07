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

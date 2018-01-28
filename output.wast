(module
  (export "fib" (func $fib))
  (func $fib (param $i i32) (result i32)
    (return
      (if (result i32)
        (i32.eq
          (get_local $i)
          (i32.const 0)
        )
        (i32.const 1)
        (if (result i32)
          (i32.eq
            (get_local $i)
            (i32.const 1)
          )
          (i32.const 1)
          (if (result i32)
            (i32.eq
              (get_local $i)
              (get_local $i)
            )
            (i32.add
              (call $fib
                (i32.sub
                  (get_local $i)
                  (i32.const 2)
                )
              )
              (call $fib
                (i32.sub
                  (get_local $i)
                  (i32.const 1)
                )
              )
            )
            (i32.const 0)
          )
        )
      )
    )
  )
  
  (export "main" (func $main))
  (func $main (result i32)
    (return
      (call $fib
        (i32.const 10)
      )
    )
  )
)

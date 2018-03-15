(module  
  (export "red" (func $red))
  (func $red (param $x i32)  (param $y i32)  (param $t i32) (result i32) 
    (return
      (i32.add
        (i32.add
          (get_local $x)
          (get_local $y)
        )
        (i32.div_s
          (get_local $t)
          (i32.const 3)
        )
      )
    )
  )
  
  
  (export "blue" (func $blue))
  (func $blue (param $x i32)  (param $y i32)  (param $t i32) (result i32) 
    (return
      (i32.add
        (get_local $y)
        (i32.div_s
          (get_local $x)
          (i32.const 2)
        )
      )
    )
  )
  
  
  (export "green" (func $green))
  (func $green (param $x i32)  (param $y i32)  (param $t i32) (result i32) 
    (return
      (i32.mul
        (i32.mul
          (get_local $x)
          (get_local $y)
        )
        (i32.div_s
          (get_local $t)
          (i32.const 10)
        )
      )
    )
  )
)

(module  
  (export "red" (func $red))
  (func $red (param $x i32)  (param $y i32)  (param $t i32) (result i32) 
    (return
      (get_local $y)
    )
  )
  
  
  (export "blue" (func $blue))
  (func $blue (param $x i32)  (param $y i32)  (param $t i32) (result i32) 
    (return
      (get_local $x)
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
        (get_local $t)
      )
    )
  )
)

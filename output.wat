(module
  (global $freeblock (mut i32) (i32.const 0))
  
  
  
  (export "malloc" (func $malloc))
  (func $malloc (param $size i32) (result i32)
    (local $address i32)
    (set_local $address (get_global $freeblock))
    (set_global $freeblock (i32.add (get_local $address) (get_local $size)))
    (return (get_local $address))
  )
  
  (func $string_copy (param $from i32) (param $to i32) (result i32)
    (local $index i32)
    (local $size i32)
  
    (set_local $index (i32.const 1))
    (set_local $size (i32.load8_u (get_local $from)))
  
    (loop $copy
      (i32.store8
        (i32.add (get_local $to) (get_local $index))
        (i32.load8_u (i32.add (get_local $from) (get_local $index)))
      )
      (set_local $index (i32.add (get_local $index) (i32.const 1)))
      (br_if $copy (i32.lt_s (get_local $index) (get_local $size)))
    )
  
    (return (get_local $size))
  )
  
  (func $string_add (param $a i32) (param $b i32) (result i32)
    (local $sum i32)
    (local $aSize i32)
    (local $newStr i32)
    (return
      (set_local $aSize (i32.load8_u (get_local $a)))
      (set_local $sum
        (i32.sub
          (i32.add
            (get_local $aSize)
            (i32.load8_u (get_local $b))
          )
          (i32.const 1)
        )
      )
      (set_local $newStr (call $malloc (i32.add (get_local $sum) (i32.const 1))))
      (i32.store8 (get_local $newStr) (get_local $sum))
      (call $string_copy (get_local $a) (get_local $newStr))
      (call $string_copy (get_local $b) (i32.sub (i32.add (get_local $newStr) (get_local $aSize)) (i32.const 1)))
      (get_local $newStr)
    )
  )

  (memory $memory 1)
  (export "memory" (memory $memory))
  
  (export "main" (func $main))
  (func $main (result i32) 
    (return
      (call $test
        (call $Just
          (i32.const 10)
        )
      )
    )
  )
  
  (export "test" (func $test))
  (func $test (param $m i32) (result i32) (local $a i32)
    (return
      (if (result i32)
        (i32.eq
          (i32.load
            (get_local $m)
          )
          (set_local $a (i32.load
            (i32.const 4)
          ))
          (i32.const 0)
        )
        (get_local $a)
        (if (result i32)
          (i32.eq
            (i32.load
              (get_local $m)
            )
            (i32.const 1)
          )
          (i32.const 5)
          (i32.const 0)
        )
      )
    )
  )
  
  (export "Just" (func $Just))
  (func $Just (param $a i32) (result i32) (local $address i32)
    (return
      (set_local $address (call $malloc
        (i32.const 8)
      ))
      (i32.store
        (get_local $address)
        (i32.const 0)
      )
      (i32.store
        (i32.add
          (get_local $address)
          (i32.const 4)
        )
        (get_local $a)
      )
      (get_local $address)
    )
  )
  
  (export "Nothing" (func $Nothing))
  (func $Nothing (result i32) (local $address i32)
    (return
      (set_local $address (call $malloc
        (i32.const 4)
      ))
      (i32.store
        (get_local $address)
        (i32.const 1)
      )
      (get_local $address)
    )
  )
)

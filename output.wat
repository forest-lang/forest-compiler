(module
  (memory $memory 1)
  (export "memory" (memory $memory))

  (global $freeblock (mut i32) (i32.const 0))

  (func $malloc (param $size i32) (result i32)
    (local $address i32)
    (set_local $address (get_global $freeblock))
    (set_global $freeblock (i32.add (get_local $address) (get_local $size)))
    (return (get_local $address))
  )

  (func $test (param $m i32) (result i32)
    (return
      (if (result i32)
        (i32.eq
          (i32.load (get_local $m))
          (i32.const 0)
        )
        (i32.load (i32.add (get_local $m) (i32.const 4)))
        (if (result i32)
          (i32.eq
            (i32.load (get_local $m))
            (i32.const 1)
          )
          (i32.const 0)
          (i32.const 0)
        )
      )
    )
  )

  (func $Just (param $m i32) (result i32)
    (local $address i32)
    (return
      (set_local $address (call $malloc (i32.const 8)))
      (i32.store (get_local $address) (i32.const 0))
      (i32.store (i32.add (get_local $address) (i32.const 4)) (get_local $m))
      (get_local $address)
    )
  )

  (func $Nothing (result i32)
    (local $address i32)
    (return
      (set_local $address (call $malloc (i32.const 4)))
      (i32.store (get_local $address) (i32.const 1))
      (get_local $address)
    )
  )

  (export "main" (func $main))
  (func $main (result i32)
    (return
      (call $Just (i32.const 3))
      (call $test (call $Just (i32.const 5)))
    )
  )
)

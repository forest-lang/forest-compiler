require 'tempfile'

def assert_equal(actual, expected, message)
  raise message unless actual == expected
end

def test_fibonacci
  wast = `stack exec forest-compiler-exe ./samples/fib.tree`

  Tempfile.open('fib.wast') do |f|
    f.write(wast)
    f.close

    `wavm #{f.path}`

    exitcode = $?.exitstatus

    assert_equal(
      exitcode,
      89,
      "Expected fib(10) to return 89 but instead got #{exitcode}"
    )
  end
end

def run_tests
  test_fibonacci

  puts 'Integration tests ran successfully!'
end

run_tests

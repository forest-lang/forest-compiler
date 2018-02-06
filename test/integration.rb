require 'tempfile'

def assert_equal(actual, expected, message)
  raise message unless actual == expected
end

def test(name, result)
  wast = `stack exec forest-compiler-exe ./samples/#{name}.tree`

  Tempfile.open("#{name}.wast") do |f|
    f.write(wast)
    f.close

    `wavm #{f.path}`

    exitcode = $?.exitstatus

    assert_equal(
      exitcode,
      result,
      "Expected #{name} to return #{result} but instead got #{exitcode}"
    )
  end
end

def run_tests
  test('fib', 89)
  test('let', 15)

  puts 'Integration tests ran successfully!'
end

run_tests

program test_string
  use earthvm_string, only: string
  print *, string('Hello')
  print *, [string('Hello'), string('world!')]
end program test_string

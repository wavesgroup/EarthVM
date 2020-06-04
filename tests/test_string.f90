program test_str
  use earthvm_str, only: str
  print *, str('Hello')
  print *, [str('Hello'), str('world!')]
end program test_str

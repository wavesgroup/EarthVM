program test_earthvm_coupling
  use earthvm_coupling, only: get_coupling, set_coupling
  call set_coupling('1', '2', '3', '4')
  print *, get_coupling()
  call set_coupling('a', 'b', 'c', 'd')
  print *, get_coupling()
  call set_coupling('spam', 'eggs', 'ham', '1234567890')
  print *, get_coupling()
  call set_coupling('wrf', 'u10', 'umwm', 'u')
  print *, get_coupling()
  call set_coupling('wrf', 'v10', 'umwm', 'v')
  print *, get_coupling()
  call set_coupling('wrf', 'taux', 'hycom', 'stressx')
  print *, get_coupling()
  print *, get_coupling(src_model='wrf')
  print *, get_coupling(dst_model='umwm')
  print *, get_coupling(dst_field='stressx')
end program test_earthvm_coupling

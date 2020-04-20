program test_earthvm_model_constructor
  use earthvm_state, only: earthvm_initialize
  use earthvm_model_wrf, only: earthvm_model_wrf_type
  implicit none
  type(earthvm_model_wrf_type) :: model
  call earthvm_initialize()
  model = earthvm_model_wrf_type('wrf')
  if (model % name /= 'wrf') error stop 'test_earthvm_model_constructor failed.'
end program test_earthvm_model_constructor

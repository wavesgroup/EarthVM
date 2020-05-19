program test_earthvm_model_constructor
  use earthvm_state, only: earthvm_initialize
  use earthvm_model, only: earthvm_model_type
  use earthvm_wrf, only: set_services
  use earthvm_datetime, only: datetime
  implicit none
  type(earthvm_model_type) :: model
  call earthvm_initialize()
  model = earthvm_model_type('wrf', datetime(2020, 4, 1), datetime(2020, 4, 5), &
                             60, set_services)
  if (model % name /= 'wrf') error stop 'test_earthvm_model_constructor failed.'
end program test_earthvm_model_constructor

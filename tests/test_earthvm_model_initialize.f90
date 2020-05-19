program test_earthvm_model_initialize
  use earthvm_state, only: earthvm_initialize, earthvm_get_mpicomm
  use earthvm_model, only: earthvm_model_type
  use earthvm_wrf, only: set_services
  use earthvm_datetime, only: datetime
  implicit none
  type(earthvm_model_type) :: model
  call earthvm_initialize()
  model = earthvm_model_type('wrf', datetime(2020, 4, 1), datetime(2020, 4, 5), &
                             60, set_services)
  call wrf_set_dm_communicator(earthvm_get_mpicomm())
  call model % initialize()
end program test_earthvm_model_initialize

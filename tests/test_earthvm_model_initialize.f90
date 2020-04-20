program test_earthvm_model_initialize
  use earthvm_state, only: earthvm_initialize, earthvm_get_mpicomm
  use earthvm_model_wrf, only: earthvm_model_wrf_type
  implicit none
  type(earthvm_model_wrf_type) :: model
  call earthvm_initialize()
  model = earthvm_model_wrf_type('wrf')
  call wrf_set_dm_communicator(earthvm_get_mpicomm())
  call model % initialize()
end program test_earthvm_model_initialize

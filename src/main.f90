program main

  use earthvm_state, only: earthvm_initialize, earthvm_finalize, earthvm_get_mpicomm
  use earthvm_model_wrf, only: earthvm_model_wrf_type

  implicit none

  type(earthvm_model_wrf_type) :: atmosphere_model
  integer :: n, rc

  call earthvm_initialize()

  atmosphere_model = earthvm_model_wrf_type('wrf')

  call wrf_set_dm_communicator(earthvm_get_mpicomm())

  call atmosphere_model % initialize()
  !do n = 1, 10
  !  call atmosphere_model % run()
  !end do
  call atmosphere_model % finalize()

  call earthvm_finalize()

end program main

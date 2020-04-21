program main

  use earthvm_state, only: earthvm_initialize, earthvm_finalize, earthvm_get_mpicomm
  use earthvm_model, only: earthvm_model_type
  use earthvm_wrf, only: set_services
  use earthvm_esmf, only: datetime

  implicit none

  type(earthvm_model_type) :: atmosphere_model
  integer :: n

  call earthvm_initialize()

  atmosphere_model = earthvm_model_type('wrf',                 &
                                         datetime(2020, 4, 1), &
                                         datetime(2020, 4, 5), &
                                         60,                   &
                                         set_services)

  call wrf_set_dm_communicator(earthvm_get_mpicomm())

  call atmosphere_model % initialize()
  !do n = 1, 10
  !  call atmosphere_model % run()
  !end do
  call atmosphere_model % finalize()

  call earthvm_finalize()

end program main

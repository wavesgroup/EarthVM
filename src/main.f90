program main

  use earthvm_state, only: earthvm_initialize, earthvm_finalize
  use earthvm_model, only: earthvm_model_type
  use earthvm_esmf, only: datetime
  use earthvm_wrf, only: set_wrf_services => set_services
  use earthvm_hycom, only: set_hycom_services => set_services

  implicit none

  type(earthvm_model_type) :: atmosphere_model, ocean_model
  integer :: n

  call earthvm_initialize()

  atmosphere_model = earthvm_model_type('wrf',                      &
                                         datetime(2019, 11, 11, 0), &
                                         datetime(2019, 11, 11, 1), &
                                         40,                        &
                                         set_wrf_services)

  ocean_model = earthvm_model_type('hycom',              &
                                   datetime(2020, 4, 1), &
                                   datetime(2020, 4, 5), &
                                   60,                   &
                                   set_hycom_services)

  call atmosphere_model % initialize()
  !call ocean_model % initialize()

  do n = 1, 90
    call atmosphere_model % run()
  end do
  call atmosphere_model % finalize()

  call earthvm_finalize()

end program main

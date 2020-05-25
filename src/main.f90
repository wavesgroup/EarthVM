program main

  use earthvm_datetime, only: datetime, timedelta
  use earthvm_hycom, only: set_hycom_services => set_services
  use earthvm_model, only: earthvm_model_type
  use earthvm_state, only: earthvm_initialize, earthvm_finalize, &
                           earthvm_get_local_pet
  use earthvm_wrf, only: set_wrf_services => set_services

  implicit none

  type(earthvm_model_type) :: atmosphere_model, ocean_model
  type(datetime) :: start_time, stop_time, time
  integer :: local_pet

  start_time = datetime(2019, 8, 29)
  stop_time = datetime(2019, 8, 29, 1)

  call earthvm_initialize()
  local_pet = earthvm_get_local_pet()

  atmosphere_model = earthvm_model_type('wrf', start_time, stop_time, &
                                        45, set_wrf_services)
  ocean_model = earthvm_model_type('hycom', start_time, stop_time, &
                                   60, set_hycom_services)

  call atmosphere_model % initialize()
  call ocean_model % initialize()
  call ocean_model % force(atmosphere_model)

  time = start_time
  do

    ! run atmosphere for one time step
    if (atmosphere_model % get_current_time() <= time) then
      if (local_pet == 0) print *, &
        time % strftime('%Y-%m-%d %H:%M:%S'), ': Running atmosphere model'
      call atmosphere_model % run()
      call atmosphere_model % write_to_netcdf()
    end if

    ! run ocean for one time step
    if (ocean_model % get_current_time() <= time) then
      if (local_pet == 0) print *, &
        time % strftime('%Y-%m-%d %H:%M:%S'), ': Running ocean model'
      call ocean_model % run()
      call ocean_model % write_to_netcdf()
      call ocean_model % force(atmosphere_model)
    end if

    ! advance master clock
    time = time + timedelta(seconds=1)
    if (time > stop_time) exit

  end do

  call earthvm_finalize()

end program main

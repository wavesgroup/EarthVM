program main

  use earthvm_datetime, only: datetime, timedelta
  use earthvm_hycom, only: set_hycom_services => set_services
  use earthvm_model, only: earthvm_model_type
  use earthvm_state, only: earthvm_initialize, earthvm_finalize, &
                           earthvm_get_local_pet
  use earthvm_str, only: str
  use earthvm_umwm, only: set_umwm_services => set_services
  use earthvm_wrf, only: set_wrf_services => set_services

  implicit none

  type(earthvm_model_type) :: atmosphere, waves, ocean
  type(datetime) :: start_time, stop_time, time
  integer :: local_pet

  start_time = datetime(2019, 8, 29)
  stop_time = datetime(2019, 9, 7)

  call earthvm_initialize()
  local_pet = earthvm_get_local_pet()

  atmosphere = earthvm_model_type('wrf', start_time, stop_time, &
                                  45, set_wrf_services)
  waves = earthvm_model_type('waves', start_time, stop_time, &
                             60, set_umwm_services)
  ocean = earthvm_model_type('hycom', start_time, stop_time, &
                             60, set_hycom_services)

  call atmosphere % set_import_fields([str('sst')])
  call atmosphere % set_export_fields([str('taux'), str('tauy'), &
    str('rainrate'), str('shortwave_flux'), str('total_flux')])

  call ocean % set_import_fields([str('taux'), str('tauy'), &
    str('rainrate'), str('shortwave_flux'), str('total_flux')])
  call ocean % set_export_fields([str('sst')])

  call atmosphere % initialize()
  call ocean % initialize()
  call ocean % force(atmosphere)

  time = start_time
  do

    ! run atmosphere for one time step
    if (atmosphere % get_current_time() < time) then
      if (local_pet == 0) print *, &
        time % strftime('%Y-%m-%d %H:%M:%S'), ': Running atmosphere'
      call atmosphere % run()
      call atmosphere % force(ocean)
      call atmosphere % write_to_netcdf()
    end if

    ! run ocean for one time step
    if (ocean % get_current_time() < time) then
      if (local_pet == 0) print *, &
        time % strftime('%Y-%m-%d %H:%M:%S'), ': Running ocean'
      call ocean % run()
      call ocean % write_to_netcdf()
      call ocean % force(atmosphere)
    end if

    ! advance master clock
    time = time + timedelta(seconds=1)
    if (time > stop_time) exit

  end do

  call earthvm_finalize()

end program main

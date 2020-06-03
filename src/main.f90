program main

  use earthvm_datetime, only: datetime, timedelta
  use earthvm_hycom, only: set_hycom_services => set_services
  use earthvm_model, only: earthvm_model_type
  use earthvm_state, only: earthvm_initialize, earthvm_finalize, &
                           earthvm_get_local_pet
  use earthvm_string, only: string
  use earthvm_wrf, only: set_wrf_services => set_services

  implicit none

  type(earthvm_model_type) :: atmosphere, ocean
  type(datetime) :: start_time, stop_time, time
  integer :: local_pet

  start_time = datetime(2019, 8, 29)
  stop_time = datetime(2019, 9, 7)

  call earthvm_initialize()
  local_pet = earthvm_get_local_pet()

  atmosphere = earthvm_model_type('wrf', start_time, stop_time, &
                                  45, set_wrf_services)
  ocean = earthvm_model_type('hycom', start_time, stop_time, &
                             60, set_hycom_services)

  call atmosphere % set_import_fields([string('sst')])
  call atmosphere % set_export_fields([string('taux'), string('tauy'), &
    string('rainrate'), string('shortwave_flux'), string('total_flux')])

  call ocean % set_import_fields([string('taux'), string('tauy'), &
    string('rainrate'), string('shortwave_flux'), string('total_flux')])
  call ocean % set_export_fields([string('sst')])

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

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

  start_time = datetime(2019, 8, 29)
  stop_time = datetime(2019, 9, 7)

  call earthvm_initialize()

  atmosphere = earthvm_model_type('wrf', start_time, stop_time, &
                                  45, set_wrf_services)
  waves = earthvm_model_type('umwm', start_time, stop_time, &
                             60, set_umwm_services)
  ocean = earthvm_model_type('hycom', start_time, stop_time, &
                             60, set_hycom_services)

  !call atmosphere % set_forcing('sst', ocean, 'sst')

  call atmosphere % set_import_fields([str('sst'),      &
                                       str('taux_atm'), &
                                       str('tauy_atm')])
  
  call atmosphere % set_export_fields([str('rainrate'),       &
                                       str('shortwave_flux'), &
                                       str('total_flux'),     &
                                       str('wspd'),           &
                                       str('wdir'),           & 
                                       str('rhoa')])
  
  call waves % set_import_fields([str('wspd'), &
                                  str('wdir'), &
                                  str('rhoa'), &
                                  str('rhow'), &
                                  str('u'),    &
                                  str('v')])
  
  call waves % set_export_fields([str('u_stokes'), &
                                  str('v_stokes'), &
                                  str('taux_atm'), &
                                  str('tauy_atm'), &
                                  str('taux_ocn'), &
                                  str('tauy_ocn')])
  
  call ocean % set_import_fields([str('taux_ocn'),       &
                                  str('tauy_ocn'),       &
                                  str('rainrate'),       &
                                  str('shortwave_flux'), & 
                                  str('total_flux'),     &
                                  str('u_stokes'),       &
                                  str('v_stokes')])

  call ocean % set_export_fields([str('sst'),  &
                                  str('rhow'), &
                                  str('u'),    &
                                  str('v')])

  call atmosphere % initialize()
  call waves % initialize()
  call ocean % initialize()

  call atmosphere % force(ocean)
  call atmosphere % force(waves)
  call ocean % force(atmosphere)
  call ocean % force(waves)
      
  time = start_time
  do while (time <= stop_time)

    ! run atmosphere for one time step
    if (atmosphere % get_current_time() < time) then
      call atmosphere % run()
      call atmosphere % force(ocean)
      call atmosphere % force(waves)
      call atmosphere % write_to_netcdf()
    end if

    ! run waves for one time step
    if (waves % get_current_time() < time) then
      call waves % run()
      call waves % force(atmosphere)
      call waves % force(ocean)
      call waves % write_to_netcdf()
    end if

    ! run ocean for one time step
    if (ocean % get_current_time() < time) then
      call ocean % run()
      call ocean % force(atmosphere)
      call ocean % force(waves)
      call ocean % write_to_netcdf()
    end if

    ! advance master clock
    time = time + timedelta(seconds=1)

  end do

  call earthvm_finalize()

end program main

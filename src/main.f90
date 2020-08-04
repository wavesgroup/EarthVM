program main
  ! Fully coupled atmosphere-wave-ocean driver.
  use earthvm_datetime, only: datetime, timedelta
  use earthvm_model, only: earthvm_model_type
  use earthvm_state, only: earthvm_initialize, earthvm_finalize
  use earthvm_wrf, only: set_wrf_services => set_services
  use earthvm_umwm, only: set_umwm_services => set_services
  use earthvm_hycom, only: set_hycom_services => set_services

  implicit none

  type(earthvm_model_type) :: atmosphere, waves, ocean
  type(datetime) :: start_time, stop_time, time

  start_time = datetime(2019, 8, 29)
  stop_time = datetime(2019, 9, 7)

  call earthvm_initialize()

  atmosphere = earthvm_model_type('wrf', start_time, stop_time, 45, set_wrf_services)
  waves = earthvm_model_type('umwm', start_time, stop_time, 60, set_umwm_services)
  ocean = earthvm_model_type('hycom', start_time, stop_time, 60, set_hycom_services)

  ! Atmosphere coupling
  call atmosphere % set_forcing('wspd', waves, 'wspd')
  call atmosphere % set_forcing('wdir', waves, 'wdir')
  call atmosphere % set_forcing('rhoa', waves, 'rhoa')
  call atmosphere % set_forcing('shortwave_flux', ocean, 'shortwave_flux')
  call atmosphere % set_forcing('total_flux', ocean, 'total_flux')
  call atmosphere % set_forcing('rainrate', ocean, 'rainrate')
 
  ! Wave coupling
  call waves % set_forcing('taux_atm', atmosphere, 'taux_wav')
  call waves % set_forcing('tauy_atm', atmosphere, 'tauy_wav')
  call waves % set_forcing('u_stokes', atmosphere, 'u_stokes')
  call waves % set_forcing('v_stokes', atmosphere, 'v_stokes')
  call waves % set_forcing('taux_ocn', ocean, 'taux')
  call waves % set_forcing('tauy_ocn', ocean, 'tauy')
  call waves % set_forcing('u_stokes', ocean, 'u_stokes')
  call waves % set_forcing('v_stokes', ocean, 'v_stokes')

  ! Ocean coupling
  call ocean % set_forcing('sst', atmosphere, 'sst')
  call ocean % set_forcing('u', atmosphere, 'u_current')
  call ocean % set_forcing('v', atmosphere, 'v_current')
  call ocean % set_forcing('u', waves, 'u')
  call ocean % set_forcing('v', waves, 'v')
  call ocean % set_forcing('rhow', waves, 'rhow')

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

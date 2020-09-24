program main
  ! Coupled atmosphere-wave driver.
  use earthvm_datetime, only: datetime, timedelta
  use earthvm_model, only: earthvm_model_type
  use earthvm_state, only: earthvm_initialize, earthvm_finalize
  use earthvm_wrf, only: set_wrf_services => set_services, wrf_domains => domains
  use earthvm_umwm, only: set_umwm_services => set_services

  implicit none

  type(earthvm_model_type) :: atmosphere, waves
  type(datetime) :: start_time, stop_time, time

  start_time = datetime(2019, 8, 29)
  stop_time = datetime(2019, 9, 7)

  call earthvm_initialize()

  atmosphere = earthvm_model_type('wrf', start_time, stop_time, 45, set_wrf_services)
  waves = earthvm_model_type('umwm', start_time, stop_time, 45, set_umwm_services)
  
  ! Atmosphere coupling
  call atmosphere % set_forcing('wspd', waves, 'wspd')
  call atmosphere % set_forcing('wdir', waves, 'wdir')
  call atmosphere % set_forcing('rhoa', waves, 'rhoa')
 
  ! Wave coupling
  call waves % set_forcing('taux_atm', atmosphere, 'taux_wav')
  call waves % set_forcing('tauy_atm', atmosphere, 'tauy_wav')
  call waves % set_forcing('u_stokes', atmosphere, 'u_stokes')
  call waves % set_forcing('v_stokes', atmosphere, 'v_stokes')

  call atmosphere % initialize()
  call waves % initialize()

  call atmosphere % force(waves)
     
  time = start_time
  do while (time <= stop_time)

    ! run atmosphere for one time step
    if (atmosphere % get_current_time() < time) then
      call atmosphere % run()
      call atmosphere % force(waves)
      !call atmosphere % write_to_netcdf()
    end if

    ! run waves for one time step
    if (waves % get_current_time() < time) then
      call waves % run()
      call waves % force(wrf_domains)
      !call waves % write_to_netcdf()
    end if

    ! advance master clock
    time = time + timedelta(seconds=1)

  end do

  call earthvm_finalize()

end program main

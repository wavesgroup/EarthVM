program main
  ! Coupled atmosphere-ocean driver.
  use earthvm_datetime, only: datetime, timedelta
  use earthvm_model, only: earthvm_model_type
  use earthvm_state, only: earthvm_initialize, earthvm_finalize
  use earthvm_wrf, only: set_wrf_services => set_services
  use earthvm_hycom, only: set_hycom_services => set_services

  implicit none

  type(earthvm_model_type) :: atmosphere, ocean
  type(datetime) :: start_time, stop_time, time

  start_time = datetime(2019, 8, 30)
  stop_time = datetime(2019, 9, 7)

  call earthvm_initialize()

  atmosphere = earthvm_model_type('wrf', start_time, stop_time, 15, set_wrf_services)
  ocean = earthvm_model_type('hycom', start_time, stop_time, 15, set_hycom_services)

  ! Atmosphere coupling
  call atmosphere % set_forcing('shortwave_flux', ocean, 'shortwave_flux')
  call atmosphere % set_forcing('total_flux', ocean, 'total_flux')
  call atmosphere % set_forcing('rainrate', ocean, 'rainrate')
  call atmosphere % set_forcing('taux', ocean, 'taux')
  call atmosphere % set_forcing('tauy', ocean, 'tauy')
 
  ! Ocean coupling
  call ocean % set_forcing('sst', atmosphere, 'sst')
  call ocean % set_forcing('u', atmosphere, 'u_current')
  call ocean % set_forcing('v', atmosphere, 'v_current')

  call atmosphere % initialize()
  call ocean % initialize()

  call atmosphere % force(ocean)
  call ocean % force(atmosphere)
     
  time = start_time
  do while (time <= stop_time)

    ! run atmosphere for one time step
    if (atmosphere % get_current_time() < time) then
      call atmosphere % run()
      call atmosphere % force(ocean)
      !call atmosphere % write_to_netcdf()
    end if

    ! run ocean for one time step
    if (ocean % get_current_time() < time) then
      call ocean % run()
      call ocean % force(atmosphere)
      !call ocean % write_to_netcdf()
    end if

    ! advance master clock
    time = time + timedelta(seconds=1)

  end do

  call earthvm_finalize()

end program main

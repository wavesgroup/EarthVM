program main
  ! Uncoupled atmosphere ocean driver, initialized with ocean SST.
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
  
  ! Ocean coupling
  call ocean % set_forcing('sst', atmosphere, 'sst')
  call ocean % set_forcing('u', atmosphere, 'u_current')
  call ocean % set_forcing('v', atmosphere, 'v_current')

  call atmosphere % initialize()
  call ocean % initialize()

  call ocean % force(atmosphere)
     
  time = start_time
  do while (time <= stop_time)

    ! run atmosphere for one time step
    if (atmosphere % get_current_time() < time) then
      call atmosphere % run()
      !call atmosphere % write_to_netcdf()
    end if

    ! advance master clock
    time = time + timedelta(seconds=1)

  end do

  call earthvm_finalize()

end program main

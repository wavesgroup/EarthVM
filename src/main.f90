program main

  use ESMF
  use earthvm_datetime, only: datetime, timedelta
  use earthvm_hycom, only: set_hycom_services => set_services
  use earthvm_io, only: write_grid_to_netcdf, write_fields_to_netcdf
  use earthvm_model, only: earthvm_model_type
  use earthvm_regrid, only: earthvm_regrid_type
  use earthvm_state, only: earthvm_initialize, earthvm_finalize, &
                           earthvm_get_local_pet
  use earthvm_wrf, only: set_wrf_services => set_services

  implicit none

  type(earthvm_model_type) :: atmosphere_model, ocean_model
  type(datetime) :: start_time, stop_time, time
  integer :: local_pet

  real, pointer :: field_data(:,:)
  integer :: ub(2), lb(2)

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

  time = start_time
  do

    ! run atmosphere for one time step
    if (atmosphere_model % get_current_time() <= time) then
      if (local_pet == 0) print *, 'running atmosphere'
      call atmosphere_model % run()
      call atmosphere_model % write_to_netcdf()
    end if

    ! run ocean for one time step
    if (ocean_model % get_current_time() <= time) then
      if (local_pet == 0) print *, 'running ocean'
      call ocean_model % run()
      call ocean_model % write_to_netcdf()
    end if

    ! print and advance master clock
    if (local_pet == 0) print *, time % strftime('%Y-%m-%d %H:%M:%S')
    time = time + timedelta(seconds=1)
    if (time > stop_time) exit

  end do

  call earthvm_finalize()

end program main

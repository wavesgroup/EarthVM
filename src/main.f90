program main

  use ESMF
  use earthvm_datetime, only: datetime
  use earthvm_hycom, only: set_hycom_services => set_services
  use earthvm_io, only: write_grid_to_netcdf, write_fields_to_netcdf
  use earthvm_model, only: earthvm_model_type
  use earthvm_regrid, only: earthvm_regrid_type
  use earthvm_state, only: earthvm_initialize, earthvm_finalize, &
                           earthvm_get_pet_count, earthvm_get_local_pet
  use earthvm_wrf, only: set_wrf_services => set_services

  implicit none

  type(earthvm_model_type) :: atmosphere_model, ocean_model
  type(datetime) :: start_time, stop_time, time
  integer :: local_pet, pet_count

  real, pointer :: field_data(:,:)
  integer :: ub(2), lb(2)

  start_time = datetime(2019, 8, 29)
  stop_time = datetime(2019, 8, 29, 1)

  call earthvm_initialize()
  local_pet = earthvm_get_local_pet()
  pet_count = earthvm_get_pet_count()

  atmosphere_model = earthvm_model_type('wrf', start_time, stop_time, &
                                        45, set_wrf_services)
  ocean_model = earthvm_model_type('hycom', start_time, stop_time, &
                                   60, set_hycom_services)

  call atmosphere_model % initialize()
  call ocean_model % initialize()

  do
    call ocean_model % run()
    time = ocean_model % get_current_time()
    if (local_pet == 0) print *, 'Ocean model: ', time % strftime('%Y-%m-%d %H:%M:%S')
    call ocean_model % write_to_netcdf()
    if (time >= stop_time) exit
  end do

  do
    call atmosphere_model % run()
    time = atmosphere_model % get_current_time()
    if (local_pet == 0) print *, 'Atmosphere model: ', time % strftime('%Y-%m-%d %H:%M:%S')
    call atmosphere_model % get_field_data('u10', field_data, lb, ub)
    if (local_pet == 0) &
      print *, 'u10 min/max: ', local_pet, &
      minval(field_data(lb(1):ub(1), lb(2):ub(2))), &
      maxval(field_data(lb(1):ub(1), lb(2):ub(2)))
    call atmosphere_model % write_to_netcdf()
    if (time >= stop_time) exit
  end do
  call atmosphere_model % finalize()

  call earthvm_finalize()

end program main

program main

  use ESMF
  use earthvm_state, only: earthvm_initialize, earthvm_finalize, earthvm_get_pet_count, earthvm_get_local_pet
  use earthvm_model, only: earthvm_model_type
  use earthvm_esmf, only: datetime, create_distgrid, create_grid, create_field
  use earthvm_regrid, only: earthvm_regrid_type
  use earthvm_io, only: write_grid_to_netcdf, write_fields_to_netcdf
  use earthvm_wrf, only: set_wrf_services => set_services
  use earthvm_hycom, only: set_hycom_services => set_services

  implicit none

  type(earthvm_model_type) :: atmosphere_model, ocean_model
  integer :: n

  integer :: local_pet, pet_count

  character(4) :: counter = '0000'

  type(earthvm_regrid_type) :: regrid
  real, pointer :: field_data(:,:)
  integer :: ub(2), lb(2)

  call earthvm_initialize()
  local_pet = earthvm_get_local_pet()
  pet_count = earthvm_get_pet_count()

  atmosphere_model = earthvm_model_type('wrf',                      &
                                         datetime(2019, 11, 11, 0), &
                                         datetime(2019, 11, 11, 1), &
                                         40,                        &
                                         set_wrf_services)

  ocean_model = earthvm_model_type('hycom',               &
                                   datetime(2019, 8, 29), &
                                   datetime(2019, 8, 30), &
                                   120,                   &
                                   set_hycom_services)

  call atmosphere_model % initialize()
  call ocean_model % initialize()
  call write_fields_to_netcdf([ocean_model % get_field('sst')], &
                               'hycom_' // counter // '.nc')
  call earthvm_finalize()
  stop

  do n = 1, 90
    call atmosphere_model % run()
    call ESMF_FieldGet(atmosphere_model % get_field('u10'), farrayPtr=field_data, &
                       exclusiveLBound=lb, exclusiveUBound=ub)
    if (local_pet == 0) &
      print *, 'u10 min/max: ', local_pet, &
      minval(field_data(lb(1):ub(1), lb(2):ub(2))), &
      maxval(field_data(lb(1):ub(1), lb(2):ub(2)))

    write(counter, '(i4.4)') n
    call write_fields_to_netcdf([atmosphere_model % get_field('u10'),  &
                                 atmosphere_model % get_field('v10'),  &
                                 atmosphere_model % get_field('sst')], &
                                'wrf_' // counter // '.nc')
  end do
  call atmosphere_model % finalize()

  call earthvm_finalize()

end program main

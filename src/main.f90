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

  type(ESMF_DistGrid) :: distgrid
  type(ESMF_Grid) :: grid
  type(ESMF_Field) :: destination_fields(3)
  integer :: im, jm, is, ie, js, je, i, j
  real, allocatable :: lon(:,:), lat(:,:)
  integer, allocatable :: mask(:,:)
  real :: lon1, lon2, lat1, lat2, dlon, dlat
  real, pointer :: field_data(:,:)
  integer :: ub(2), lb(2)
  character(4) :: counter = '0000'

  type(earthvm_regrid_type) :: regrid

  call earthvm_initialize()

  atmosphere_model = earthvm_model_type('wrf',                      &
                                         datetime(2019, 11, 11, 0), &
                                         datetime(2019, 11, 11, 1), &
                                         40,                        &
                                         set_wrf_services)

  ocean_model = earthvm_model_type('hycom',              &
                                   datetime(2020, 4, 1), &
                                   datetime(2020, 4, 5), &
                                   60,                   &
                                   set_hycom_services)

  call atmosphere_model % initialize()
  !call ocean_model % initialize()

  im = 200
  jm = 200
  local_pet = earthvm_get_local_pet()
  pet_count = earthvm_get_pet_count()
  if (local_pet == 0) then
    is = 1; ie = im / 2
    js = 1; je = jm / 2
  else if (local_pet == 1) then
    is = jm / 2 + 1; ie = im
    js = 1; je = jm / 2
  else if (local_pet == 2) then
    is = 1; ie = im / 2
    js = jm / 2 + 1; je = jm
  else if (local_pet == 3) then
    is = im / 2 + 1; ie = im
    js = jm / 2 + 1; je = jm
  end if

  allocate(lon(im,jm))
  allocate(lat(im,jm))
  allocate(mask(im,jm))

  lon1 = 144; lon2 = 150
  lat1 = -44; lat2 = -40
  dlon = (lon2 - lon1) / real(im)
  dlat = (lat2 - lat1) / real(jm)

  do j = 1, jm
    lon(:,j) = [(lon1 + (i - 1) * dlon, i = 1, im)]
  end do
  do i = 1, im
    lat(i,:) = [(lat1 + (j - 1) * dlat, j = 1, jm)]
  end do
  mask = 1

  distgrid = create_distgrid([is, js], [ie, je], [1, 1], [im, jm])
  grid = create_grid(distgrid, 'test grid', lon, lat, mask)
  call write_grid_to_netcdf(grid, 'test_grid.nc')
  destination_fields = [create_field(grid, 'u10'), &
                        create_field(grid, 'v10'), &
                        create_field(grid, 'sst')]

  call regrid % regrid_field_store(atmosphere_model % get_field('u10'), &
                                   destination_fields(1))
  !print *, 'regrid object is initialized', regrid % initialized

  !call ESMF_FieldGet(field, farrayPtr=field_data)
  !print *, 'before regrid', field_data(is:ie,js:je)

  call regrid % regrid_field(atmosphere_model % get_field('u10'), &
                             destination_fields(1))
  call regrid % regrid_field(atmosphere_model % get_field('v10'), &
                             destination_fields(2))
  call regrid % regrid_field(atmosphere_model % get_field('sst'), &
                             destination_fields(3))

  !print *, 'after regrid', field_data(is:ie,js:je)

  call write_fields_to_netcdf([atmosphere_model % get_field('u10'),  &
                               atmosphere_model % get_field('v10'),  &
                               atmosphere_model % get_field('sst')], &
                              'source_fields_0000.nc')

  call write_fields_to_netcdf(destination_fields, 'destination_fields_0000.nc')

  do n = 1, 90
    if (local_pet == 0) print *, 'calling atmosphere_model % run()'
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
                                'source_fields_' // counter // '.nc')
  end do
  call atmosphere_model % finalize()

  call earthvm_finalize()

end program main

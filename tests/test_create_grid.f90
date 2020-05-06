program test_create_grid

  use ESMF
  use earthvm_assert, only: assert
  use earthvm_state, only: earthvm_get_local_pet, earthvm_get_pet_count, &
                           earthvm_initialize, earthvm_finalize
  use earthvm_esmf, only: create_grid, create_distgrid, create_field
  use earthvm_io, only: write_fields_to_netcdf
  implicit none

  type(ESMF_DistGrid) :: distgrid
  type(ESMF_Grid) :: grid
  type(ESMF_Field) :: fields(3)
  integer :: im, jm, is, ie, js, je, i, j
  real, allocatable :: lon(:,:), lat(:,:)
  integer, allocatable :: mask(:,:)
  real :: lon1, lon2, lat1, lat2, dlon, dlat
  integer :: local_pet, pet_count

  call earthvm_initialize()

  local_pet = earthvm_get_local_pet()
  pet_count = earthvm_get_pet_count()

  call assert(pet_count == 4, 'Error: must be run on 4 processes')

  im = 200
  jm = 200
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
  fields = [create_field(grid, 'u10'), &
            create_field(grid, 'v10'), &
            create_field(grid, 'sst')]

  call write_fields_to_netcdf(fields, 'test_fields.nc')

  call earthvm_finalize()

end program test_create_grid

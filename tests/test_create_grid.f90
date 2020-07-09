program test_create_grid

  use ESMF
  use earthvm_state, only: earthvm_initialize, earthvm_finalize
  use earthvm_esmf, only: create_grid, create_distgrid, create_field
  use earthvm_io, only: write_fields_to_netcdf
  implicit none

  type(ESMF_DistGrid) :: distgrid
  type(ESMF_Grid) :: grid
  type(ESMF_Field) :: fields(3)
  integer :: i, j
  integer, parameter :: im = 100, jm = 100
  real, allocatable :: lon(:,:), lat(:,:)
  integer, allocatable :: mask(:,:)
  real :: lon1, lon2, lat1, lat2, dlon, dlat

  call earthvm_initialize()
  
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

  distgrid = create_distgrid([1, 1], [im, jm], [1, 1], [im, jm])
  grid = create_grid(distgrid, 'test grid', lon, lat, mask)
  fields = [create_field(grid, 'u10'), &
            create_field(grid, 'v10'), &
            create_field(grid, 'sst')]

  call earthvm_finalize()

end program test_create_grid

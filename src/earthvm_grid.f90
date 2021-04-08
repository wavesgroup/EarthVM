module earthvm_grid
  use esmf, only: ESMF_KIND_I4, ESMF_KIND_R4, ESMF_KIND_R8
  implicit none
  
  private
  public :: grid_rotation
  public :: DEGREES_TO_RADIANS

  real(ESMF_KIND_R8), parameter :: PI = 3.141592653589793_ESMF_KIND_R8
  real(ESMF_KIND_R8), parameter :: DEGREES_TO_RADIANS = PI / 180

contains

  pure function grid_rotation(lon, lat) result(res)
    ! Returns the 2-dimensional array whose values are the angle
    ! of the x-coordinate of the grid relative to the zonal direction.
    real(ESMF_KIND_R4), intent(in) :: lon(:,:), lat(:,:)
    real(ESMF_KIND_R4), allocatable :: res(:,:), lon_scale(:,:)
    integer(ESMF_KIND_I4) :: i, j, im, jm

    im = size(lon, dim=1)
    jm = size(lon, dim=1)
    allocate(res(im,jm), lon_scale(im,jm))

    lon_scale = abs(cos(DEGREES_TO_RADIANS * lat))

    ! Inner points
    do concurrent(i = 2:im-1, j = 2:jm-1)
      res(i,j) = atan2(lat(i+1,j) - lat(i-1,j), &
                      (lon(i+1,j) - lon(i-1,j)) * lon_scale(i,j))
    end do

    ! West and East boundaries
    do concurrent(j = 2:jm-1)
      res(1,j)  = atan2(lat(2,j) - lat(1,j),&
                       (lon(2,j) - lon(1,j)) * lon_scale(1,j))
      res(im,j) = atan2(lat(im,j) - lat(im-1,j), &
                       (lon(im,j) - lon(im-1,j)) * lon_scale(im,j))
    end do

    ! South and North boundaries
    do concurrent(i = 2:im-1)
      res(i,1) = atan2(lat(i+1,1) - lat(i-1,1), &
                       (lon(i+1,1) - lon(i-1,1)) * lon_scale(i,1))
      res(i,jm) = atan2(lat(i+1,jm) - lat(i-1,jm), &
                       (lon(i+1,jm) - lon(i-1,jm)) * lon_scale(i,jm))
    end do

    ! Corner cells
    res(1,1) = atan2(lat(2,1) - lat(1,1), &
                    (lon(2,1) - lon(1,1)) * lon_scale(1,1))
    res(im,1) = atan2(lat(im,1) - lat(im-1,1), &
                     (lon(im,1) - lon(im-1,1)) * lon_scale(im,1))
    res(1,jm) = atan2(lat(2,jm) - lat(1,jm), &
                     (lon(2,jm) - lon(1,jm)) * lon_scale(1,jm))
    res(im,jm) = atan2(lat(im,jm) - lat(im-1,jm), &
                      (lon(im,jm) - lon(im-1,jm)) * lon_scale(im,jm))

  end function grid_rotation

end module earthvm_grid

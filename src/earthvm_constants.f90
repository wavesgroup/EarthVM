module earthvm_constants
  use esmf, only: dp => ESMF_KIND_R8
  implicit none

  private
  public :: DEGREES_TO_RADIANS
  public :: PI
  public :: RADIANS_TO_DEGREES

  real(dp), parameter :: PI = 4 * atan(1._dp) 
  real(dp), parameter :: DEGREES_TO_RADIANS = PI / 180
  real(dp), parameter :: RADIANS_TO_DEGREES = 180 / PI

end module earthvm_constants

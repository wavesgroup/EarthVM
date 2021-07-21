module earthvm_constants
  use esmf, only: dp => ESMF_KIND_R8
  implicit none

  private

  real(dp), public, parameter ::   &
    PI = 4 * atan(1._dp),          &
    DEGREES_TO_RADIANS = PI / 180, &
    RADIANS_TO_DEGREES = 180 / PI, &
    VON_KARMAN = 0.4_dp

end module earthvm_constants

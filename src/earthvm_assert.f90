module earthvm_assert
  use iso_fortran_env, only: stderr => error_unit
  use ESMF, only: ESMF_Finalize, ESMF_END_ABORT, ESMF_SUCCESS
  implicit none
  private
  public :: assert, assert_success
contains

  subroutine assert(condition, msg)
    logical, intent(in) :: condition
    character(*), intent(in), optional :: msg
    if (.not. condition) then
      if (present(msg)) write(stderr, '(a)') msg
      call ESMF_Finalize()
      error stop 'EarthVM: Assertion failed.'
    end if
  end subroutine assert

  subroutine assert_success(rc)
    integer, intent(in out) :: rc
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  end subroutine assert_success

end module earthvm_assert

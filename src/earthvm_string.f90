module earthvm_string
  implicit none

  private
  public :: string

  type :: string
    character(:), allocatable :: value
  contains
    procedure :: eq
    procedure :: w
    generic :: write(formatted) => w
    generic :: operator(==) => eq
  end type string

contains

  subroutine w(self, unit, iotype, vlist, iostat, iomsg)
    class(string), intent(in) :: self
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    if (allocated(self % value)) then
      write(unit, '(a)') self % value
    else
      write(unit, '(a)') ''
    end if
  end subroutine w


  pure elemental logical function eq(self, other_string)
    class(string), intent(in) :: self, other_string
    eq = self % value == other_string % value
  end function eq

end module earthvm_string

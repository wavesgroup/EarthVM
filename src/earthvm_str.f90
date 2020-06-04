module earthvm_str
  implicit none

  private
  public :: str

  type :: str
    character(:), allocatable :: value
  contains
    procedure :: eq
    procedure :: w
    generic :: write(formatted) => w
    generic :: operator(==) => eq
  end type str

contains

  subroutine w(self, unit, iotype, vlist, iostat, iomsg)
    class(str), intent(in) :: self
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


  pure elemental logical function eq(self, other_str)
    class(str), intent(in) :: self, other_str
    eq = self % value == other_str % value
  end function eq

end module earthvm_str

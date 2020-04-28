module earthvm_regrid
  use iso_fortran_env, only: stderr => error_unit
  use ESMF
  use earthvm_assert, only: assert_success
  use earthvm_esmf, only: get_grid
  use earthvm_state, only: earthvm_finalize
  implicit none
  private
  public :: earthvm_regrid_type

  type :: earthvm_regrid_type
    logical :: initialized = .false.
    type(ESMF_Grid) :: source_grid, destination_grid
    type(ESMF_RouteHandle) :: weights
  contains
    procedure, pass(self), public :: regrid_field
    procedure, pass(self), public :: regrid_field_store
  end type earthvm_regrid_type

  !interface earthvm_regrid_type
  !  module procedure :: earthvm_regrid_constructor
  !end interface earthvm_regrid_type

contains

  subroutine regrid_field_store(self, source_field, destination_field)
    class(earthvm_regrid_type), intent(in out) :: self
    type(ESMF_Field), intent(in) :: source_field
    type(ESMF_Field), intent(in out) :: destination_field
    type(ESMF_RouteHandle) :: weights
    integer :: rc

    call ESMF_FieldRegridStore(source_field, destination_field, &
                               routehandle=weights, rc=rc)
    call assert_success(rc)

    self % weights = weights
    self % source_grid = get_grid(source_field)
    self % destination_grid = get_grid(destination_field)
    self % initialized = .true.

  end subroutine regrid_field_store


  subroutine regrid_field(self, source_field, destination_field)
    class(earthvm_regrid_type), intent(in out) :: self
    type(ESMF_Field), intent(in) :: source_field
    type(ESMF_Field), intent(in out) :: destination_field
    integer :: rc

    if (.not. self % initialized) then
      write(stderr, *) 'EarthVM Error: earthvm_regrid_type instance is not initialized.'
      call earthvm_finalize()
    end if

    if (self % source_grid /= get_grid(source_field)) then
      write(stderr, *) 'EarthVM Error: Incompatible source_field in regrid_field.'
      call earthvm_finalize()
    end if

    if (self % destination_grid /= get_grid(destination_field)) then
      write(stderr, *) 'EarthVM Error: Incompatible destination_field in regrid_field.'
      call earthvm_finalize()
    end if

    call ESMF_FieldRegrid(source_field, destination_field, self % weights, rc=rc)
    call assert_success(rc)

  end subroutine regrid_field

end module earthvm_regrid

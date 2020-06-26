module earthvm_coupling
  implicit none

  private
  public :: get_coupling, set_coupling

  type :: coupling_type
    character(:), allocatable :: src_model, src_field, dst_model, dst_field
  contains
    procedure :: w
    generic :: write(formatted) => w 
  end type coupling_type

  type(coupling_type), allocatable :: coupling(:)

contains


  function get_coupling(src_model, src_field, dst_model, dst_field) result(res)
    character(*), intent(in), optional :: src_model, src_field, dst_model, dst_field
    type(coupling_type), allocatable :: res(:)
    integer :: n
    
    allocate(res(0))
    
    if (.not. allocated(coupling)) return
   
    if (.not. any([present(src_model), present(src_field), &
                   present(dst_model), present(dst_field)])) then
      res = coupling
      return
    end if

    do n = 1, size(coupling)
      
      if (present(src_model)) then
        if (src_model == coupling(n) % src_model) res = [res, coupling(n)]
      end if
    
      if (present(src_field)) then
        if (src_field == coupling(n) % src_field) res = [res, coupling(n)]
      end if
    
      if (present(dst_model)) then
        if (dst_model == coupling(n) % dst_model) res = [res, coupling(n)]
      end if
    
      if (present(dst_field)) then
        if (dst_field == coupling(n) % dst_field) res = [res, coupling(n)]
      end if
    
    end do

  end function get_coupling


  subroutine set_coupling(src_model, src_field, dst_model, dst_field)
    character(*), intent(in) :: src_model, src_field, dst_model, dst_field
    if (.not. allocated(coupling)) allocate(coupling(0))
    coupling = [coupling, coupling_type(src_model, src_field, dst_model, dst_field)]
  end subroutine set_coupling

  
  subroutine w(self, unit, iotype, vlist, iostat, iomsg)
    class(coupling_type), intent(in) :: self
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '(4(a, 1x))') self % src_model, self % src_field, &
                              self % dst_model, self % dst_field
  end subroutine w

end module earthvm_coupling
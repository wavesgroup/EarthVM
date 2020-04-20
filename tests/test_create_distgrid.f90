program test_create_distgrid
  use ESMF, only: ESMF_DistGrid
  use earthvm_state, only: earthvm_initialize, earthvm_get_local_pet
  use earthvm_esmf, only: create_distgrid
  type(ESMF_DistGrid) :: distgrid
  integer :: index_list(4)
  call earthvm_initialize()
  if (earthvm_get_local_pet() == 0) then
    !distgrid = create_distgrid([1, 1], [40, 30])
  else if (earthvm_get_local_pet() == 1) then
    !distgrid = create_distgrid([1, 31], [41, 60])
  end if
end program test_create_distgrid

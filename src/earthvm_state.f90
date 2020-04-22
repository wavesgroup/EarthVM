module earthvm_state
  use ESMF!TODO, only: ...
  use earthvm_assert, only: assert, assert_success
  implicit none
  private
  public :: earthvm_initialize, earthvm_finalize, earthvm_get_mpicomm, &
            earthvm_get_pet_count, earthvm_get_local_pet, earthvm_get_vm

  type(ESMF_VM) :: vm
  logical :: earthvm_is_initialized = .false.

contains

  subroutine earthvm_initialize()
    integer :: rc
    call ESMF_Initialize(defaultCalKind = ESMF_CALKIND_GREGORIAN, &
                         vm             = vm,                     &
                         logkindflag    = ESMF_LOGKIND_SINGLE,    &
                         rc             = rc)
    call assert_success(rc)
    earthvm_is_initialized = .true.
  end subroutine earthvm_initialize

  subroutine earthvm_finalize()
    call ESMF_Finalize()
  end subroutine earthvm_finalize

  integer function earthvm_get_mpicomm() result(res)
    integer :: rc
    call assert(earthvm_is_initialized,          &
                'Error in earthvm_get_mpicomm(): &
                 EarthVM has not been initialized.')
    call ESMF_VMGet(vm=vm, mpiCommunicator=res, rc=rc)
    call assert_success(rc)
  end function earthvm_get_mpicomm

  integer function earthvm_get_local_pet() result(res)
    integer :: rc
    call assert(earthvm_is_initialized,            &
                'Error in earthvm_get_local_pet(): &
                 EarthVM has not been initialized.')
    call ESMF_VMGet(vm=vm, localPET=res, rc=rc)
    call assert_success(rc)
  end function earthvm_get_local_pet

  integer function earthvm_get_pet_count() result(res)
    integer :: rc
    call assert(earthvm_is_initialized,            &
                'Error in earthvm_get_pet_count(): &
                 EarthVM has not been initialized.')
    call ESMF_VMGet(vm=vm, petCount=res, rc=rc)
    call assert_success(rc)
  end function earthvm_get_pet_count

  type(ESMF_VM) function earthvm_get_vm() result(res)
    call assert(earthvm_is_initialized,     &
                'Error in earthvm_get_vm(): &
                 EarthVM has not been initialized.')
    res = vm
  end function earthvm_get_vm

end module earthvm_state

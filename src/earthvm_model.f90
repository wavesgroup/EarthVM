module earthvm_model
  use ESMF, only: ESMF_Clock, ESMF_ClockCreate, &
                  ESMF_GridCompInitialize, ESMF_GridCompRun, &
                  ESMF_GridCompFinalize, ESMF_GridCompSetServices, &
                  ESMF_SYNC_BLOCKING, ESMF_SYNC_NONBLOCKING, &
                  ESMF_DistGrid, ESMF_DistGridCreate, ESMF_Grid, ESMF_GridComp, &
                  ESMF_Field, &
                  ESMF_LogWrite, ESMF_LogFlush, ESMF_LOGMSG_INFO, &
                  ESMF_KIND_I4, ESMF_KIND_R4, &
                  ESMF_State, ESMF_Time, ESMF_TimeInterval, &
                  ESMF_VM, ESMF_VMGather, ESMF_VMBroadcast, &
                  ESMF_INDEX_GLOBAL
  use earthvm_assert, only: assert_success
  use earthvm_state, only: earthvm_get_local_pet, earthvm_get_pet_count, &
                           earthvm_get_vm
  implicit none

  private
  public :: earthvm_model_type

  type :: earthvm_model_type
    character(:), allocatable :: name
    type(ESMF_Clock) :: clock
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Grid) :: grid
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_Field), allocatable :: import_field(:), export_field(:)
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Time) :: current_time, start_time, stop_time
    type(ESMF_TimeInterval) :: time_step
    type(ESMF_VM) :: vm
  contains
    procedure, pass(self) :: initialize, run, finalize, set_services
  end type earthvm_model_type

contains

  subroutine initialize(self)
    class(earthvm_model_type), intent(in out) :: self
    integer :: rc
    call ESMF_GridCompInitialize(gridComp    = self % gridded_component, &
                                 importState = self % import_state,      &
                                 exportState = self % export_state,      &
                                 clock       = self % clock,             &
                                 phase       = 1,                        &
                                 syncflag    = ESMF_SYNC_BLOCKING,       &
                                 rc          = rc)
    call assert_success(rc)
    call ESMF_LogWrite('Model ' // self % name // ' initialized', ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()
  end subroutine initialize


  subroutine run(self)
    class(earthvm_model_type), intent(in out) :: self
    integer :: rc
    call ESMF_GridCompRun(gridComp    = self % gridded_component, &
                          importState = self % import_state,      &
                          exportState = self % export_state,      &
                          clock       = self % clock,             &
                          phase       = 1,                        &
                          syncflag    = ESMF_SYNC_BLOCKING,       &
                          rc          = rc)
    call assert_success(rc)
    call ESMF_LogWrite('Model ' // self % name // ' ran', ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()
  end subroutine run


  subroutine finalize(self)
    class(earthvm_model_type), intent(in out) :: self
    integer :: rc
    call ESMF_GridCompFinalize(gridComp    = self % gridded_component, &
                               importState = self % import_state,      &
                               exportState = self % export_state,      &
                               clock       = self % clock,             &
                               phase       = 1,                        &
                               syncflag    = ESMF_SYNC_NONBLOCKING,    &
                               rc          = rc)
    call assert_success(rc)
    call ESMF_LogWrite('Model ' // self % name // ' ran', ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()
  end subroutine finalize


  subroutine set_services(self, user_routine)
    class(earthvm_model_type), intent(in out) :: self
    interface
      subroutine user_routine(gridcomp, rc)
        import :: ESMF_GridComp
        type(ESMF_GridComp) :: gridcomp
        integer, intent(out) :: rc
      end subroutine user_routine
    end interface
    integer :: rc
    call ESMF_GridCompSetServices(gridcomp    = self % gridded_component, &
                                  userRoutine = user_routine,             &
                                  rc          = rc)
    call assert_success(rc)
  end subroutine set_services

end module earthvm_model

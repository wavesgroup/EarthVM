module earthvm_model
  use ESMF !TODO , only: ...
  use earthvm_assert, only: assert_success
  use earthvm_state, only: earthvm_get_local_pet, earthvm_get_pet_count, &
                           earthvm_get_vm
  use earthvm_esmf, only: datetime
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
    procedure, pass(self) :: get_field
    procedure, pass(self) :: initialize
    procedure, pass(self) :: run
    procedure, pass(self) :: finalize
    procedure, pass(self) :: set_services
  end type earthvm_model_type

  interface earthvm_model_type
    module procedure :: earthvm_model_constructor
  end interface earthvm_model_type

contains

  type(earthvm_model_type) function earthvm_model_constructor( &
    name, start_time, stop_time, time_step, user_services) result(self)
    character(*), intent(in) :: name
    type(datetime), intent(in) :: start_time, stop_time
    integer, intent(in) :: time_step ! seconds
    interface
      subroutine user_services(gridcomp, rc)
        import :: ESMF_GridComp
        type(ESMF_GridComp) :: gridcomp
        integer, intent(out) :: rc
      end subroutine user_services
    end interface
    integer :: rc
    self % name = name

    call ESMF_TimeIntervalSet(timeinterval=self % time_step, s=time_step, rc=rc)
    call assert_success(rc)

    CALL ESMF_TimeSet(time = self % start_time,   &
                      yy   = start_time % year,   &
                      mm   = start_time % month,  &
                      dd   = start_time % day,    &
                      h    = start_time % hour,   &
                      m    = start_time % minute, &
                      s    = start_time % second, &
                      rc   = rc)
    call assert_success(rc)

    CALL ESMF_TimeSet(time = self % stop_time,   &
                      yy   = stop_time % year,   &
                      mm   = stop_time % month,  &
                      dd   = stop_time % day,    &
                      h    = stop_time % hour,   &
                      m    = stop_time % minute, &
                      s    = stop_time % second, &
                      rc   = rc)
    call assert_success(rc)

    self % clock = ESMF_ClockCreate(timeStep = self % time_step,   &
                                    startTime = self % start_time, &
                                    stopTime  = self % stop_time,  &
                                    rc        = rc)
    call assert_success(rc)

    self % gridded_component = ESMF_GridCompCreate(name        = self % name,            &
                                                   clock       = self % clock,           &
                                                   contextflag = ESMF_CONTEXT_PARENT_VM, &
                                                   rc          = rc)
    call assert_success(rc)

    self % import_state = ESMF_StateCreate(name        = self % name // 'import_state', &
                                           stateintent = ESMF_STATEINTENT_IMPORT,       &
                                           rc          = rc)
    call assert_success(rc)

    self % export_state = ESMF_StateCreate(name        = self % name // 'export_state', &
                                           stateintent = ESMF_STATEINTENT_EXPORT,       &
                                           rc          = rc)
    call assert_success(rc)

    !call self % set_services(set_wrf_services)
    call ESMF_GridCompSetServices(gridcomp    = self % gridded_component, &
                                  userRoutine = user_services,            &
                                  rc          = rc)
    call assert_success(rc)

  end function earthvm_model_constructor


  type(ESMF_Field) function get_field(self, field_name) result(field)
    class(earthvm_model_type), intent(in) :: self
    character(*), intent(in) :: field_name
    integer :: rc
    call ESMF_StateGet(self % import_state, field_name, field, rc=rc)
    if (rc == ESMF_RC_NOT_FOUND) then
      call ESMF_StateGet(self % export_state, field_name, field, rc=rc)
      call assert_success(rc)
    else
      call assert_success(rc)
    end if
  end function get_field


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

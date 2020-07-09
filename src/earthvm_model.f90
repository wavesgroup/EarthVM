module earthvm_model
  use ESMF !TODO , only: ...
  use earthvm_assert, only: assert_success
  use earthvm_datetime, only: datetime
  use earthvm_esmf, only: get_itemlist_from_state
  use earthvm_events
  use earthvm_io, only: write_fields_to_netcdf
  use earthvm_state, only: earthvm_get_local_pet, earthvm_get_pet_count, &
                           earthvm_get_vm
  use earthvm_regrid, only: earthvm_regrid_type
  use earthvm_str, only: str
  implicit none

  private
  public :: earthvm_model_type

  type :: earthvm_model_type
    character(:), allocatable :: name
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    type(earthvm_regrid_type), allocatable :: regrid(:)
    type(str), allocatable :: import_fields(:), export_fields(:)
    logical :: verbose = .true.
  contains
    procedure, pass(self) :: finalize
    procedure, pass(self) :: force
    procedure, pass(self) :: force_single_field
    procedure, pass(self) :: get_current_time
    procedure, pass(self) :: get_field
    procedure, pass(self) :: get_field_data
    procedure, pass(self) :: initialize
    procedure, pass(self) :: run
    procedure, pass(self) :: set_import_fields
    procedure, pass(self) :: set_export_fields
    procedure, pass(self) :: set_services
    procedure, pass(self) :: write_to_netcdf
  end type earthvm_model_type

  interface earthvm_model_type
    module procedure :: earthvm_model_constructor
  end interface earthvm_model_type

contains

  type(earthvm_model_type) function earthvm_model_constructor( &
    name, start_time, stop_time, time_step, user_services, &
    import_fields, export_fields, verbose) result(self)
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
    type(str), intent(in), optional :: import_fields(:), export_fields(:)
    logical, intent(in), optional :: verbose
    type(ESMF_Time) :: esmf_start_time, esmf_stop_time
    type(ESMF_TimeInterval) :: esmf_time_step
    integer :: rc
    self % name = name

    allocate(self % regrid(0))

    if (present(import_fields)) then
      self % import_fields = import_fields
    else
      allocate(self % import_fields(0))
    end if

    if (present(export_fields)) then
      self % export_fields = export_fields
    else
      allocate(self % export_fields(0))
    end if

    if (present(verbose)) self % verbose = verbose

    call ESMF_TimeIntervalSet(timeinterval=esmf_time_step, s=time_step, rc=rc)
    call assert_success(rc)

    call ESMF_TimeSet(time = esmf_start_time,          &
                      yy   = start_time % getYear(),   &
                      mm   = start_time % getMonth(),  &
                      dd   = start_time % getDay(),    &
                      h    = start_time % getHour(),   &
                      m    = start_time % getMinute(), &
                      s    = start_time % getSecond(), &
                      rc   = rc)
    call assert_success(rc)

    call ESMF_TimeSet(time = esmf_stop_time,          &
                      yy   = stop_time % getYear(),   &
                      mm   = stop_time % getMonth(),  &
                      dd   = stop_time % getDay(),    &
                      h    = stop_time % getHour(),   &
                      m    = stop_time % getMinute(), &
                      s    = stop_time % getSecond(), &
                      rc   = rc)
    call assert_success(rc)

    self % clock = ESMF_ClockCreate(timeStep  = esmf_time_step,  &
                                    startTime = esmf_start_time, &
                                    stopTime  = esmf_stop_time,  &
                                    rc        = rc)
    call assert_success(rc)

    self % gridded_component = ESMF_GridCompCreate(name        = self % name,            &
                                                   clock       = self % clock,           &
                                                   contextflag = ESMF_CONTEXT_PARENT_VM, &
                                                   rc          = rc)
    call assert_success(rc)

    self % import_state = ESMF_StateCreate(name        = self % name // '_import_state', &
                                           stateintent = ESMF_STATEINTENT_IMPORT,       &
                                           rc          = rc)
    call assert_success(rc)

    self % export_state = ESMF_StateCreate(name        = self % name // '_export_state', &
                                           stateintent = ESMF_STATEINTENT_EXPORT,       &
                                           rc          = rc)
    call assert_success(rc)

    call ESMF_GridCompSetServices(gridcomp    = self % gridded_component, &
                                  userRoutine = user_services,            &
                                  rc          = rc)
    call assert_success(rc)

  end function earthvm_model_constructor


  subroutine force(self, target_model)
    ! Regrids the export fields from this model to the target model.
    ! This affects only ESMF fields involved.
    ! To force the target model in effect, the regridded values must be copied
    ! to the native model data structure.
    class(earthvm_model_type), intent(in out) :: self, target_model
    integer :: n
    do n = 1, size(self % export_fields)
      if (any(self % export_fields(n) == target_model % import_fields)) then
        call self % force_single_field(target_model, self % export_fields(n) % value)
      end if
    end do
  end subroutine force


  subroutine force_single_field(self, target_model, field_name)
    ! Regrids a single field from this to target model.
    class(earthvm_model_type), intent(in out) :: self, target_model
    character(*), intent(in) :: field_name
    type(ESMF_Field) :: source_field, destination_field
    logical :: found
    integer :: n

    ! search for the correct regrid instance
    found = .false.
    do n = 1, size(self % regrid)
      if (self % regrid(n) % name == target_model % name) then
        found = .true.
        exit
      end if         
    end do

    if (.not. found) then
      ! regrid instance not found; create a new one and add it to the stack
      self % regrid = [self % regrid, earthvm_regrid_type(target_model % name)]
      n = size(self % regrid)
    end if

    source_field = self % get_field(field_name)
    destination_field = target_model % get_field(field_name)

    if (.not. self % regrid(n) % initialized) then
      call self % regrid(n) % regrid_field_store(source_field, destination_field)
    end if

    call self % regrid(n) % regrid_field(source_field, destination_field)

  end subroutine force_single_field


  type(datetime) function get_current_time(self) result(time)
    ! Returns the current model time.
    class(earthvm_model_type), intent(in) :: self
    type(ESMF_Time) :: current_esmf_time
    integer :: year, month, day, hour, minute, second
    integer :: rc

    call ESMF_ClockGet(self % clock, currTime=current_esmf_time, rc=rc)
    call assert_success(rc)

    call ESMF_TimeGet(current_esmf_time, yy=year, mm=month, dd=day, &
                      h=hour, m=minute, s=second, rc=rc)
    call assert_success(rc)

    time = datetime(year, month, day, hour, minute, second)

  end function get_current_time


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


  subroutine get_field_data(self, field_name, field_data, lower_bounds, upper_bounds)
    class(earthvm_model_type), intent(in) :: self
    character(*), intent(in) :: field_name
    real, pointer, intent(out) :: field_data(:,:)
    integer, intent(out), optional :: lower_bounds(2), upper_bounds(2)
    integer :: rc
    call ESMF_FieldGet(self % get_field(field_name), farrayPtr=field_data, &
                       exclusiveLBound=lower_bounds, exclusiveUBound=upper_bounds)
    call assert_success(rc)
  end subroutine get_field_data


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
    type(datetime) :: current_time
    integer :: rc

    if (self % verbose) then
      if (earthvm_get_local_pet() == 0) then
        current_time = self % get_current_time()
        print *, current_time % strftime('%Y-%m-%d %H:%M:%S') // ': Running ' // self % name
      end if
    end if

    ! call the run user method
    call ESMF_GridCompRun(gridComp    = self % gridded_component, &
                          importState = self % import_state,      &
                          exportState = self % export_state,      &
                          clock       = self % clock,             &
                          phase       = 1,                        &
                          syncflag    = ESMF_SYNC_BLOCKING,       &
                          rc          = rc)
    call assert_success(rc)

    ! tick the clock forward in time
    call ESMF_ClockAdvance(self % clock, rc=rc)
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


  subroutine set_import_fields(self, fields)
    class(earthvm_model_type), intent(in out) :: self
    type(str), intent(in) :: fields(:)
    self % import_fields = fields
  end subroutine set_import_fields


  subroutine set_export_fields(self, fields)
    class(earthvm_model_type), intent(in out) :: self
    type(str), intent(in) :: fields(:)
    self % export_fields = fields
  end subroutine set_export_fields


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


  subroutine write_to_netcdf(self)
    ! Writes all model fields into a NetCDF file.
    class(earthvm_model_type), intent(in) :: self
    type(ESMF_Field), allocatable :: fields(:)
    type(datetime) :: current_time
    character(ESMF_MAXSTR), allocatable :: field_names(:)
    character(:), allocatable :: filename
    integer :: n

    fields = [ESMF_Field::]

    field_names = get_itemlist_from_state(self % import_state)
    do n = 1, size(field_names)
      fields = [fields, self % get_field(trim(field_names(n)))]
    end do

    field_names = get_itemlist_from_state(self % export_state)
    do n = 1, size(field_names)
      fields = [fields, self % get_field(trim(field_names(n)))]
    end do

    current_time = self % get_current_time()
    filename = self % name // '_' // current_time % strftime('%Y-%m-%d_%H:%M:%S') // '.nc'
    call write_fields_to_netcdf(fields, filename)

  end subroutine write_to_netcdf

end module earthvm_model

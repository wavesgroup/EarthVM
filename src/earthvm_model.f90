module earthvm_model
  use ESMF !TODO , only: ...
  use earthvm_assert, only: assert_success
  use earthvm_datetime, only: datetime
  use earthvm_esmf, only: get_itemlist_from_state
  use earthvm_io, only: write_fields_to_netcdf
  use earthvm_state, only: earthvm_get_local_pet, earthvm_get_pet_count, earthvm_get_vm
  use earthvm_regrid, only: earthvm_regrid_type
  implicit none

  private
  public :: earthvm_model_type

  type :: earthvm_forcing_type
    character(:), allocatable :: source_field_name
    character(:), allocatable :: target_model_name
    character(:), allocatable :: target_field_name
  end type earthvm_forcing_type

  type :: earthvm_model_type
    character(:), allocatable :: name
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    type(earthvm_regrid_type), allocatable :: regrid(:)
    type(earthvm_forcing_type), allocatable :: forcing(:)
    logical :: nest = .false.
    logical :: verbose = .false.
  contains
    procedure, pass(self) :: finalize
    procedure, pass(self) :: force
    procedure, pass(self) :: force_single_field
    procedure, pass(self) :: get_current_time
    procedure, pass(self) :: get_field
    procedure, pass(self) :: get_field_data
    procedure, pass(self) :: initialize
    procedure, pass(self) :: run
    procedure, pass(self) :: set_forcing
    procedure, pass(self) :: set_services
    procedure, pass(self) :: write_to_netcdf
  end type earthvm_model_type

  interface earthvm_model_type
    module procedure :: earthvm_model_constructor
  end interface earthvm_model_type

contains

  type(earthvm_model_type) function earthvm_model_constructor(name, &
    start_time, stop_time, time_step, user_services, nest, verbose) result(self)
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
    logical, intent(in), optional :: nest, verbose
    type(ESMF_Time) :: esmf_start_time, esmf_stop_time
    type(ESMF_TimeInterval) :: esmf_time_step
    integer :: rc
    
    self % name = name
    
    if (present(nest)) self % nest = nest
    if (present(verbose)) self % verbose = verbose

    allocate(self % regrid(0))
    allocate(self % forcing(0))

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
    !real :: t1, t2

    !call cpu_time(t1)

    ! loop over forcings on this model
    do n = 1, size(self % forcing)
      if (self % forcing(n) % target_model_name == target_model % name) then
        call self % force_single_field(self % forcing(n) % source_field_name, &
                                       target_model,                          &
                                       self % forcing(n) % target_field_name)
      end if
    end do

    !call cpu_time(t2)
    !if (earthvm_get_local_pet() == 0) &
    !  print *, 'EarthVM: ' // self % name // ' -> ' // target_model % name &
    !           // ' force elapsed', t2 - t1, 'seconds.'

  end subroutine force


  subroutine force_single_field(self, source_field_name, target_model, target_field_name)
    ! Regrids a single field from this to target model.
    ! Source and target fields can have different names, but they both must be
    ! created and present in their respective model export and import states.
    class(earthvm_model_type), intent(in out) :: self
    character(*), intent(in) :: source_field_name
    class(earthvm_model_type), intent(in out) :: target_model
    character(*), intent(in) :: target_field_name
    type(ESMF_Field) :: source_field, target_field
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

    source_field = self % get_field(source_field_name)
    target_field = target_model % get_field(target_field_name)

    if (self % verbose) then
      if (earthvm_get_local_pet() == 0) then
        print *, 'Regridding ' // source_field_name // ' from ' // self % name &
          // ' to ' // target_field_name // ' on ' // target_model % name
      end if
    end if

    if (.not. self % regrid(n) % initialized) then
      call self % regrid(n) % regrid_field_store(source_field, target_field)
    end if

    call self % regrid(n) % regrid_field(source_field, target_field)

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
    ! Returns a field instance given the field name.
    class(earthvm_model_type), intent(in) :: self
    character(*), intent(in) :: field_name
    integer :: rc
    call ESMF_StateGet(self % export_state, field_name, field, rc=rc)
    if (rc == ESMF_RC_NOT_FOUND) &
      call ESMF_StateGet(self % import_state, field_name, field, rc=rc)
    call assert_success(rc)
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
    real :: t1, t2

    call cpu_time(t1)

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

    call cpu_time(t2)
    if (earthvm_get_local_pet() == 0) &
      print *, 'EarthVM: ' // self % name // ' run elapsed', t2 - t1, 'seconds.'

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

  
  subroutine set_forcing(self, source_field_name, target_model, target_field_name)
    ! Adds a forcing configuration to the list of forcings.
    class(earthvm_model_type), intent(in out) :: self
    character(*), intent(in) :: source_field_name
    class(earthvm_model_type), intent(in) :: target_model
    character(*), intent(in) :: target_field_name
    character(:), allocatable :: target_model_name
    
    target_model_name = target_model % name ! needed to work around a bug in gfortran-9.2.0
    self % forcing = [self % forcing, &
      earthvm_forcing_type(source_field_name, target_model_name, target_field_name)]

  end subroutine set_forcing

  
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

    allocate(fields(0))

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

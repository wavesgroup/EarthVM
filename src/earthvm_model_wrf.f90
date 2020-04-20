module earthvm_model_wrf

  use ESMF, only: ESMF_METHOD_INITIALIZE, ESMF_METHOD_FINALIZE, &
                  ESMF_METHOD_RUN, ESMF_GridCompSetEntryPoint, &
                  ESMF_GridComp, ESMF_State, ESMF_Clock, ESMF_SUCCESS, &
                  ESMF_GridCompSetServices, &
                  ESMF_DistGrid, ESMF_Grid, &
                  ESMF_TimeSet, ESMF_TimeIntervalSet, ESMF_ClockCreate, &
                  ESMF_GridCompCreate, ESMF_CONTEXT_PARENT_VM, &
                  ESMF_StateCreate, ESMF_STATEINTENT_EXPORT, ESMF_STATEINTENT_IMPORT
  use earthvm_assert, only: assert_success
  use earthvm_esmf, only: create_distgrid, create_grid
  use earthvm_io, only: write_grid_to_netcdf
  use earthvm_model, only: earthvm_model_type
  use module_wrf_top, only: get_ijk_from_grid, head_grid
  use module_wrf_top, only: wrf_init, wrf_run, wrf_finalize

  implicit none

  private
  public :: earthvm_model_wrf_type

  type, extends(earthvm_model_type) :: earthvm_model_wrf_type
  end type earthvm_model_wrf_type

  interface earthvm_model_wrf_type
    module procedure :: earthvm_model_wrf_constructor
  end interface earthvm_model_wrf_type

contains

  type(earthvm_model_wrf_type) function earthvm_model_wrf_constructor(name) result(self)
    character(*), intent(in) :: name
    integer :: rc
    self % name = name

    call ESMF_TimeIntervalSet(timeinterval=self % time_step, s=60, rc=rc)
    call assert_success(rc)

    CALL ESMF_TimeSet(time=self % start_time, yy=2020, mm=4, dd=1, h=0, m=0, s=0, rc=rc)
    call assert_success(rc)

    CALL ESMF_TimeSet(time=self % stop_time, yy=2020, mm=4, dd=5, h=0, m=0, s=0, rc=rc)
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

    call self % set_services(set_wrf_services)

  end function earthvm_model_wrf_constructor

  subroutine set_wrf_services(gridded_component, rc)
    type(ESMF_GridComp) :: gridded_component
    integer, intent(out) :: rc
    call ESMF_GridCompSetEntryPoint(gridcomp    = gridded_component, &
                                    methodflag  = ESMF_METHOD_INITIALIZE,   &
                                    userRoutine = wrf_component_init,       &
                                    rc          = rc)
    call assert_success(rc)
    call ESMF_GridCompSetEntryPoint(gridcomp    = gridded_component, &
                                    methodflag  = ESMF_METHOD_RUN,          &
                                    userRoutine = wrf_component_run,        &
                                    rc          = rc)
    call assert_success(rc)
    call ESMF_GridCompSetEntryPoint(gridcomp    = gridded_component, &
                                    methodflag  = ESMF_METHOD_FINALIZE,     &
                                    userRoutine = wrf_component_finalize,   &
                                    rc          = rc)
    call assert_success(rc)
  end subroutine set_wrf_services


  subroutine wrf_component_init(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Grid) :: grid
    integer, intent(out) :: rc
    integer :: ids, ide, jds, jde, kds, kde
    integer :: ims, ime, jms, jme, kms, kme
    integer :: ips, ipe, jps, jpe, kps, kpe
    print *, 'In wrf_component_init'
    call wrf_init()
    call get_ijk_from_grid(head_grid, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe)
    print *, ids, ide, jds, jde, kds, kde
    print *, ims, ime, jms, jme, kms, kme
    print *, ips, ipe, jps, jpe, kps, kpe

    ide = ide - 1
    jde = jde - 1
    ipe = min(ide, ipe)
    jpe = min(jde, jpe)

    distgrid = create_distgrid([ips, jps], [ipe, jpe], [ids, jds], [ide, jde])
    grid = create_grid(distgrid, 'WRF grid', &
                       lon=head_grid % xlong(ids:ide, jds:jde), &
                       lat=head_grid % xlat(ids:ide, jds:jde), &
                       mask=nint(head_grid % xland(ids:ide, jds:jde) - 1))

    call write_grid_to_netcdf(grid, 'wrf_grid.nc')

    rc = ESMF_SUCCESS
  end subroutine wrf_component_init

  subroutine wrf_component_run(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    print *, 'In wrf_component_run'
    rc = ESMF_SUCCESS
  end subroutine wrf_component_run

  subroutine wrf_component_finalize(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    print *, 'In wrf_component_finalize'
    rc = ESMF_SUCCESS
  end subroutine wrf_component_finalize

end module earthvm_model_wrf

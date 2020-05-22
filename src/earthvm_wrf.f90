module earthvm_wrf

  use ESMF !TODO , only: ...
  use earthvm_assert, only: assert_success
  use earthvm_esmf, only: create_distgrid, create_grid, create_field, &
                          get_field_values, set_field_values
  use earthvm_io, only: write_grid_to_netcdf
  use earthvm_state, only: earthvm_get_mpicomm
  use module_wrf_top, only: get_ijk_from_grid, head_grid, &
                            wrf_init, wrf_run, wrf_finalize

  implicit none

  private
  public :: set_services

contains

  subroutine set_services(gridded_component, rc)
    type(ESMF_GridComp) :: gridded_component
    integer, intent(out) :: rc
    call ESMF_GridCompSetEntryPoint(gridcomp    = gridded_component,      &
                                    methodflag  = ESMF_METHOD_INITIALIZE, &
                                    userRoutine = model_init,             &
                                    rc          = rc)
    call assert_success(rc)
    call ESMF_GridCompSetEntryPoint(gridcomp    = gridded_component, &
                                    methodflag  = ESMF_METHOD_RUN,   &
                                    userRoutine = model_run,         &
                                    rc          = rc)
    call assert_success(rc)
    call ESMF_GridCompSetEntryPoint(gridcomp    = gridded_component,    &
                                    methodflag  = ESMF_METHOD_FINALIZE, &
                                    userRoutine = model_finalize,       &
                                    rc          = rc)
    call assert_success(rc)
  end subroutine set_services


  subroutine model_init(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Grid) :: grid
    integer, intent(out) :: rc

    type(ESMF_Field), allocatable :: fields(:)

    integer :: ids, ide, jds, jde, kds, kde
    integer :: ims, ime, jms, jme, kms, kme
    integer :: ips, ipe, jps, jpe, kps, kpe
    print *, 'In wrf_component_init'

    call wrf_set_dm_communicator(earthvm_get_mpicomm())

    call wrf_init()
    call get_ijk_from_grid(head_grid, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe)

    ! exclude the last staggered grid cell
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

    fields = [create_field(grid, 'sst')]
    call set_field_values(fields(1), head_grid % tsk(ips:ipe,jps:jpe))
    call ESMF_StateAdd(import_state, fields, rc=rc)
    call assert_success(rc)

    fields = [create_field(grid, 'u10'), create_field(grid, 'v10')]
    call set_field_values(fields(1), head_grid % u10(ips:ipe,jps:jpe))
    call set_field_values(fields(2), head_grid % v10(ips:ipe,jps:jpe))
    call ESMF_StateAdd(export_state, fields, rc=rc)
    call assert_success(rc)

    rc = ESMF_SUCCESS
  end subroutine model_init


  subroutine model_run(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field
    integer :: ids, ide, jds, jde, kds, kde
    integer :: ims, ime, jms, jme, kms, kme
    integer :: ips, ipe, jps, jpe, kps, kpe
    integer :: i, j

    real, pointer :: field_values(:,:)
    integer :: lb(2), ub(2)

    call get_ijk_from_grid(head_grid, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe)

    ! exclude the last staggered grid cell
    ide = ide - 1
    jde = jde - 1
    ipe = min(ide, ipe)
    jpe = min(jde, jpe)

    ! copy values from ESMF field to the WRF data structure
    call ESMF_StateGet(import_state, 'sst', field)
    call get_field_values(field, field_values, lb, ub)
    do concurrent (i = ips:ipe, j = jps:jpe, head_grid % xland(i,j) > 1.5)
      head_grid % tsk(i,j) = field_values(i,j) + 273.15
    end do

    call set_wrf_clock(clock)
    call wrf_run()

    call ESMF_StateGet(export_state, 'u10', field)
    call set_field_values(field, head_grid % u10(ips:ipe,jps:jpe))

    call ESMF_StateGet(export_state, 'v10', field)
    call set_field_values(field, head_grid % v10(ips:ipe,jps:jpe))

    rc = ESMF_SUCCESS
  end subroutine model_run


  subroutine model_finalize(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    call wrf_finalize(no_shutdown=.true.)
    rc = ESMF_SUCCESS
  end subroutine model_finalize


  subroutine set_wrf_clock(clock)
    ! Sets WRF's internal clock to the input ESMF clock.
    use module_utility, only: WRFU_Time
    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time) :: current_time
    type(ESMF_TimeInterval) :: time_step
    character(256) :: start_time_string, stop_time_string
    type(WRFU_Time) :: wrf_start_time, wrf_stop_time

    call ESMF_ClockGet(clock, timeStep=time_step, currTime=current_time)
    call ESMF_TimeGet(current_time, timeStringISOFrac=start_time_string)
    call ESMF_TimeGet(current_time + time_step, timeStringISOFrac=stop_time_string)

    start_time_string(11:11) = '_'
    stop_time_string(11:11) = '_'

    call wrf_atotime(start_time_string, wrf_start_time)
    call wrf_atotime(stop_time_string, wrf_stop_time)

    head_grid % start_subtime = wrf_start_time
    head_grid % stop_subtime  = wrf_stop_time

  end subroutine set_wrf_clock

end module earthvm_wrf

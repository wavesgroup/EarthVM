module earthvm_wrf

  use ESMF !TODO , only: ...
  use earthvm_assert, only: assert_success
  use earthvm_esmf, only: create_distgrid, create_grid, create_field
  use earthvm_io, only: write_grid_to_netcdf
  use earthvm_model, only: earthvm_model_type
  use module_wrf_top, only: get_ijk_from_grid, head_grid, &
                            wrf_init, wrf_run, wrf_finalize

  implicit none

  private
  public :: set_services

contains

  subroutine set_services(gridded_component, rc)
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
  end subroutine set_services


  subroutine wrf_component_init(gridded_component, import_state, export_state, clock, rc)
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
    call ESMF_StateAdd(import_state, fields, rc=rc)
    call assert_success(rc)

    fields = [create_field(grid, 'u10'), create_field(grid, 'v10')]
    call ESMF_StateAdd(export_state, fields, rc=rc)
    call assert_success(rc)

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

end module earthvm_wrf

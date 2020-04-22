module earthvm_hycom

  use ESMF !TODO , only: ...
  use earthvm_assert, only: assert_success
  use earthvm_esmf, only: create_distgrid, create_grid, create_field
  use earthvm_io, only: write_grid_to_netcdf
  use earthvm_model, only: earthvm_model_type
  use earthvm_state, only: earthvm_get_mpicomm

  use mod_hycom, only: hycom_init, hycom_run, hycom_final
  use mod_xc, only: xcspmd

  implicit none

  private
  public :: set_services

contains

  subroutine set_services(gridded_component, rc)
    type(ESMF_GridComp) :: gridded_component
    integer, intent(out) :: rc
    call ESMF_GridCompSetEntryPoint(gridcomp    = gridded_component, &
                                    methodflag  = ESMF_METHOD_INITIALIZE,   &
                                    userRoutine = model_init,       &
                                    rc          = rc)
    call assert_success(rc)
    call ESMF_GridCompSetEntryPoint(gridcomp    = gridded_component, &
                                    methodflag  = ESMF_METHOD_RUN,          &
                                    userRoutine = model_run,        &
                                    rc          = rc)
    call assert_success(rc)
    call ESMF_GridCompSetEntryPoint(gridcomp    = gridded_component, &
                                    methodflag  = ESMF_METHOD_FINALIZE,     &
                                    userRoutine = model_finalize,   &
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

    call xcspmd(earthvm_get_mpicomm())
    call hycom_init()
    !distgrid = create_distgrid([ips, jps], [ipe, jpe], [ids, jds], [ide, jde])
    !grid = create_grid(distgrid, 'WRF grid', &
    !                   lon=head_grid % xlong(ids:ide, jds:jde), &
    !                   lat=head_grid % xlat(ids:ide, jds:jde), &
    !                   mask=nint(head_grid % xland(ids:ide, jds:jde) - 1))

    !call write_grid_to_netcdf(grid, 'wrf_grid.nc')

    !fields = [create_field(grid, 'sst')]
    !call ESMF_StateAdd(import_state, fields, rc=rc)
    !call assert_success(rc)

    !fields = [create_field(grid, 'u10'), create_field(grid, 'v10')]
    !call ESMF_StateAdd(export_state, fields, rc=rc)
    !call assert_success(rc)

    rc = ESMF_SUCCESS
  end subroutine model_init


  subroutine model_run(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    print *, 'In hycom_model_run'
    rc = ESMF_SUCCESS
  end subroutine model_run


  subroutine model_finalize(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    print *, 'In hycom_model_finalize'
    rc = ESMF_SUCCESS
  end subroutine model_finalize

end module earthvm_hycom

module earthvm_umwm

  use ESMF !TODO , only: ...
  use earthvm_assert, only: assert_success
  use earthvm_esmf, only: create_distgrid, create_grid, create_field, &
                          get_field_values, set_field_values
  use earthvm_events
  use earthvm_io, only: write_grid_to_netcdf
  use earthvm_state, only: earthvm_get_mpicomm, earthvm_get_local_pet, &
                           earthvm_get_pet_count
  use umwm_top, only: umwm_initialize, umwm_run, umwm_finalize

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

    use umwm_module, only: mm, nm, istart, iend, mi, ni, lon, lat, mask

    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Grid) :: grid
    integer, intent(out) :: rc

    type(ESMF_Field), allocatable :: fields(:)
    integer :: ips, ipe, jps, jpe
    integer :: local_pet, pet_count

    local_pet = earthvm_get_local_pet()
    pet_count = earthvm_get_pet_count()

    call umwm_initialize()

    if (mm >= nm) then
      ! row-major remapping
      ips = mi(istart)
      ipe = mi(iend)
      jps = 1
      jpe = nm
      if (local_pet == 0) ips = ips - 1
      if (local_pet == pet_count - 1) ipe = ipe + 1
    else if (mm < nm) then
      ! column-major remapping
      ips = 1
      ipe = mm
      jps = ni(istart)
      jpe = ni(iend)
      if (local_pet == 0) jps = jps - 1
      if (local_pet == pet_count - 1) jpe = jpe + 1
    end if

    distgrid = create_distgrid([ips, jps], [ipe, jpe], [1, 1], [mm, nm])
    grid = create_grid(distgrid, 'UMWM grid', lon, lat, mask)
    call write_grid_to_netcdf(grid, 'umwm_grid.nc')

    rc = ESMF_SUCCESS
  end subroutine model_init


  subroutine model_run(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field

    real, pointer :: field_values(:,:)
    integer :: lb(2), ub(2)

    !call umwm_run()

    rc = ESMF_SUCCESS
  end subroutine model_run


  subroutine model_finalize(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    call umwm_finalize()
    rc = ESMF_SUCCESS
  end subroutine model_finalize


end module earthvm_umwm

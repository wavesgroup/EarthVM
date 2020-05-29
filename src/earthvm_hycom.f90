module earthvm_hycom

  use ESMF !TODO , only: ...
  use earthvm_assert, only: assert_success
  use earthvm_esmf, only: create_distgrid, create_grid, create_field, &
                          get_field_values, set_field_values
  use earthvm_events
  use earthvm_io, only: write_grid_to_netcdf
  use earthvm_state, only: earthvm_get_local_pet, earthvm_get_mpicomm

  use mod_cb_arrays, only: plon, plat, depths, temp, taux, tauy, &
                           u, v, ubavg, vbavg, srfhgt, sshgmn, &
                           w0, w1, w2, w3
  use mod_dimensions, only: itdm, jtdm, i0, j0, ii, jj
  use mod_hycom, only: hycom_init, hycom_run, hycom_final, end_of_run
  use mod_xc, only: xcspmd

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
    real, allocatable :: lon(:,:), lat(:,:)
    integer, allocatable :: mask(:,:)
    integer :: local_pet

    integer :: i, j
    integer :: ids, ide, jds, jde
    integer :: ips, ipe, jps, jpe

    local_pet = earthvm_get_local_pet()

    if (local_pet == 0) call set_hycom_time_limits(clock)

    call xcspmd(earthvm_get_mpicomm())
    call hycom_init()

    ! set forcing weights--always use the values from the first index
    w0 = 1
    w1 = 0

    ! Map local HYCOM indices to global indices
    ids = 1
    jds = 1
    ide = itdm
    jde = jtdm

    ips = i0 + 1
    ipe = i0 + ii
    jps = j0 + 1
    jpe = j0 + jj

    allocate(lon(ide, jde))
    allocate(lat(ide, jde))
    allocate(mask(ide, jde))
    lon = 0
    lat = 0
    mask = 0

    lon(ips:ipe,jps:jpe) = plon(1:ii,1:jj)
    lat(ips:ipe,jps:jpe) = plat(1:ii,1:jj)

    do concurrent (i=ips:ipe, j=jps:jpe, depths(i-ips+1,j-jps+1) > 0)
      mask(i,j) = 1
    end do

    distgrid = create_distgrid([ips, jps], [ipe, jpe], [ids, jds], [ide, jde])
    grid = create_grid(distgrid, 'HYCOM grid', lon, lat, mask)
    call write_grid_to_netcdf(grid, 'hycom_grid.nc')

    fields = [                   &
      create_field(grid, 'u'),   &
      create_field(grid, 'v'),   &
      create_field(grid, 'ssh'), &
      create_field(grid, 'sst')  &
    ]

    call set_field_values(fields(1), real(u(1:ii,1:jj,1,1) + ubavg(1:ii,1:jj,1)))
    call set_field_values(fields(2), real(v(1:ii,1:jj,1,1) + vbavg(1:ii,1:jj,1)))
    call set_field_values(fields(3), real(srfhgt(1:ii,1:jj)) / 9.8)
    call set_field_values(fields(4), real(temp(1:ii,1:jj,1,1)))

    call ESMF_StateAdd(export_state, fields, rc=rc)
    call assert_success(rc)

    fields = [                        &
      create_field(grid, 'taux'),     &
      create_field(grid, 'tauy'),     &
      create_field(grid, 'rainrate'), &
      create_field(grid, 'swflux')    &
    ]
    call ESMF_StateAdd(import_state, fields, rc=rc)
    call assert_success(rc)

    rc = ESMF_SUCCESS
  end subroutine model_init


  subroutine model_run(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field

    call import_taux(import_state)
    call import_tauy(import_state)
    call import_precip(import_state)
    call import_swflux(import_state)

    call hycom_run()

    call export_ssh(export_state)
    call export_sst(export_state)
    call export_u(export_state)
    call export_v(export_state)

    rc = ESMF_SUCCESS
  end subroutine model_run


  subroutine model_finalize(gridded_component, import_state, export_state, clock, rc)
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    call hycom_final()
    rc = ESMF_SUCCESS
  end subroutine model_finalize


  subroutine set_hycom_time_limits(clock)
    ! Takes start and stop time from the input clock,
    ! and writes them into a file to be read by HYCOM.
    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time) :: start_time, stop_time, hycom_time
    integer :: days, hours
    real(ESMF_KIND_R8) :: start_day, end_day
    integer :: u

    call ESMF_ClockGet(clock, startTime=start_time)
    call ESMF_ClockGet(clock, stopTime=stop_time)

    call ESMF_TimeSet(hycom_time, yy=1900, mm=12, dd=31)

    call ESMF_TimeIntervalGet(start_time - hycom_time, d=days, h=hours)
    start_day = days + hours / 24d0
    call ESMF_TimeIntervalGet(stop_time - hycom_time, d=days, h=hours)
    end_day = days + hours / 24d0

    open(newunit=u, file='limits', status='unknown', action='write')
    write(u, *) start_day, end_day
    close(u)

  end subroutine set_hycom_time_limits


  subroutine import_taux(state)
    ! Updates the eastward component of surface stress in HYCOM.
    use mod_cb_arrays, only: taux
    use mod_dimensions, only: ii, jj
    type(ESMF_State), intent(in) :: state
    type(ESMF_Field) :: field
    integer :: lb(2), ub(2)
    real, pointer :: field_values(:,:)
    call ESMF_StateGet(state, 'taux', field)
    call get_field_values(field, field_values, lb, ub)
    taux(1:ii,1:jj,1) = field_values(lb(1):ub(1),lb(2):ub(2))
  end subroutine import_taux


  subroutine import_tauy(state)
    ! Updates the northward component of surface stress in HYCOM.
    use mod_cb_arrays, only: tauy
    use mod_dimensions, only: ii, jj
    type(ESMF_State), intent(in) :: state
    type(ESMF_Field) :: field
    integer :: lb(2), ub(2)
    real, pointer :: field_values(:,:)
    call ESMF_StateGet(state, 'tauy', field)
    call get_field_values(field, field_values, lb, ub)
    tauy(1:ii,1:jj,1) = field_values(lb(1):ub(1),lb(2):ub(2))
  end subroutine import_tauy


  subroutine import_precip(state)
    ! Updates the precipitation [m/s] field in HYCOM.
    use mod_cb_arrays, only: precip
    use mod_dimensions, only: ii, jj
    type(ESMF_State), intent(in) :: state
    type(ESMF_Field) :: field
    integer :: lb(2), ub(2)
    real, pointer :: field_values(:,:)
    call ESMF_StateGet(state, 'rainrate', field)
    call get_field_values(field, field_values, lb, ub)
    precip(1:ii,1:jj,1) = field_values(lb(1):ub(1),lb(2):ub(2))
  end subroutine import_precip


  subroutine import_swflux(state)
    ! Updates the shortwave radiative flux (positive downward) in HYCOM.
    use mod_cb_arrays, only: swflx
    use mod_dimensions, only: ii, jj
    type(ESMF_State), intent(in) :: state
    type(ESMF_Field) :: field
    integer :: lb(2), ub(2)
    real, pointer :: field_values(:,:)
    call ESMF_StateGet(state, 'swflux', field)
    call get_field_values(field, field_values, lb, ub)
    swflx(1:ii,1:jj,1) = field_values(lb(1):ub(1),lb(2):ub(2))
  end subroutine import_swflux


  subroutine export_ssh(state)
    ! Exports the sea surface height from HYCOM.
    use mod_cb_arrays, only: srfhgt
    use mod_dimensions, only: ii, jj
    type(ESMF_State), intent(in out) :: state
    type(ESMF_Field) :: field
    call ESMF_StateGet(state, 'ssh', field)
    call set_field_values(field, real(srfhgt(1:ii,1:jj)) / 9.8)
  end subroutine export_ssh


  subroutine export_sst(state)
    ! Exports the sea surface temperature from HYCOM.
    use mod_cb_arrays, only: temp
    use mod_dimensions, only: ii, jj
    type(ESMF_State), intent(in out) :: state
    type(ESMF_Field) :: field
    call ESMF_StateGet(state, 'sst', field)
    call set_field_values(field, real(temp(1:ii,1:jj,1,2)))
  end subroutine export_sst


  subroutine export_u(state)
    ! Exports the eastward component of surface current from HYCOM.
    use mod_cb_arrays, only: u, ubavg
    use mod_dimensions, only: ii, jj
    type(ESMF_State), intent(in out) :: state
    type(ESMF_Field) :: field
    call ESMF_StateGet(state, 'u', field)
    call set_field_values(field, real(u(1:ii,1:jj,1,2) + ubavg(1:ii,1:jj,2)))
  end subroutine export_u


  subroutine export_v(state)
    ! Exports the northward component of surface current from HYCOM.
    use mod_cb_arrays, only: v, vbavg
    use mod_dimensions, only: ii, jj
    type(ESMF_State), intent(in out) :: state
    type(ESMF_Field) :: field
    call ESMF_StateGet(state, 'v', field)
    call set_field_values(field, real(v(1:ii,1:jj,1,2) + vbavg(1:ii,1:jj,2)))
  end subroutine export_v


end module earthvm_hycom

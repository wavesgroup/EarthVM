module earthvm_hycom

  use ESMF !TODO , only: ...
  use earthvm_assert, only: assert_success
  use earthvm_esmf, only: create_distgrid, create_grid, create_field, set_field_values
  use earthvm_io, only: write_grid_to_netcdf
  use earthvm_model, only: earthvm_model_type
  use earthvm_state, only: earthvm_get_local_pet, earthvm_get_mpicomm

  use mod_cb_arrays, only: plon, plat, depths, temp, taux, tauy, &
                           u, v, ubavg, vbavg
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
      create_field(grid, 'sst'), &
      create_field(grid, 'u'),   &
      create_field(grid, 'v')    &
    ]

    call set_field_values(fields(1), real(temp(1:ii,1:jj,1,1)))
    call set_field_values(fields(2), real(u(1:ii,1:jj,1,1) + ubavg(1:ii,1:jj,1)))
    call set_field_values(fields(3), real(v(1:ii,1:jj,1,1) + vbavg(1:ii,1:jj,1)))

    call ESMF_StateAdd(export_state, fields, rc=rc)
    call assert_success(rc)

    fields = [create_field(grid, 'taux'), create_field(grid, 'tauy')]
    call set_field_values(fields(1), real(taux(1:ii,1:jj,1)))
    call set_field_values(fields(2), real(tauy(1:ii,1:jj,1)))
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

    call hycom_run()

    call ESMF_StateGet(export_state, 'sst', field)
    call set_field_values(field, real(temp(1:ii,1:jj,1,2)))

    call ESMF_StateGet(export_state, 'u', field)
    call set_field_values(field, real(u(1:ii,1:jj,1,2) + ubavg(1:ii,1:jj,2)))

    call ESMF_StateGet(export_state, 'v', field)
    call set_field_values(field, real(v(1:ii,1:jj,1,2) + vbavg(1:ii,1:jj,2)))

    call ESMF_StateGet(import_state, 'taux', field)
    call set_field_values(field, real(taux(1:ii,1:jj,1)))

    call ESMF_StateGet(import_state, 'tauy', field)
    call set_field_values(field, real(tauy(1:ii,1:jj,1)))

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

end module earthvm_hycom

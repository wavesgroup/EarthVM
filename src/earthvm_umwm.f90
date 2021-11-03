module earthvm_umwm

  use ESMF !TODO , only: ...
  use earthvm_assert, only: assert_success
  use earthvm_esmf, only: create_distgrid, create_grid, create_field, &
                          get_field_values, grid_rotation, set_field_values
  use earthvm_io, only: write_grid_to_netcdf
  use earthvm_state, only: earthvm_get_mpicomm, earthvm_get_local_pet, &
                           earthvm_get_pet_count
  use umwm_top, only: umwm_initialize, umwm_run, umwm_finalize

  implicit none

  private
  public :: set_services

  type(ESMF_RouteHandle) :: halo_route_handle
  real(ESMF_KIND_R4), allocatable :: alpha(:,:)

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

    use umwm_module, only: mm, nm, lon, lat, mask

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

    call umwm_get_tile_bounds(mm, nm, ips, ipe, jps, jpe)

    distgrid = create_distgrid([ips, jps], [ipe, jpe], [1, 1], [mm, nm])
    grid = create_grid(distgrid, 'UMWM grid', lon(ips:ipe,jps:jpe), &
                       lat(ips:ipe,jps:jpe), mask(ips:ipe,jps:jpe))
    call write_grid_to_netcdf(grid, 'umwm_grid.nc')
    
    ! Caluclate grid rotation for reuse in the run() subroutine
    ! Add one point buffer around each tile so that we have the grid rotation
    ! in the halo regions.
    allocate(alpha(ips-1:ipe+1,jps-1:jpe+1))
    alpha = grid_rotation(lon(ips-1:ipe+1,jps-1:jpe+1), lat(ips-1:ipe+1,jps-1:jpe+1))

    ! create and add import fields to import state
    fields = [create_field(grid, 'wspd'), &
              create_field(grid, 'wdir'), &
              create_field(grid, 'rhoa'), &
              create_field(grid, 'u'),    &
              create_field(grid, 'v'),    &
              create_field(grid, 'rhow')  &
              ]

    ! Compute and store the route handle for halo points;
    ! This can be done with any input field so we'll just use the first one.
    call ESMF_FieldHaloStore(fields(1), halo_route_handle)

    call ESMF_StateAdd(import_state, fields, rc=rc)
    call assert_success(rc)
    
    ! create and add export fields to import state
    fields = [                            &
      create_field(grid, 'u_stokes_1m'),  &
      create_field(grid, 'v_stokes_1m'),  &
      create_field(grid, 'u_stokes_sfc'), &
      create_field(grid, 'v_stokes_sfc'), &
      create_field(grid, 'taux_atm'),     &
      create_field(grid, 'tauy_atm'),     &
      create_field(grid, 'taux_ocn'),     &
      create_field(grid, 'tauy_ocn')      &
    ]
    call ESMF_StateAdd(export_state, fields, rc=rc)
    call assert_success(rc)

    rc = ESMF_SUCCESS
  end subroutine model_init


  subroutine model_run(gridded_component, import_state, export_state, clock, rc)

    use umwm_module, only: istart, iend, iistart, iiend, mm, nm, mi, ni, &
                           rhoa, rhow, rhorat, wspd, wdir,               &
                           taux_form, tauy_form, taux_skin, tauy_skin,   &
                           taux_ocntop, tauy_ocntop, taux_snl, tauy_snl, &
                           uc, vc
    use umwm_stokes, only: us, vs

    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_Time) :: current_time
    type(ESMF_TimeInterval) :: time_step
    character(256) :: start_time_string, stop_time_string

    integer :: i
    integer :: ips, ipe, jps, jpe

    ! For use with scalar fields
    type(ESMF_Field) :: field
    real, pointer :: field_values(:,:)
    
    ! For use with vector fields
    type(ESMF_Field) :: field_u, field_v
    real, pointer :: field_u_values(:,:), field_v_values(:,:)
    real(ESMF_KIND_R4) :: cos_a, sin_a
    
    integer :: lb(2), ub(2)
    
    integer :: local_pet, pet_count

    local_pet = earthvm_get_local_pet()
    pet_count = earthvm_get_pet_count()

    call ESMF_ClockGet(clock, timeStep=time_step, currTime=current_time)
    call ESMF_TimeGet(current_time, timeStringISOFrac=start_time_string)
    call ESMF_TimeGet(current_time + time_step, timeStringISOFrac=stop_time_string)

    start_time_string(11:11) = ' '
    stop_time_string(11:11) = ' '

    ! set import field values to UMWM arrays
    call ESMF_StateGet(import_state, 'wspd', field)
    call get_field_values(field, field_values, lb, ub)
    do concurrent(i = istart:iend)
      wspd(i) = field_values(mi(i),ni(i))
    end do

    call ESMF_StateGet(import_state, 'wdir', field)
    call get_field_values(field, field_values, lb, ub)
    do concurrent(i = istart:iend)
      wdir(i) = field_values(mi(i),ni(i))
    end do

    ! For currents, we need to update halo points as well
    ! because they're needed for wave propagation across tile edges.

    ! Rotate the current vector (u, v) from lat-lon to wave model grid

    call ESMF_StateGet(import_state, 'u', field_u)
    call ESMF_FieldHalo(field_u, halo_route_handle)
    call get_field_values(field_u, field_u_values, lb, ub)

    call ESMF_StateGet(import_state, 'v', field_v)
    call ESMF_FieldHalo(field_v, halo_route_handle)
    call get_field_values(field_v, field_v_values, lb, ub)

    do concurrent(i = iistart:iiend)

      ! Rotating back from lat-lon to local grid requires negative angle
      cos_a = cos(- alpha(mi(i),ni(i)))
      sin_a = sin(- alpha(mi(i),ni(i)))

      uc(i) = field_u_values(mi(i),ni(i)) * cos_a &
            - field_v_values(mi(i),ni(i)) * sin_a
      vc(i) = field_u_values(mi(i),ni(i)) * sin_a &
            + field_v_values(mi(i),ni(i)) * cos_a

    end do

    call ESMF_StateGet(import_state, 'rhoa', field)
    call get_field_values(field, field_values, lb, ub)
    do concurrent(i = istart:iend)
      rhoa(i) = field_values(mi(i),ni(i))
    end do

    call ESMF_StateGet(import_state, 'rhow', field)
    call get_field_values(field, field_values, lb, ub)
    do concurrent(i = istart:iend)
      rhow(i) = field_values(mi(i),ni(i)) + 1030
    end do
    rhorat = rhoa / rhow
    
    call umwm_run(trim(start_time_string), trim(stop_time_string))
    
    call umwm_get_tile_bounds(mm, nm, ips, ipe, jps, jpe)
    
    ! set export field values from UMWM arrays
    block
      real :: tmp_array(ips:ipe,jps:jpe)
    
      ! taux_atm
      tmp_array = 0
      do concurrent(i = istart:iend)
        tmp_array(mi(i),ni(i)) = taux_form(i) + taux_skin(i)
      end do
      call ESMF_StateGet(export_state, 'taux_atm', field)
      call set_field_values(field, tmp_array)
      
      ! tauy_atm
      tmp_array = 0
      do concurrent(i = istart:iend)
        tmp_array(mi(i),ni(i)) = tauy_form(i) + tauy_skin(i)
      end do
      call ESMF_StateGet(export_state, 'tauy_atm', field)
      call set_field_values(field, tmp_array)
      
      ! taux_ocn
      tmp_array = 0
      do concurrent(i = istart:iend)
        ! Rotating from local grid to lat-lon
        cos_a = cos(alpha(mi(i),ni(i)))
        sin_a = sin(alpha(mi(i),ni(i)))
        tmp_array(mi(i),ni(i)) = (taux_ocntop(i) - taux_snl(i)) * cos_a &
                               - (tauy_ocntop(i) - tauy_snl(i)) * sin_a
        !tmp_array(mi(i),ni(i)) = (taux_ocntop(i) - taux_snl(i))
      end do
      call ESMF_StateGet(export_state, 'taux_ocn', field)
      call set_field_values(field, tmp_array)
      
      ! tauy_ocn
      tmp_array = 0
      do concurrent(i = istart:iend)
        ! Rotating from local grid to lat-lon
        cos_a = cos(alpha(mi(i),ni(i)))
        sin_a = sin(alpha(mi(i),ni(i)))
        tmp_array(mi(i),ni(i)) = (taux_ocntop(i) - taux_snl(i)) * sin_a &
                               + (tauy_ocntop(i) - tauy_snl(i)) * cos_a
      end do
      call ESMF_StateGet(export_state, 'tauy_ocn', field)
      call set_field_values(field, tmp_array)

      ! Surface Stokes drift for WRF; don't need to rotate it since we're one
      ! the same grid in the two models.
      ! u_stokes
      tmp_array = 0
      do concurrent(i = istart:iend)
        tmp_array(mi(i),ni(i)) = us(i,1)
      end do
      call ESMF_StateGet(export_state, 'u_stokes_sfc', field)
      call set_field_values(field, tmp_array)

      ! v_stokes
      tmp_array = 0
      do concurrent(i = istart:iend)
        tmp_array(mi(i),ni(i)) = vs(i,1)
      end do
      call ESMF_StateGet(export_state, 'v_stokes_sfc', field)
      call set_field_values(field, tmp_array)

      ! Top 1-m layer integrated Stokes drift for HYCOM;
      ! Need to rotate it to the lat-lon grid.
      ! u_stokes_1m
      tmp_array = 0
      do concurrent(i = istart:iend)
        ! Rotating from local grid to lat-lon
        cos_a = cos(alpha(mi(i),ni(i)))
        sin_a = sin(alpha(mi(i),ni(i)))
        !FIXME don't hardcode Stokes depths
        tmp_array(mi(i),ni(i)) = 0.1 * (sum(us(i,1:10)) * cos_a &
                                      - sum(vs(i,1:10)) * sin_a)
      end do
      call ESMF_StateGet(export_state, 'u_stokes_1m', field)
      call set_field_values(field, tmp_array)

      ! v_stokes_1m
      tmp_array = 0
      do concurrent(i = istart:iend)
        ! Rotating from local grid to lat-lon
        cos_a = cos(alpha(mi(i),ni(i)))
        sin_a = sin(alpha(mi(i),ni(i)))
        !FIXME don't hardcode Stokes depths
        tmp_array(mi(i),ni(i)) = 0.1 * (sum(us(i,1:10)) * sin_a &
                                      + sum(vs(i,1:10)) * cos_a)
      end do
      call ESMF_StateGet(export_state, 'v_stokes_1m', field)
      call set_field_values(field, tmp_array)

    end block

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

  
  subroutine umwm_get_tile_bounds(ide, jde, ips, ipe, jps, jpe)
    ! Given domain extent indices, returns local tile extend indices.
    use umwm_module, only: istart, iend, mi, ni
    use umwm_init, only: remap_dir
    integer, intent(in) :: ide, jde
    integer, intent(out) :: ips, ipe, jps, jpe
    integer :: local_pet, pet_count
   
    local_pet = earthvm_get_local_pet()
    pet_count = earthvm_get_pet_count()
   
    if (remap_dir == 'v') then
      ! tiles are oriented in the y-axis
      ips = mi(istart)
      ipe = mi(iend)
      jps = 1
      jpe = jde
      if (local_pet == 0) ips = ips - 1
      if (local_pet == pet_count - 1) ipe = ipe + 1
    else if (remap_dir == 'h') then
      ! tiles are oriented in the x-axis
      ips = 1
      ipe = ide
      jps = ni(istart)
      jpe = ni(iend)
      if (local_pet == 0) jps = jps - 1
      if (local_pet == pet_count - 1) jpe = jpe + 1
    end if
  
  end subroutine umwm_get_tile_bounds


  subroutine umwm_set_boundary_values(array, ide, jde)
    ! Fills boundary values of a 2-d array with the immediate neighbor values.
    real, intent(in out) :: array(:,:)
    integer, intent(in) :: ide, jde
    integer :: ips, ipe, jps, jpe
    
    call umwm_get_tile_bounds(ide, jde, ips, ipe, jps, jpe)
    
    if (ips == 1) array(ips,:) = array(ips+1,:)
    if (ipe == ide) array(ipe,:) = array(ipe-1,:)
    if (jps == 1) array(:,jps) = array(:,jps+1)
    if (jpe == jde) array(:,jpe) = array(:,jpe-1)

  end subroutine umwm_set_boundary_values


end module earthvm_umwm

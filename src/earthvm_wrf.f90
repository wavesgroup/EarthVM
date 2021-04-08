module earthvm_wrf

  ! EarthVM interface to WRF.

  use ESMF !TODO , only: ...

  ! EarthVM module imports
  use earthvm_assert, only: assert, assert_success
  use earthvm_datetime, only: datetime
  use earthvm_esmf, only: create_distgrid, create_grid, create_field, &
                          get_field_values, set_field_values, get_grid, &
                          get_current_time_from_clock, get_stop_time_from_clock
  use earthvm_events, only: earthvm_event_type
  use earthvm_io, only: write_grid_to_netcdf
  use earthvm_model, only: earthvm_model_type
  use earthvm_state, only: earthvm_get_mpicomm, earthvm_get_local_pet
 
  ! WRF module imports
  use module_domain_type, only: domain
  use module_wrf_top, only: get_ijk_from_grid, head_grid, wrf_init, wrf_run, &
                            wrf_finalize

  implicit none

  private
  public :: set_services, domains

  ! Thin wrapper to hold the pointer to WRF domains,
  ! parent and inner nests alike
  type :: domain_ptr_type
    type(domain), pointer :: ptr
  end type domain_ptr_type

  ! This number is limited by the WRF implementation
  integer, parameter :: MAX_WRF_DOMAINS = 21

  type(domain_ptr_type) :: dom(MAX_WRF_DOMAINS)
  integer :: num_wrf_domains = 0
  integer :: time_step_count = 0

  type(earthvm_model_type), allocatable :: domains(:)

contains

  recursive integer function get_num_wrf_domains(dom) result(res)
    ! Returns the number of active WRF domains.
    use module_domain, only: domain_clockisstoptime
    type(domain), pointer, intent(in) :: dom
    integer :: n
    res = 1
    do n = 1, dom % num_nests
      if (.not. domain_clockisstoptime(dom % nests(n) % ptr)) &
        res = res + get_num_wrf_domains(dom % nests(n) % ptr)
    end do
  end function get_num_wrf_domains


  logical function nest_has_moved(dom, nest)
    ! Compares the lat, lon fields between the WRF internal domain structure
    ! and the one we have in EarthVM. If they're different, return .true. as the result.
    type(domain), pointer, intent(in) :: dom
    type(earthvm_model_type), intent(in) :: nest
    real(ESMF_KIND_R4), pointer :: lon_ptr(:,:), lat_ptr(:,:)
    integer :: lb(2), ub(2)
    integer :: ic, jc
    integer :: rc

    ! Get pointer to the longitude array and its bounds
    call ESMF_GridGetCoord(nest % grid, 1, farrayPtr=lon_ptr, &
                           exclusiveLBound=lb, exclusiveUBound=ub, rc=rc)
    call assert_success(rc)

    ! Get pointer to the latitude array and its bounds
    call ESMF_GridGetCoord(nest % grid, 2, farrayPtr=lat_ptr, rc=rc)
    call assert_success(rc)

    ic = lb(1) + (ub(1) - lb(1)) / 2
    jc = lb(2) + (ub(2) - lb(2)) / 2

    nest_has_moved = dom % xlong(ic,jc) /= lon_ptr(ic,jc) &
                .or. dom % xlat(ic,jc) /= lat_ptr(ic,jc)

  end function nest_has_moved


  subroutine get_wrf_array_bounds(dom, ids, ide, jds, jde, ips, ipe, jps, jpe)
    ! Thin wrapper around WRF's get_ijk_from_grid() subroutine.
    ! Given a WRF domain pointer, it returns the global and local start and end
    ! indices in x and y dimensions.
    ! The last point is excluded due to staggering.
    type(domain), pointer, intent(in) :: dom
    integer, intent(out) :: ids, ide, jds, jde ! global start and end indices
    integer, intent(out) :: ips, ipe, jps, jpe ! local start and end indices
    integer :: kds, kde, kps, kpe, ims, ime, jms, jme, kms, kme

    ! Get all indices from the WRF domain pointer
    call get_ijk_from_grid(dom,                          &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe)

    ! Exclude the last staggered grid cell
    ide = ide - 1
    jde = jde - 1
    ipe = min(ide, ipe)
    jpe = min(jde, jpe)

  end subroutine get_wrf_array_bounds


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


  type(earthvm_model_type) function new_wrf_domain(dom, name, import_state, export_state) result(nest)
    ! Creates a new EarthVM model data structure
    ! given WRF domain pointer and a name as input arguments.
    type(domain), pointer, intent(in) :: dom
    character(*), intent(in) :: name
    type(ESMF_State), intent(in out), optional :: import_state, export_state
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Grid) :: grid
    type(ESMF_Field), allocatable :: fields(:)
    integer :: rc
    integer :: ids, ide, jds, jde, ips, ipe, jps, jpe

    ! Create the new model instance.
    ! Arguments 2-4 are not used but must be provided.
    nest = earthvm_model_type(name, datetime(1), datetime(2), 1, set_services, nest=.true.)

    ! Get the WRF start and end bounds in x and y dimensions
    call get_wrf_array_bounds(dom, ids, ide, jds, jde, ips, ipe, jps, jpe)

    ! Create the ESMF distributed grid object
    distgrid = create_distgrid([ips, jps], [ipe, jpe], [ids, jds], [ide, jde])

    ! Create the grid based on WRF latitude, longitude, and seamask arrays
    grid = create_grid(distgrid, name // '_grid', &
                       lon=dom % xlong(ips:ipe, jps:jpe), &
                       lat=dom % xlat(ips:ipe, jps:jpe), &
                       mask=nint(dom % xland(ips:ipe, jps:jpe) - 1))
    nest % grid = grid

    ! Output the grid so we can verify that everything looks good
    call write_grid_to_netcdf(grid, name // '_grid.nc')

    ! Create import fields
    fields = [                         &
      create_field(grid, 'sst'),       &
      create_field(grid, 'taux_wav'),  &
      create_field(grid, 'tauy_wav'),  &
      create_field(grid, 'u_current'), &
      create_field(grid, 'v_current'), &
      create_field(grid, 'u_stokes'),  &
      create_field(grid, 'v_stokes')   &
    ]
    call ESMF_StateAdd(nest % import_state, fields, rc=rc)
    call assert_success(rc)

    ! If import_state was passed as an optional argument, add the fields to it
    if (present(import_state)) then
      call ESMF_StateAdd(import_state, fields, rc=rc)
      call assert_success(rc)
    end if

    ! Create export fields
    fields = [                              &
      create_field(grid, 'u10'),            &
      create_field(grid, 'v10'),            &
      create_field(grid, 'psfc'),           &
      create_field(grid, 'taux'),           &
      create_field(grid, 'tauy'),           &
      create_field(grid, 'rainrate'),       &
      create_field(grid, 'shortwave_flux'), &
      create_field(grid, 'total_flux'),     &
      create_field(grid, 'rhoa'),           &
      create_field(grid, 'wspd'),           &
      create_field(grid, 'wdir')            &
    ]

    call set_field_values(fields(1), dom % u10(ips:ipe,jps:jpe))
    call set_field_values(fields(2), dom % v10(ips:ipe,jps:jpe))
    call set_field_values(fields(3), dom % psfc(ips:ipe,jps:jpe))
    call set_field_values(fields(9), 1 / dom % alt(ips:ipe,1,jps:jpe))

    block
      real :: wspd(ips:ipe,jps:jpe)
      real :: wdir(ips:ipe,jps:jpe)
      real :: taux(ips:ipe,jps:jpe)
      real :: tauy(ips:ipe,jps:jpe)
      wspd = sqrt(dom % u10(ips:ipe,jps:jpe)**2 &
                + dom % v10(ips:ipe,jps:jpe)**2)
      wdir = atan2(dom % v10(ips:ipe,jps:jpe), dom % u10(ips:ipe,jps:jpe))
      taux = dom % ust(ips:ipe,jps:jpe)**2 * dom % u10(ips:ipe,jps:jpe) &
           / (wspd * dom % alt(ips:ipe,1,jps:jpe))
      tauy = dom % ust(ips:ipe,jps:jpe)**2 * dom % v10(ips:ipe,jps:jpe) &
           / (wspd * dom % alt(ips:ipe,1,jps:jpe))
      call set_field_values(fields(4), taux)
      call set_field_values(fields(5), tauy)
      call set_field_values(fields(10), wspd)
      call set_field_values(fields(11), wdir)
    end block

    block
      real :: rainrate(ips:ipe,jps:jpe)
      rainrate = (dom % raincv(ips:ipe,jps:jpe)   & ! from cumulus param.
                + dom % rainncv(ips:ipe,jps:jpe)) & ! explicit
                / dom % time_step                 & ! mm / time_step -> mm / s
                * 1e-3                              ! mm / s -> m / s
      call set_field_values(fields(6), rainrate)
    end block

    block
      real :: swflux(ips:ipe,jps:jpe)
      swflux = dom % swdown(ips:ipe,jps:jpe) * (1 - dom % albedo(ips:ipe,jps:jpe))
      call set_field_values(fields(7), swflux)
    end block

    block
      real :: radiative_flux(ips:ipe,jps:jpe)
      real :: enthalpy_flux(ips:ipe,jps:jpe)
      real(ESMF_KIND_R8), parameter :: sigma = 5.670374419d-8
      integer :: i, j
      do concurrent(i = ips:ipe, j = jps:jpe)
        ! positive downward (into the ocean)
        radiative_flux(i,j) = (dom % swdown(i,j) + dom % glw(i,j)) &
                            * (1 - dom % albedo(i,j))                    &
                            - dom % emiss(i,j) * sigma * dom % tsk(i,j)**4
        ! positive downward (into the ocean)
        enthalpy_flux(i,j) = - dom % hfx(i,j) - dom % lh(i,j)
      end do
      call set_field_values(fields(8), radiative_flux + enthalpy_flux)
    end block

    call ESMF_StateAdd(nest % export_state, fields, rc=rc)
    call assert_success(rc)

    ! If export_state was passed as an optional argument,
    ! add the fields to it. 
    if (present(export_state)) then
      call ESMF_StateAdd(export_state, fields, rc=rc)
      call assert_success(rc)
    end if

    call nest % write_to_netcdf()

  end function new_wrf_domain


  subroutine model_init(gridded_component, import_state, export_state, clock, rc)
    ! EarthVM interface to the WRF initialize procedure.
    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! This initializes the WRF MPI communicator.
    ! Because we already initialized the communicator in ESMF,
    ! we pass it to WRF here as an argument.
    call wrf_set_dm_communicator(earthvm_get_mpicomm())

    ! Call WRF's internal init() subroutine to initialize the model
    call wrf_init()

    ! Associate the 1st domain pointer with the parent domain
    dom(1) % ptr => head_grid

    ! Initialize parent domain grid and import and export states,
    ! and add it to the domains list
    domains = [new_wrf_domain(dom(1) % ptr, 'wrf', import_state, export_state)]

    ! Export fields for coupling with other models
    call set_export_fields(dom(1) % ptr, domains(1))

    rc = ESMF_SUCCESS
  end subroutine model_init


  subroutine model_run(gridded_component, import_state, export_state, clock, rc)

    use module_sf_sfclayrev, only: earthvm_momentum_coupling

    type(ESMF_GridComp) :: gridded_component
    type(ESMF_State) :: import_state, export_state
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(earthvm_model_type) :: nest
    integer :: n
    character(2) :: nest_number

    ! Increment the time step count, needed to determine whether we are ready
    ! to update roughness length in WRF
    time_step_count = time_step_count + 1

    ! Flip the coupling switch in the WRF surface layer module to override
    ! WRF's calculation of the surface roughness length
    if (time_step_count > 1) earthvm_momentum_coupling = .true.

    ! Associate the first domain with the WRF parent domain
    dom(1) % ptr => head_grid

    ! Get number of active domains (parent + nests)
    num_wrf_domains = get_num_wrf_domains(dom(1) % ptr)

    ! Associate our array of domain pointers with WRF domains
    do n = 1, num_wrf_domains
      if (associated(dom(n) % ptr % nests(1) % ptr)) then
        dom(n+1) % ptr => dom(n) % ptr % nests(1) % ptr
      end if
    end do

    ! Loop over active nests and if EarthVM model structure
    ! for the nest has not been created yet, create one now
    do n = 2, num_wrf_domains
      if (size(domains) < n) then
        write(nest_number, '(i2.2)') n
        domains = [domains, new_wrf_domain(dom(n) % ptr, 'wrf_d' // nest_number)]
      end if
    end do

    ! Update internal WRF arrays with the values from the EarthVM import fields
    do n = 1, num_wrf_domains
      call set_import_fields(dom(n) % ptr, domains(n))
      if (earthvm_momentum_coupling) then
        call set_friction_velocity(dom(n) % ptr, domains(n))
        call set_roughness_length(dom(n) % ptr, domains(n))
      end if
    end do

    ! Sychronize WRF's internal clock with the EarthVM clock
    call set_wrf_clock(clock)

    ! Run the model for one time step
    call wrf_run()

    ! Test if any of the nests have moved, and if yes,
    ! create a new data structure to track the new grid
    do n = 2, num_wrf_domains
      if (nest_has_moved(dom(n) % ptr, domains(n))) then
        write(nest_number, '(i2.2)') n
        domains(n) = new_wrf_domain(dom(n) % ptr, 'wrf_d' // nest_number)
      else
      end if
    end do

    ! Export fields for coupling with other models
    do n = 1, num_wrf_domains
      call set_export_fields(dom(n) % ptr, domains(n))
    end do

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


  subroutine set_import_fields(dom, wrf_domain)
    ! Updates the WRF arrays with the values from the import fields.
    type(domain), pointer, intent(in) :: dom
    type(earthvm_model_type), intent(in) :: wrf_domain
    type(ESMF_Field) :: field
    real, pointer :: field_values(:,:)
    integer :: lb(2), ub(2)
    integer :: ids, ide, jds, jde, ips, ipe, jps, jpe
    integer :: i, j

    ! Get the WRF start and end bounds in x and y dimensions
    call get_wrf_array_bounds(dom, ids, ide, jds, jde, ips, ipe, jps, jpe)

    ! Sea surface temperature
    call ESMF_StateGet(wrf_domain % import_state, 'sst', field)
    call get_field_values(field, field_values, lb, ub)
    if (any(field_values /= 0)) then
      do concurrent (i = ips:ipe, j = jps:jpe, dom % xland(i,j) > 1.5)
        dom % tsk(i,j) = field_values(i,j) + 273.15
      end do
    end if

    ! Zonal surface current
    call ESMF_StateGet(wrf_domain % import_state, 'u_current', field)
    call get_field_values(field, field_values, lb, ub)
    if (any(field_values /= 0)) then
      do concurrent (i = ips:ipe, j = jps:jpe, dom % xland(i,j) > 1.5)
        dom % earthvm_u_current(i,j) = field_values(i,j)
      end do
    end if

    ! Meridional surface current
    call ESMF_StateGet(wrf_domain % import_state, 'v_current', field)
    call get_field_values(field, field_values, lb, ub)
    if (any(field_values /= 0)) then
      do concurrent (i = ips:ipe, j = jps:jpe, dom % xland(i,j) > 1.5)
        dom % earthvm_v_current(i,j) = field_values(i,j)
      end do
    end if

    ! Zonal component of the wave-dependent stress vector
    call ESMF_StateGet(wrf_domain % import_state, 'taux_wav', field)
    call get_field_values(field, field_values, lb, ub)
    if (any(field_values /= 0)) then
      do concurrent (i = ips:ipe, j = jps:jpe, dom % xland(i,j) > 1.5)
        dom % earthvm_taux(i,j) = field_values(i,j)
      end do
    end if

    ! Meridional component of the wave-dependent stress vector
    call ESMF_StateGet(wrf_domain % import_state, 'tauy_wav', field)
    call get_field_values(field, field_values, lb, ub)
    if (any(field_values /= 0)) then
      do concurrent (i = ips:ipe, j = jps:jpe, dom % xland(i,j) > 1.5)
        dom % earthvm_tauy(i,j) = field_values(i,j)
      end do
    end if

    ! Zonal Stokes drift
    call ESMF_StateGet(wrf_domain % import_state, 'u_stokes', field)
    call get_field_values(field, field_values, lb, ub)
    if (any(field_values /= 0)) then
      do concurrent (i = ips:ipe, j = jps:jpe, dom % xland(i,j) > 1.5)
        dom % earthvm_u_stokes(i,j) = field_values(i,j)
      end do
    end if

    ! Meridional Stokes drift
    call ESMF_StateGet(wrf_domain % import_state, 'v_stokes', field)
    call get_field_values(field, field_values, lb, ub)
    if (any(field_values /= 0)) then
      do concurrent (i = ips:ipe, j = jps:jpe, dom % xland(i,j) > 1.5)
        dom % earthvm_v_stokes(i,j) = field_values(i,j)
      end do
    end if

  end subroutine set_import_fields


  subroutine set_export_fields(dom, wrf_domain)
    ! Updates the export fields for exchange with other models.
    type(domain), pointer, intent(in) :: dom
    type(earthvm_model_type), intent(in) :: wrf_domain
    type(ESMF_Field) :: field
    real, pointer :: field_values(:,:)
    integer :: lb(2), ub(2)
    integer :: ids, ide, jds, jde, ips, ipe, jps, jpe
    integer :: i, j

    ! Get the WRF start and end bounds in x and y dimensions
    call get_wrf_array_bounds(dom, ids, ide, jds, jde, ips, ipe, jps, jpe)

    ! Set x-component of 10-m wind
    call ESMF_StateGet(wrf_domain % export_state, 'u10', field)
    call set_field_values(field, dom % u10(ips:ipe,jps:jpe))

    ! Set y-component of 10-m wind
    call ESMF_StateGet(wrf_domain % export_state, 'v10', field)
    call set_field_values(field, dom % v10(ips:ipe,jps:jpe))

    ! Set surface air pressure
    call ESMF_StateGet(wrf_domain % export_state, 'psfc', field)
    call set_field_values(field, dom % psfc(ips:ipe,jps:jpe))
    
    ! Set air density in the lowest model layer
    call ESMF_StateGet(wrf_domain % export_state, 'rhoa', field)
    call set_field_values(field, 1 / dom % alt(ips:ipe,1,jps:jpe))

    block
      real :: wspd(ips:ipe,jps:jpe)
      real :: wdir(ips:ipe,jps:jpe)
      real :: taux(ips:ipe,jps:jpe)
      real :: tauy(ips:ipe,jps:jpe)

      wspd = sqrt(dom % u10(ips:ipe,jps:jpe)**2 &
                + dom % v10(ips:ipe,jps:jpe)**2)
      wdir = atan2(dom % v10(ips:ipe,jps:jpe), dom % u10(ips:ipe,jps:jpe))

      ! Set 10-m wind speed
      call ESMF_StateGet(wrf_domain % export_state, 'wspd', field)
      call set_field_values(field, wspd)

      ! Set 10-m wind direction
      call ESMF_StateGet(wrf_domain % export_state, 'wdir', field)
      call set_field_values(field, wdir)

      taux = dom % ust(ips:ipe,jps:jpe)**2 * dom % u10(ips:ipe,jps:jpe) &
           / (wspd * dom % alt(ips:ipe,1,jps:jpe))
      tauy = dom % ust(ips:ipe,jps:jpe)**2 * dom % v10(ips:ipe,jps:jpe) &
           / (wspd * dom % alt(ips:ipe,1,jps:jpe))

      ! Set x-component of surface stress (for coupling with ocean without waves)
      call ESMF_StateGet(wrf_domain % export_state, 'taux', field)
      call set_field_values(field, taux)

      ! Set y-component of surface stress (for coupling with ocean without waves)
      call ESMF_StateGet(wrf_domain % export_state, 'tauy', field)
      call set_field_values(field, tauy)

    end block

    !TODO force ustara in HYCOM based on taux and tauy

    ! Set precipitation minus evaporation
    block
      real :: rainrate(ips:ipe,jps:jpe)
      rainrate = ((dom % raincv(ips:ipe,jps:jpe)   & ! from cumulus param.
                 + dom % rainncv(ips:ipe,jps:jpe)) & ! explicit
                 / dom % time_step                 & ! mm / time_step -> mm / s
                 - dom % qfx(ips:ipe,jps:jpe))     &
                 * 1d-3                              ! mm / s -> m / s
      call ESMF_StateGet(wrf_domain % export_state, 'rainrate', field)
      call set_field_values(field, rainrate)
    end block

    ! Set shortwave radiative flux
    block
      real :: swflux(ips:ipe,jps:jpe)
      swflux = dom % swdown(ips:ipe,jps:jpe) &
             * (1 - dom % albedo(ips:ipe,jps:jpe))
      call ESMF_StateGet(wrf_domain % export_state, 'shortwave_flux', field)
      call set_field_values(field, swflux)
    end block

    ! Set total thermal flux (radiative + enthalpy)
    block
      real :: radiative_flux(ips:ipe,jps:jpe)
      real :: enthalpy_flux(ips:ipe,jps:jpe)
      real(ESMF_KIND_R8), parameter :: sigma = 5.67037321d-8
      do concurrent(i = ips:ipe, j = jps:jpe)
        ! positive downward (into the ocean)
        radiative_flux(i,j) = (dom % swdown(i,j) + dom % glw(i,j)) &
                            * (1 - dom % albedo(i,j))              &
                            - dom % emiss(i,j) * sigma * dom % tsk(i,j)**4
        ! positive downward (into the ocean)
        enthalpy_flux(i,j) = - dom % hfx(i,j) - dom % lh(i,j)
      end do
      call ESMF_StateGet(wrf_domain % export_state, 'total_flux', field)
      call set_field_values(field, radiative_flux + enthalpy_flux)
    end block

  end subroutine set_export_fields


  subroutine set_friction_velocity(dom, wrf_domain)
    ! Updates the friction velocity u* in a WRF domain instance based on the
    ! vector stress imported from the wave model and set as ESMF fields on
    ! wrf_domain.
    type(domain), pointer, intent(in) :: dom
    type(earthvm_model_type), intent(in) :: wrf_domain
    type(ESMF_Field) :: field
    integer :: lb(2), ub(2)
    integer :: ids, ide, jds, jde, ips, ipe, jps, jpe
    integer :: i, j
    real, pointer :: taux(:,:), tauy(:,:)

    ! Get the WRF start and end bounds in x and y dimensions
    call get_wrf_array_bounds(dom, ids, ide, jds, jde, ips, ipe, jps, jpe)

    call ESMF_StateGet(wrf_domain % import_state, 'taux_wav', field)
    call get_field_values(field, taux, lb, ub)

    call ESMF_StateGet(wrf_domain % import_state, 'tauy_wav', field)
    call get_field_values(field, tauy, lb, ub)

    do concurrent (i = ips:ipe, j = jps:jpe, dom % xland(i,j) > 1.5)
      dom % ust(i,j) = sqrt(sqrt(taux(i,j)**2 + tauy(i,j)**2) * dom % alt(i,1,j))
    end do

  end subroutine set_friction_velocity


  subroutine set_roughness_length(dom, wrf_domain)
    ! Updates the roughness length z0 in a WRF domain instance based on the
    ! vector stress imported from the wave model and set as ESMF fields on
    ! wrf_domain.
    type(domain), pointer, intent(in) :: dom
    type(earthvm_model_type), intent(in) :: wrf_domain
    type(ESMF_Field) :: field
    integer :: lb(2), ub(2)
    integer :: ids, ide, jds, jde, ips, ipe, jps, jpe
    integer :: i, j
    real :: psim10, psix10, wspd10, ust
    real, pointer :: taux(:,:), tauy(:,:)
    real, parameter :: VON_KARMAN = 0.4

    ! Get the WRF start and end bounds in x and y dimensions
    call get_wrf_array_bounds(dom, ids, ide, jds, jde, ips, ipe, jps, jpe)

    call ESMF_StateGet(wrf_domain % import_state, 'taux_wav', field)
    call get_field_values(field, taux, lb, ub)

    call ESMF_StateGet(wrf_domain % import_state, 'tauy_wav', field)
    call get_field_values(field, tauy, lb, ub)
    
    do concurrent (i = ips:ipe, j = jps:jpe, dom % xland(i,j) > 1.5)
      wspd10 = sqrt(dom % u10(i,j)**2 + dom % v10(i,j)**2)
      psix10 = wspd10 * dom % fm(i,j) / dom % wspd(i,j)
      psim10 = log(10 / dom % znt(i,j)) - psix10
      ust = sqrt(sqrt(taux(i,j)**2 + tauy(i,j)**2) * dom % alt(i,1,j))
      dom % znt(i,j) = 10 * exp(- VON_KARMAN * wspd10 / ust - psim10)
      dom % znt(i,j) = min(max(dom % znt(i,j), 1e-5), 1e1)
    end do

  end subroutine set_roughness_length


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

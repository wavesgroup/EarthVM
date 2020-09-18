module earthvm_wrf

  ! EarthVM interface to WRF.

  use ESMF !TODO , only: ...

  ! EarthVM module imports
  use earthvm_assert, only: assert, assert_success
  use earthvm_esmf, only: create_distgrid, create_grid, create_field, &
                          get_field_values, set_field_values
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
  public :: set_services

  ! Thin wrapper to hold the pointer to WRF domains,
  ! parent and inner nests alike
  type :: domain_ptr
    type(domain), pointer :: ptr
  end type domain_ptr

  type(domain_ptr), allocatable :: dom(:)
  integer :: num_wrf_domains = 0

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

    ! This initializes the WRF MPI communicator.
    ! Because we already initialized the communicator in ESMF,
    ! we pass it to WRF here as an argument.
    call wrf_set_dm_communicator(earthvm_get_mpicomm())

    ! This calls WRF's internal init() subroutine which initializes the model
    call wrf_init()

    ! Get the number of WRF domains. This won't register the nests before they start,
    ! however it will register them even after they're destroyed.
    num_wrf_domains = get_num_wrf_domains(head_grid)
    call assert(num_wrf_domains >= 1 .and. num_wrf_domains <= 21, &
                'WRF namelist parameter max_dom must be in the range [1, 21]')
    if (earthvm_get_local_pet() == 0) print *, 'num_wrf_domains', num_wrf_domains

    ! Associate the 1st domain pointer with the parent domain
    allocate(dom(num_wrf_domains))
    dom(1) % ptr => head_grid

    ! Get the global (*d*), memory (*m*), and local process (*p*) array bounds
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
                       lon=head_grid % xlong(ips:ipe, jps:jpe), &
                       lat=head_grid % xlat(ips:ipe, jps:jpe), &
                       mask=nint(head_grid % xland(ips:ipe, jps:jpe) - 1))

    call write_grid_to_netcdf(grid, 'wrf_grid.nc')

    fields = [create_field(grid, 'sst'),       &
              create_field(grid, 'taux_wav'),  &
              create_field(grid, 'tauy_wav'),  &
              create_field(grid, 'u_current'), &
              create_field(grid, 'v_current'), &
              create_field(grid, 'u_stokes'),  &
              create_field(grid, 'v_stokes')]
    call ESMF_StateAdd(import_state, fields, rc=rc)
    call assert_success(rc)

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

    call set_field_values(fields(1), head_grid % u10(ips:ipe,jps:jpe))
    call set_field_values(fields(2), head_grid % v10(ips:ipe,jps:jpe))
    call set_field_values(fields(3), head_grid % psfc(ips:ipe,jps:jpe))
    call set_field_values(fields(9), 1 / head_grid % alt(ips:ipe,1,jps:jpe))

    block
      real :: wspd(ips:ipe,jps:jpe)
      real :: wdir(ips:ipe,jps:jpe)
      real :: taux(ips:ipe,jps:jpe)
      real :: tauy(ips:ipe,jps:jpe)
      wspd = sqrt(head_grid % u10(ips:ipe,jps:jpe)**2 &
                + head_grid % v10(ips:ipe,jps:jpe)**2)
      wdir = atan2(head_grid % v10(ips:ipe,jps:jpe), head_grid % u10(ips:ipe,jps:jpe))
      taux = head_grid % ust(ips:ipe,jps:jpe)**2 * head_grid % u10(ips:ipe,jps:jpe) &
           / (wspd * head_grid % alt(ips:ipe,1,jps:jpe))
      tauy = head_grid % ust(ips:ipe,jps:jpe)**2 * head_grid % v10(ips:ipe,jps:jpe) &
           / (wspd * head_grid % alt(ips:ipe,1,jps:jpe))
      call set_field_values(fields(4), taux)
      call set_field_values(fields(5), tauy)
      call set_field_values(fields(10), wspd)
      call set_field_values(fields(11), wdir)
    end block

    block
      real :: rainrate(ips:ipe,jps:jpe)
      rainrate = (head_grid % raincv(ips:ipe,jps:jpe)   & ! from cumulus param.
                + head_grid % rainncv(ips:ipe,jps:jpe)) & ! explicit
                / head_grid % time_step                 & ! mm / time_step -> mm / s
                * 1d-3                                    ! mm / s -> m / s
      call set_field_values(fields(6), rainrate)
    end block

    block
      real :: swflux(ips:ipe,jps:jpe)
      swflux = head_grid % swdown(ips:ipe,jps:jpe) &
             * (1 - head_grid % albedo(ips:ipe,jps:jpe))
      call set_field_values(fields(7), swflux)
    end block

    block
      real :: radiative_flux(ips:ipe,jps:jpe)
      real :: enthalpy_flux(ips:ipe,jps:jpe)
      real(ESMF_KIND_R8), parameter :: sigma = 5.67037321d-8
      integer :: i, j
      do concurrent(i = ips:ipe, j = jps:jpe)
        ! positive downward (into the ocean)
        radiative_flux(i,j) = (head_grid % swdown(i,j) + head_grid % glw(i,j)) &
                            * (1 - head_grid % albedo(i,j))                    &
                            - head_grid % emiss(i,j) * sigma * head_grid % tsk(i,j)**4
        ! positive downward (into the ocean)
        enthalpy_flux(i,j) = - head_grid % hfx(i,j) - head_grid % lh(i,j)
      end do
      call set_field_values(fields(8), radiative_flux + enthalpy_flux)
    end block

    call ESMF_StateAdd(export_state, fields, rc=rc)
    call assert_success(rc)

    rc = ESMF_SUCCESS
  end subroutine model_init


  subroutine model_run(gridded_component, import_state, export_state, clock, rc)

    use module_sf_sfclayrev, only: earthvm_momentum_coupling

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
    
    num_wrf_domains = get_num_wrf_domains(head_grid)
    call assert(num_wrf_domains >= 1 .and. num_wrf_domains <= 21, &
                'WRF namelist parameter max_dom must be in the range [1, 21]')
    if (earthvm_get_local_pet() == 0) print *, 'num_wrf_domains', num_wrf_domains

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

    call ESMF_StateGet(import_state, 'taux_wav', field)
    call get_field_values(field, field_values, lb, ub)
    do concurrent (i = ips:ipe, j = jps:jpe, head_grid % xland(i,j) > 1.5)
      head_grid % earthvm_taux(i,j) = field_values(i,j)
    end do

    call ESMF_StateGet(import_state, 'tauy_wav', field)
    call get_field_values(field, field_values, lb, ub)
    do concurrent (i = ips:ipe, j = jps:jpe, head_grid % xland(i,j) > 1.5)
      head_grid % earthvm_tauy(i,j) = field_values(i,j)
    end do

    ! Set roughness length in WRF
    stress_coupling: block
      real :: psix10, wspd10, ust
      real :: psim10(ips:ipe,jps:jpe) ! stability function for momentum at 10-m height
      real, pointer :: taux(:,:), tauy(:,:)
      real, parameter :: von_karman_constant = 0.4

      call ESMF_StateGet(import_state, 'taux_wav', field)
      call get_field_values(field, taux, lb, ub)

      call ESMF_StateGet(import_state, 'tauy_wav', field)
      call get_field_values(field, tauy, lb, ub)

      do j = jps, jpe
        do i = ips, ipe
          if (head_grid % xland(i,j) > 1.5) then
            !wspd10 = sqrt(head_grid % u10(i,j)**2 + head_grid % v10(i,j)**2)
            !psix10 = wspd10 * head_grid % fm(i,j) / head_grid % wspd(i,j)
            !psim10(i,j) = log(10 / head_grid % znt(i,j)) - psix10
            !ust = sqrt(sqrt(taux(i,j)**2 + tauy(i,j)**2) * head_grid % alt(i,1,j))
            !head_grid % znt(i,j) = 10 * exp(- von_karman_constant * wspd10 / ust - psim10(i,j))
            !head_grid % znt(i,j) = max(head_grid % znt(i,j), 1e-5)
          end if
        end do
      end do

    end block stress_coupling

    call ESMF_StateGet(import_state, 'u_current', field)
    call get_field_values(field, field_values, lb, ub)
    do concurrent (i = ips:ipe, j = jps:jpe, head_grid % xland(i,j) > 1.5)
      head_grid % earthvm_u_current(i,j) = field_values(i,j)
    end do

    call ESMF_StateGet(import_state, 'v_current', field)
    call get_field_values(field, field_values, lb, ub)
    do concurrent (i = ips:ipe, j = jps:jpe, head_grid % xland(i,j) > 1.5)
      head_grid % earthvm_v_current(i,j) = field_values(i,j)
    end do

    call ESMF_StateGet(import_state, 'u_stokes', field)
    call get_field_values(field, field_values, lb, ub)
    do concurrent (i = ips:ipe, j = jps:jpe, head_grid % xland(i,j) > 1.5)
      head_grid % earthvm_u_stokes(i,j) = field_values(i,j)
    end do

    call ESMF_StateGet(import_state, 'v_stokes', field)
    call get_field_values(field, field_values, lb, ub)
    do concurrent (i = ips:ipe, j = jps:jpe, head_grid % xland(i,j) > 1.5)
      head_grid % earthvm_v_stokes(i,j) = field_values(i,j)
    end do

    call set_wrf_clock(clock)
    call wrf_run()
    
    ! flip the coupling switch in the WRF surface layer module to override
    ! WRF's calculation of the surface roughness length
    earthvm_momentum_coupling = .false.

    call ESMF_StateGet(export_state, 'u10', field)
    call set_field_values(field, head_grid % u10(ips:ipe,jps:jpe))

    call ESMF_StateGet(export_state, 'v10', field)
    call set_field_values(field, head_grid % v10(ips:ipe,jps:jpe))

    call ESMF_StateGet(export_state, 'psfc', field)
    call set_field_values(field, head_grid % psfc(ips:ipe,jps:jpe))
    
    call ESMF_StateGet(export_state, 'rhoa', field)
    call set_field_values(field, 1 / head_grid % alt(ips:ipe,1,jps:jpe))

    block
      real :: wspd(ips:ipe,jps:jpe)
      real :: wdir(ips:ipe,jps:jpe)
      real :: taux(ips:ipe,jps:jpe)
      real :: tauy(ips:ipe,jps:jpe)
      
      wspd = sqrt(head_grid % u10(ips:ipe,jps:jpe)**2 &
                + head_grid % v10(ips:ipe,jps:jpe)**2)
      wdir = atan2(head_grid % v10(ips:ipe,jps:jpe), head_grid % u10(ips:ipe,jps:jpe))
      
      call ESMF_StateGet(export_state, 'wspd', field)
      call set_field_values(field, wspd)
      
      call ESMF_StateGet(export_state, 'wdir', field)
      call set_field_values(field, wdir)
      
      taux = head_grid % ust(ips:ipe,jps:jpe)**2 * head_grid % u10(ips:ipe,jps:jpe) &
           / (wspd * head_grid % alt(ips:ipe,1,jps:jpe))
      tauy = head_grid % ust(ips:ipe,jps:jpe)**2 * head_grid % v10(ips:ipe,jps:jpe) &
           / (wspd * head_grid % alt(ips:ipe,1,jps:jpe))

      call ESMF_StateGet(export_state, 'taux', field)
      call set_field_values(field, taux)

      call ESMF_StateGet(export_state, 'tauy', field)
      call set_field_values(field, tauy)

    end block

    !TODO force ustara in HYCOM based on taux and tauy

    block
      ! precipitation minus evaporation
      real :: rainrate(ips:ipe,jps:jpe)
      rainrate = ((head_grid % raincv(ips:ipe,jps:jpe)   & ! from cumulus param.
                 + head_grid % rainncv(ips:ipe,jps:jpe)) & ! explicit
                 / head_grid % time_step                 & ! mm / time_step -> mm / s
                 - head_grid % qfx(ips:ipe,jps:jpe))     &
                 * 1d-3                                    ! mm / s -> m / s
      call ESMF_StateGet(export_state, 'rainrate', field)
      call set_field_values(field, rainrate)
    end block

    block
      real :: swflux(ips:ipe,jps:jpe)
      swflux = head_grid % swdown(ips:ipe,jps:jpe) &
             * (1 - head_grid % albedo(ips:ipe,jps:jpe))
      call ESMF_StateGet(export_state, 'shortwave_flux', field)
      call set_field_values(field, swflux)
    end block

    block
      real :: radiative_flux(ips:ipe,jps:jpe)
      real :: enthalpy_flux(ips:ipe,jps:jpe)
      real(ESMF_KIND_R8), parameter :: sigma = 5.67037321d-8
      integer :: i, j
      do concurrent(i = ips:ipe, j = jps:jpe)
        ! positive downward (into the ocean)
        radiative_flux(i,j) = (head_grid % swdown(i,j) + head_grid % glw(i,j)) &
                            * (1 - head_grid % albedo(i,j))                    &
                            - head_grid % emiss(i,j) * sigma * head_grid % tsk(i,j)**4
        ! positive downward (into the ocean)
        enthalpy_flux(i,j) = - head_grid % hfx(i,j) - head_grid % lh(i,j)
      end do
      call ESMF_StateGet(export_state, 'total_flux', field)
      call set_field_values(field, radiative_flux + enthalpy_flux)
    end block

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

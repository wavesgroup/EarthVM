module earthvm_esmf

  use ESMF ! , only: ...
  use earthvm_assert, only: assert_success
  use earthvm_constants, only: DEGREES_TO_RADIANS
  use earthvm_datetime, only: datetime
  use earthvm_state, only: earthvm_get_vm, earthvm_get_local_pet, earthvm_get_pet_count

  implicit none

  private

  public :: create_distgrid
  public :: create_grid
  public :: create_field
  public :: get_grid
  public :: get_current_time_from_clock
  public :: get_start_time_from_clock
  public :: get_stop_time_from_clock
  public :: get_field_values
  public :: get_itemlist_from_state
  public :: grid_rotation
  public :: set_field_values

  interface get_grid
    module procedure :: get_grid_from_field
    module procedure :: get_grid_from_gridded_component
  end interface get_grid

contains

  type(ESMF_DistGrid) function create_distgrid(pet_start_index, pet_end_index, &
                                               min_index, max_index) result(res)
    integer, intent(in) :: pet_start_index(2), pet_end_index(2)
    integer, intent(in) :: min_index(2), max_index(2)
    integer(ESMF_KIND_I4), allocatable :: tile_dimensions(:,:), de_block_list(:,:,:)
    integer(ESMF_KIND_I4), allocatable :: tile_dimensions_1d(:)
    integer :: local_pet, pet_count, n, rc

    local_pet = earthvm_get_local_pet()
    pet_count = earthvm_get_pet_count()

    allocate(tile_dimensions(4,earthvm_get_pet_count()))
    tile_dimensions = 0

    call ESMF_VMGather(vm       = earthvm_get_vm(),               &
                       sendData = [pet_start_index(1), pet_end_index(1),  &
                                   pet_start_index(2), pet_end_index(2)], &
                       recvData = tile_dimensions(:,local_pet+1), &
                       count    = 4,                              &
                       rootPET  = 0,                              &
                       rc       = rc)
    call assert_success(rc)

    allocate(tile_dimensions_1d(size(tile_dimensions)))
    tile_dimensions_1d = reshape(tile_dimensions, shape(tile_dimensions_1d))

    call ESMF_VMBroadcast(vm       = earthvm_get_vm(),      &
                          bcstData = tile_dimensions_1d,    &
                          count    = size(tile_dimensions), &
                          rootPET  = 0,                     &
                          rc       = rc)
    call assert_success(rc)

    tile_dimensions = reshape(tile_dimensions_1d, shape(tile_dimensions))

    allocate(de_block_list(2,2,pet_count))
    do n = 1, pet_count
      de_block_list(:,1,n) = [tile_dimensions(1,n), tile_dimensions(3,n)]
      de_block_list(:,2,n) = [tile_dimensions(2,n), tile_dimensions(4,n)]
    end do

    res = ESMF_DistGridCreate(minIndex    = min_index,         &
                              maxIndex    = max_index,         &
                              deBlockList = de_block_list,     &
                              indexflag   = ESMF_INDEX_GLOBAL, &
                              vm          = earthvm_get_vm(),  &
                              rc          = rc)
    call assert_success(rc)

  end function create_distgrid


  type(ESMF_Field) function create_field(grid, name, values) result(field)
    type(ESMF_Grid), intent(in) :: grid
    character(*), intent(in) :: name
    real(ESMF_KIND_R4), intent(in), optional :: values(:,:)

    type(ESMF_ArraySpec) :: arrayspec
    real(ESMF_KIND_R4), pointer :: field_data(:,:)
    integer :: rc

    call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R4, rc=rc)
    call assert_success(rc)

    field = ESMF_FieldCreate(grid, arrayspec, name=name,  &
                             indexFlag=ESMF_INDEX_GLOBAL, &
                             totalLWidth=[1, 1], totalUWidth=[1, 1], rc=rc)
    call assert_success(rc)

    call ESMF_FieldGet(field, localDE=0, farrayPtr=field_data, rc=rc)
    call assert_success(rc)

    field_data = 0
    if (present(values)) field_data = values

  end function create_field


  type(ESMF_Grid) function create_grid(distgrid, name, lon, lat, mask) result(grid)
    type(ESMF_DistGrid), intent(in) :: distgrid
    character(*), intent(in) :: name
    real, intent(in) :: lon(:,:), lat(:,:)
    integer, intent(in) :: mask(:,:)
    integer :: lb(2), ub(2)
    real(ESMF_KIND_R4), pointer :: lon_ptr(:,:), lat_ptr(:,:)
    integer(ESMF_KIND_I4), pointer :: mask_ptr(:,:)
    integer :: rc

    grid = ESMF_GridCreate(name          = name,                  &
                           coordsys      = ESMF_COORDSYS_SPH_DEG, &
                           distGrid      = distgrid,              &
                           coordTypeKind = ESMF_TYPEKIND_R4,      &
                           coordDimCount = [2, 2],                &
                           indexflag     = ESMF_INDEX_GLOBAL,     &
                           rc            = rc)
    call assert_success(rc)

    CALL ESMF_GridAddCoord(grid       = grid,                   &
                           staggerloc = ESMF_STAGGERLOC_CENTER, &
                           rc         = rc)
    call assert_success(rc)

    call ESMF_GridGetCoord(grid            = grid,                   &
                           localDE         = 0,                      &
                           CoordDim        = 1,                      &
                           staggerloc      = ESMF_STAGGERLOC_CENTER, &
                           exclusiveLBound = lb,                     &
                           exclusiveUBound = ub,                     &
                           farrayPtr       = lon_ptr,                &
                           rc              = rc)
    call assert_success(rc)

    lon_ptr(lb(1):ub(1), lb(2):ub(2)) = lon

    call ESMF_GridGetCoord(grid            = grid,                   &
                           localDE         = 0,                      &
                           CoordDim        = 2,                      &
                           staggerloc      = ESMF_STAGGERLOC_CENTER, &
                           exclusiveLBound = lb,                     &
                           exclusiveUBound = ub,                     &
                           farrayPtr       = lat_ptr,                &
                           rc              = rc)
    call assert_success(rc)

    lat_ptr(lb(1):ub(1), lb(2):ub(2)) = lat

    call ESMF_GridAddItem(grid       = grid,                   &
                          staggerloc = ESMF_STAGGERLOC_CENTER, &
                          itemflag   = ESMF_GRIDITEM_MASK,     &
                          rc         = rc)
    call assert_success(rc)

    call ESMF_GridGetItem(grid       = grid,                   &
                          localDE    = 0,                      &
                          staggerloc = ESMF_STAGGERLOC_CENTER, &
                          itemflag   = ESMF_GRIDITEM_MASK,     &
                          farrayPtr  = mask_ptr,               &
                          rc         = rc)
    call assert_success(rc)

    mask_ptr(lb(1):ub(1), lb(2):ub(2)) = mask

  end function create_grid


  type(ESMF_Grid) function get_grid_from_field(field) result(grid)
    type(ESMF_Field), intent(in) :: field
    integer :: rc
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    call assert_success(rc)
  end function get_grid_from_field


  type(ESMF_Grid) function get_grid_from_gridded_component(gridcomp) result(grid)
    type(ESMF_GridComp), intent(in) :: gridcomp
    integer :: rc
    call ESMF_GridCompGet(gridcomp, grid=grid, rc=rc)
    call assert_success(rc)
  end function get_grid_from_gridded_component


  function get_itemlist_from_state(state) result(itemlist)
    ! Returns an array of names of items contained in a state.
    type(ESMF_State), intent(in) :: state
    character(ESMF_MAXSTR), allocatable :: itemlist(:)
    integer :: itemcount, rc

    call ESMF_StateGet(state, itemCount=itemcount, rc=rc)
    call assert_success(rc)

    allocate(itemlist(itemcount))

    call ESMF_StateGet(state, itemNameList=itemlist, rc=rc)
    call assert_success(rc)

  end function get_itemlist_from_state


  subroutine get_field_values(field, field_values, lower_bounds, upper_bounds)
    ! Returns the pointer to field values and lower and upper bounds,
    ! given input field.
    type(ESMF_Field), intent(in) :: field
    real, pointer, intent(out) :: field_values(:,:)
    integer, intent(out), optional :: lower_bounds(2), upper_bounds(2)
    integer :: rc
    call ESMF_FieldGet(field,                        &
                       farrayPtr=field_values,       &
                       exclusiveLBound=lower_bounds, & 
                       exclusiveUBound=upper_bounds, &
                       rc=rc)
    call assert_success(rc)
  end subroutine get_field_values


  type(datetime) function get_current_time_from_clock(clock) result(time)
    ! Returns the current clock time.
    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time) :: current_esmf_time
    integer :: year, month, day, hour, minute, second
    integer :: rc

    call ESMF_ClockGet(clock, currTime=current_esmf_time, rc=rc)
    call assert_success(rc)

    call ESMF_TimeGet(current_esmf_time, yy=year, mm=month, dd=day, &
                      h=hour, m=minute, s=second, rc=rc)
    call assert_success(rc)

    time = datetime(year, month, day, hour, minute, second)

  end function get_current_time_from_clock


  type(datetime) function get_start_time_from_clock(clock) result(time)
    ! Returns the clock start time.
    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time) :: start_esmf_time
    integer :: year, month, day, hour, minute, second
    integer :: rc

    call ESMF_ClockGet(clock, startTime=start_esmf_time, rc=rc)
    call assert_success(rc)

    call ESMF_TimeGet(start_esmf_time, yy=year, mm=month, dd=day, &
                      h=hour, m=minute, s=second, rc=rc)
    call assert_success(rc)

    time = datetime(year, month, day, hour, minute, second)

  end function get_start_time_from_clock


  type(datetime) function get_stop_time_from_clock(clock) result(time)
    ! Returns the clock stop time.
    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time) :: stop_esmf_time
    integer :: year, month, day, hour, minute, second
    integer :: rc

    call ESMF_ClockGet(clock, stopTime=stop_esmf_time, rc=rc)
    call assert_success(rc)

    call ESMF_TimeGet(stop_esmf_time, yy=year, mm=month, dd=day, &
                      h=hour, m=minute, s=second, rc=rc)
    call assert_success(rc)

    time = datetime(year, month, day, hour, minute, second)

  end function get_stop_time_from_clock


  pure function grid_rotation(lon, lat) result(res)
    ! Returns the 2-dimensional array whose values are the angle
    ! of the x-coordinate of the grid relative to the zonal direction.
    real(ESMF_KIND_R4), intent(in) :: lon(:,:), lat(:,:)
    real(ESMF_KIND_R4), allocatable :: res(:,:), lon_scale(:,:)
    integer(ESMF_KIND_I4) :: i, j, im, jm

    im = size(lon, dim=1)
    jm = size(lon, dim=2)
    allocate(res(im,jm), lon_scale(im,jm))
    res = 0

    lon_scale = abs(cos(DEGREES_TO_RADIANS * lat))

    ! Inner points
    do concurrent(i = 2:im-1, j = 2:jm-1)
      res(i,j) = atan2(lat(i+1,j) - lat(i-1,j), &
                      (lon(i+1,j) - lon(i-1,j)) * lon_scale(i,j))
    end do

    ! West and East boundaries
    do concurrent(j = 2:jm-1)
      res(1,j)  = atan2(lat(2,j) - lat(1,j), &
                       (lon(2,j) - lon(1,j)) * lon_scale(1,j))
      res(im,j) = atan2(lat(im,j) - lat(im-1,j), &
                       (lon(im,j) - lon(im-1,j)) * lon_scale(im,j))
    end do

    ! South and North boundaries
    do concurrent(i = 2:im-1)
      res(i,1) = atan2(lat(i+1,1) - lat(i-1,1), &
                       (lon(i+1,1) - lon(i-1,1)) * lon_scale(i,1))
      res(i,jm) = atan2(lat(i+1,jm) - lat(i-1,jm), &
                       (lon(i+1,jm) - lon(i-1,jm)) * lon_scale(i,jm))
    end do

    ! Corner cells
    res(1,1) = atan2(lat(2,1) - lat(1,1), &
                    (lon(2,1) - lon(1,1)) * lon_scale(1,1))
    res(im,1) = atan2(lat(im,1) - lat(im-1,1), &
                     (lon(im,1) - lon(im-1,1)) * lon_scale(im,1))
    res(1,jm) = atan2(lat(2,jm) - lat(1,jm), &
                     (lon(2,jm) - lon(1,jm)) * lon_scale(1,jm))
    res(im,jm) = atan2(lat(im,jm) - lat(im-1,jm), &
                      (lon(im,jm) - lon(im-1,jm)) * lon_scale(im,jm))

  end function grid_rotation


  subroutine set_field_values(field, field_values)
    type(ESMF_Field), intent(in out) :: field
    real, intent(in) :: field_values(:,:)
    real, pointer :: field_data_pointer(:,:)
    integer :: lb(2), ub(2)
    integer :: rc
    call ESMF_FieldGet(field, farrayPtr=field_data_pointer, &
                       exclusiveLBound=lb, exclusiveUBound=ub, rc=rc)
    call assert_success(rc)
    field_data_pointer(lb(1):ub(1),lb(2):ub(2)) = field_values(:,:)
  end subroutine set_field_values

end module earthvm_esmf

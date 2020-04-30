module earthvm_io
  use iso_fortran_env, only: stderr => error_unit
  use ESMF
  use netcdf
  use earthvm_state, only: earthvm_finalize, earthvm_get_vm, &
                           earthvm_get_local_pet, earthvm_get_pet_count
  use earthvm_assert, only: assert_success
  implicit none
  private
  public :: write_grid_to_netcdf, write_fields_to_netcdf
contains

  subroutine write_grid_to_netcdf(grid, filename)
    type(ESMF_Grid), intent(in) :: grid
    character(*), intent(in) :: filename
    real, pointer :: lon_ptr(:,:), lat_ptr(:,:)
    integer, pointer :: mask_ptr(:,:)
    integer :: ncid, xdimid, ydimid, varid
    integer :: x_size, y_size
    integer(ESMF_KIND_I4) :: lb(2), ub(2), lb_global(2), ub_global(2)
    integer :: n, rc

    call ESMF_GridGetCoord(grid=grid, localDE=0, CoordDim=1,       &
                           exclusiveLBound=lb, exclusiveUBound=ub, &
                           farrayPtr=lon_ptr, rc=rc)
    call assert_success(rc)

    call ESMF_GridGetCoord(grid=grid, localDE=0, CoordDim=2, &
                           farrayPtr=lat_ptr, rc= rc)
    call assert_success(rc)

    call ESMF_GridGetItem(grid=grid, localDE=0, itemFlag=ESMF_GRIDITEM_MASK, &
                           farrayPtr=mask_ptr, rc= rc)
    call assert_success(rc)

    call ESMF_VMAllReduce(earthvm_get_vm(), lb, lb_global, 2, ESMF_REDUCE_MIN, rc=rc)
    call assert_success(rc)

    call ESMF_VMAllReduce(earthvm_get_vm(), ub, ub_global, 2, ESMF_REDUCE_MAX, rc=rc)
    call assert_success(rc)

    x_size = ub_global(1) - lb_global(1) + 1
    y_size = ub_global(2) - lb_global(2) + 1

    if (earthvm_get_local_pet() == 0) then
      call netcdf_check(nf90_create(filename, NF90_CLOBBER, ncid))
      call netcdf_check(nf90_def_dim(ncid, 'X', x_size, xdimid))
      call netcdf_check(nf90_def_dim(ncid, 'Y', y_size, ydimid))
      call netcdf_check(nf90_def_var(ncid, 'Longitude', NF90_FLOAT, [xdimid, ydimid], varid))
      call netcdf_check(nf90_def_var(ncid, 'Latitude', NF90_FLOAT, [xdimid, ydimid], varid))
      call netcdf_check(nf90_def_var(ncid, 'Seamask', NF90_INT, [xdimid, ydimid], varid))
      call netcdf_check(nf90_enddef(ncid))
      call netcdf_check(nf90_close(ncid))
    end if

    do n = 0, earthvm_get_pet_count() - 1
      if (earthvm_get_local_pet() == n) then
        call netcdf_check(nf90_open(filename, NF90_WRITE, ncid))
        call netcdf_check(nf90_inq_varid(ncid, 'Longitude', varid))
        call netcdf_check(nf90_put_var(ncid, varid, lon_ptr(lb(1):ub(1), lb(2):ub(2)), &
                                       start=[lb(1), lb(2)], &
                                       count=[ub(1)-lb(1)+1, ub(2)-lb(2)+1]))
        call netcdf_check(nf90_inq_varid(ncid, 'Latitude', varid))
        call netcdf_check(nf90_put_var(ncid, varid, lat_ptr(lb(1):ub(1), lb(2):ub(2)), &
                                       start=[lb(1), lb(2)], &
                                       count=[ub(1)-lb(1)+1, ub(2)-lb(2)+1]))
        call netcdf_check(nf90_inq_varid(ncid, 'Seamask', varid))
        call netcdf_check(nf90_put_var(ncid, varid, mask_ptr(lb(1):ub(1), lb(2):ub(2)), &
                                       start=[lb(1), lb(2)], &
                                       count=[ub(1)-lb(1)+1, ub(2)-lb(2)+1]))
        call netcdf_check(nf90_close(ncid))
      end if
      call ESMF_VMBarrier(earthvm_get_vm(), rc=rc)
    end do

  end subroutine write_grid_to_netcdf


  subroutine write_fields_to_netcdf(fields, filename)
    type(ESMF_Field), intent(in) :: fields(:)
    character(*), intent(in) :: filename
    type(ESMF_Grid) :: grid
    character(100) :: field_names(size(fields))
    real, pointer :: lon_ptr(:,:), lat_ptr(:,:), field_data_ptr(:,:)
    integer, pointer :: mask_ptr(:,:)
    integer :: ncid, xdimid, ydimid, tdimid, varid
    integer :: x_size, y_size
    integer(ESMF_KIND_I4) :: lb(2), ub(2), lb_global(2), ub_global(2)
    integer :: n, nfield, rc

    ! get field names
    do nfield = 1, size(fields)
      call ESMF_FieldGet(fields(nfield), name=field_names(nfield), rc=rc)
      call assert_success(rc)
    end do

    ! get grid
    call ESMF_FieldGet(fields(1), grid=grid, rc=rc)
    call assert_success(rc)

    call ESMF_GridGetCoord(grid=grid, localDE=0, CoordDim=1,       &
                           exclusiveLBound=lb, exclusiveUBound=ub, &
                           farrayPtr=lon_ptr, rc=rc)
    call assert_success(rc)

    call ESMF_GridGetCoord(grid=grid, localDE=0, CoordDim=2, &
                           farrayPtr=lat_ptr, rc= rc)
    call assert_success(rc)

    call ESMF_GridGetItem(grid=grid, localDE=0, itemFlag=ESMF_GRIDITEM_MASK, &
                           farrayPtr=mask_ptr, rc= rc)
    call assert_success(rc)

    call ESMF_VMAllReduce(earthvm_get_vm(), lb, lb_global, 2, ESMF_REDUCE_MIN, rc=rc)
    call assert_success(rc)

    call ESMF_VMAllReduce(earthvm_get_vm(), ub, ub_global, 2, ESMF_REDUCE_MAX, rc=rc)
    call assert_success(rc)

    x_size = ub_global(1) - lb_global(1) + 1
    y_size = ub_global(2) - lb_global(2) + 1

    if (earthvm_get_local_pet() == 0) then
      call netcdf_check(nf90_create(filename, NF90_CLOBBER, ncid))
      call netcdf_check(nf90_def_dim(ncid, 'X', x_size, xdimid))
      call netcdf_check(nf90_def_dim(ncid, 'Y', y_size, ydimid))
      call netcdf_check(nf90_def_dim(ncid, 'Time', 0, tdimid))
      call netcdf_check(nf90_def_var(ncid, 'Longitude', NF90_FLOAT, [xdimid, ydimid], varid))
      call netcdf_check(nf90_def_var(ncid, 'Latitude', NF90_FLOAT, [xdimid, ydimid], varid))
      call netcdf_check(nf90_def_var(ncid, 'Seamask', NF90_INT, [xdimid, ydimid], varid))
      do nfield = 1, size(fields)
        call netcdf_check(nf90_def_var(ncid, trim(field_names(nfield)), &
                                       NF90_FLOAT, [xdimid, ydimid, tdimid], varid))
      end do
      call netcdf_check(nf90_enddef(ncid))
      call netcdf_check(nf90_close(ncid))
    end if

    do n = 0, earthvm_get_pet_count() - 1
      if (earthvm_get_local_pet() == n) then
        call netcdf_check(nf90_open(filename, NF90_WRITE, ncid))
        call netcdf_check(nf90_inq_varid(ncid, 'Longitude', varid))
        call netcdf_check(nf90_put_var(ncid, varid, lon_ptr(lb(1):ub(1), lb(2):ub(2)), &
                                       start=[lb(1), lb(2), 1], &
                                       count=[ub(1)-lb(1)+1, ub(2)-lb(2)+1, 1]))
        call netcdf_check(nf90_inq_varid(ncid, 'Latitude', varid))
        call netcdf_check(nf90_put_var(ncid, varid, lat_ptr(lb(1):ub(1), lb(2):ub(2)), &
                                       start=[lb(1), lb(2), 1], &
                                       count=[ub(1)-lb(1)+1, ub(2)-lb(2)+1, 1]))
        call netcdf_check(nf90_inq_varid(ncid, 'Seamask', varid))
        call netcdf_check(nf90_put_var(ncid, varid, mask_ptr(lb(1):ub(1), lb(2):ub(2)), &
                                       start=[lb(1), lb(2), 1], &
                                       count=[ub(1)-lb(1)+1, ub(2)-lb(2)+1, 1]))

        do nfield = 1, size(fields)
          call ESMF_FieldGet(fields(nfield), farrayPtr=field_data_ptr, rc=rc)
          call assert_success(rc)
          call netcdf_check(nf90_inq_varid(ncid, field_names(nfield), varid))
          call netcdf_check(nf90_put_var(ncid, varid, &
            field_data_ptr(lb(1):ub(1), lb(2):ub(2)), &
            start=[lb(1), lb(2), 1], count=[ub(1)-lb(1)+1, ub(2)-lb(2)+1, 1]))
        end do
        call netcdf_check(nf90_close(ncid))

      end if
      call ESMF_VMBarrier(earthvm_get_vm(), rc=rc)
    end do

  end subroutine write_fields_to_netcdf


  subroutine netcdf_check(nc_status)
    integer, intent(in) :: nc_status
    if (nc_status /= NF90_NOERR) then
      write(stderr, *) 'Error in earthvm_io, netcdf_check: ' // &
                       trim(nf90_strerror(nc_status))
      call earthvm_finalize()
    end if
  end subroutine netcdf_check

end module earthvm_io

module earthvm_io
  use iso_fortran_env, only: stderr => error_unit
  use ESMF, only: ESMF_Grid, ESMF_VMAllReduce, ESMF_REDUCE_MIN, ESMF_REDUCE_MAX, &
                  ESMF_GridGetCoord, ESMF_KIND_I4, ESMF_GridGetItem, ESMF_GRIDITEM_MASK, &
                  ESMF_VMBarrier
  use netcdf, only: NF90_NOERR, NF90_NOWRITE, NF90_WRITE, NF90_CLOBBER, &
                    nf90_open, nf90_close, nf90_create, nf90_def_dim, &
                    nf90_def_var, nf90_enddef, nf90_put_var, nf90_strerror, &
                    NF90_FLOAT, NF90_INT, nf90_inq_varid
  use earthvm_state, only: earthvm_finalize, earthvm_get_vm, &
                           earthvm_get_local_pet, earthvm_get_pet_count
  use earthvm_assert, only: assert_success
  implicit none
  private
  public :: write_grid_to_netcdf
contains
  subroutine write_grid_to_netcdf(grid, filename)
    type(ESMF_Grid), intent(in) :: grid
    character(*), intent(in) :: filename
    real, pointer :: lon_ptr(:,:), lat_ptr(:,:)
    integer, pointer :: mask_ptr(:,:)
    integer :: ncid, xdimid, ydimid, varid, lonid, latid, maskid
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
      call netcdf_check(nf90_def_var(ncid, 'Longitude', NF90_FLOAT, [xdimid, ydimid], lonid))
      call netcdf_check(nf90_def_var(ncid, 'Latitude', NF90_FLOAT, [xdimid, ydimid], latid))
      call netcdf_check(nf90_def_var(ncid, 'Seamask', NF90_INT, [xdimid, ydimid], maskid))
      call netcdf_check(nf90_enddef(ncid))
      call netcdf_check(nf90_close(ncid))
    end if

    do n = 0, earthvm_get_pet_count() - 1
      if (earthvm_get_local_pet() == n) then
        call netcdf_check(nf90_open(filename, NF90_WRITE, ncid))
        call netcdf_check(nf90_inq_varid(ncid, 'Longitude', lonid))
        call netcdf_check(nf90_inq_varid(ncid, 'Latitude', latid))
        call netcdf_check(nf90_inq_varid(ncid, 'Seamask', maskid))
        call netcdf_check(nf90_put_var(ncid, lonid, lon_ptr(lb(1):ub(1), lb(2):ub(2)), &
                                       start=[lb(1), lb(2)], &
                                       count=[ub(1) - lb(1) + 1, ub(2) - lb(2) + 1]))
        call netcdf_check(nf90_put_var(ncid, latid, lat_ptr(lb(1):ub(1), lb(2):ub(2)), &
                                       start=[lb(1), lb(2)], &
                                       count=[ub(1) - lb(1) + 1, ub(2) - lb(2) + 1]))
        call netcdf_check(nf90_put_var(ncid, maskid, mask_ptr(lb(1):ub(1), lb(2):ub(2)), &
                                       start=[lb(1), lb(2)], &
                                       count=[ub(1) - lb(1) + 1, ub(2) - lb(2) + 1]))
        call netcdf_check(nf90_close(ncid))
      end if
      call ESMF_VMBarrier(earthvm_get_vm(), rc=rc)
    end do

  end subroutine write_grid_to_netcdf

  subroutine netcdf_check(nc_status)
    integer, intent(in) :: nc_status
    if (nc_status /= NF90_NOERR) then
      write(stderr, *) 'Error in earthvm_io, netcdf_check: ' // &
                       trim(nf90_strerror(nc_status))
      call earthvm_finalize()
    end if
  end subroutine netcdf_check

end module earthvm_io

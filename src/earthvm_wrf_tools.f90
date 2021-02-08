module earthvm_wrf_tools

  use earthvm_datetime, only: datetime
  implicit none

  private
  public :: get_start_time, get_end_time

contains

  type(datetime) function get_start_time() result(res)
    !TODO Unfortunately reading only some variables from a namelist doesn't work
    !TODO This needs to be parsed by hand
    integer :: start_year, start_month, start_day
    integer :: start_hour, start_minute, start_second
    integer :: u
    namelist /time_control/ start_year, start_month, start_day, &
                            start_hour, start_minute, start_second
    open(newunit=u, file='namelist.input', status='old', action='read')
    read(u, nml=time_control)
    close(u)
    res = datetime(start_year, start_month, start_day, &
                   start_hour, start_minute, start_second)
  end function get_start_time

  type(datetime) function get_end_time() result(res)
    !TODO Unfortunately reading only some variables from a namelist doesn't work
    !TODO This needs to be parsed by hand
    integer :: end_year, end_month, end_day
    integer :: end_hour, end_minute, end_second
    integer :: u
    namelist /time_control/ end_year, end_month, end_day, &
                            end_hour, end_minute, end_second
    open(newunit=u, file='namelist.input', status='old', action='read')
    read(u, nml=time_control)
    close(u)
    res = datetime(end_year, end_month, end_day, &
                   end_hour, end_minute, end_second)
  end function get_end_time

end module earthvm_wrf_tools
module earthvm_events
  use earthvm_str, only: str
  implicit none
  
  private
  public :: earthvm_event_type

  type :: earthvm_event_type
    type(str) :: name
    logical :: status
  end type earthvm_event_type

end module earthvm_events

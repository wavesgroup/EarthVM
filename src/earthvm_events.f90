module earthvm_events
  use earthvm_string, only: string
  implicit none
  private

  type :: earthvm_event_type
    type(string) :: name
    logical :: status
  end type earthvm_event_type

end module earthvm_events

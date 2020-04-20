program test_earthvm_initialize
  use earthvm_state, only: earthvm_initialize, earthvm_get_local_pet, &
                          earthvm_get_pet_count
  implicit none
  call earthvm_initialize()
  print *, 'EarthVM initialized on PET', earthvm_get_local_pet(), &
           'of', earthvm_get_pet_count()
end program test_earthvm_initialize

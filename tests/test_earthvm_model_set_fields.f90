program test_earthvm_model_set_fields
  use earthvm_assert, only: assert
  use earthvm_state, only: earthvm_initialize
  use earthvm_model, only: earthvm_model_type
  use earthvm_wrf, only: set_services
  use earthvm_datetime, only: datetime
  use earthvm_str, only: str
  implicit none
  type(earthvm_model_type) :: model
  call earthvm_initialize()
  model = earthvm_model_type('wrf', datetime(2020, 4, 1), datetime(2020, 4, 5), &
                             60, set_services)
  call model % set_import_fields([str('sst')])
  call model % set_export_fields([str('taux'), str('tauy')])
  call assert(all(model % import_fields == [str('sst')]), &
              'import_fields not set correctly')
  call assert(all(model % export_fields == [str('taux'), str('tauy')]), &
              'export_fields not set correctly')
end program test_earthvm_model_set_fields

project(VariationalRegistration)
enable_testing()

set(
  ExternalData_URL_TEMPLATES
  "http://jehrhardt.bplaced.net/VariationalRegistration/%(algo)/%(hash)"
)

itk_module_impl()

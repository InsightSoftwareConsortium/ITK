include_directories("${VariationalRegistration_SOURCE_DIR}/src")

find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

message(
  STATUS
  "Use Librariers:"
  "${ITK_LIBRARIES} ${VariationalRegistration_LIBRARIES}"
)

add_executable(itkVariationalRegistration VariationalRegistrationMain.cxx)
target_link_libraries(
  itkVariationalRegistration
  ${ITK_LIBRARIES}
  ${VariationalRegistration_LIBRARIES}
)

add_executable(itkVariationalRegistration2D VariationalRegistrationMain.cxx)
target_link_libraries(
  itkVariationalRegistration2D
  ${ITK_LIBRARIES}
  ${VariationalRegistration_LIBRARIES}
)
set_target_properties(
  itkVariationalRegistration2D
  PROPERTIES
    COMPILE_FLAGS
      -DUSE_2D_IMPL
)

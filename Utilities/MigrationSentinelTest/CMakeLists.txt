itk_add_test(
  NAME MigrationSentinelCollect
  COMMAND
    ${CMAKE_COMMAND}
    -DITK_SOURCE_DIR=${ITK_SOURCE_DIR}
    -P
    ${CMAKE_CURRENT_SOURCE_DIR}/CollectTest.cmake
)

itk_add_test(
  NAME MigrationSentinelDownstreamIdiom
  COMMAND
    ${CMAKE_COMMAND}
    -P
    ${CMAKE_CURRENT_SOURCE_DIR}/DownstreamIdiomTest.cmake
)

itk_add_test(
  NAME MigrationSentinelConfigureHeaderGuard
  COMMAND
    ${CMAKE_COMMAND}
    -DITK_CONFIGURE_HEADER=${ITK_BINARY_DIR}/Modules/Core/Common/itkConfigure.h
    -P
    ${CMAKE_CURRENT_SOURCE_DIR}/ConfigureHeaderGuardTest.cmake
)

itk_add_test(
  NAME MigrationSentinelConsumer
  COMMAND
    ${CMAKE_COMMAND}
    -DCONSUMER_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/Consumer
    -DCONSUMER_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}/ConsumerBuild
    -DITK_BINARY_DIR=${ITK_BINARY_DIR}
    -P
    ${CMAKE_CURRENT_SOURCE_DIR}/ConsumerTest.cmake
)

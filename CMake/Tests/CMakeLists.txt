add_test(
  NAME FFTWTargetArchitecture
  COMMAND
    "${CMAKE_COMMAND}" -S "${CMAKE_CURRENT_SOURCE_DIR}/FFTWTargetArchitecture"
    -B "${CMAKE_CURRENT_BINARY_DIR}/FFTWTargetArchitecture"
)
add_test(
  NAME ExternalModuleSettings
  COMMAND
    "${CMAKE_COMMAND}" -S "${CMAKE_CURRENT_SOURCE_DIR}/ExternalModuleSettings"
    -B "${CMAKE_CURRENT_BINARY_DIR}/ExternalModuleSettings"
)

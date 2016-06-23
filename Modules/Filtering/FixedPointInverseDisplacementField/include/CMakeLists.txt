# This is the root ITK CMakeLists file.
if(COMMAND CMAKE_POLICY)
endif()

project(ITKUtils)

# Find ITK.
find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

add_executable(InvertDeformationField InvertDeformationField.cxx)

target_link_libraries(InvertDeformationField ${ITK_LIBRARIES})

set(INSTALL_RUNTIME_DESTINATION bin CACHE STRING "Install destination")

install(
  TARGETS
    InvertDeformationField
  DESTINATION ${INSTALL_RUNTIME_DESTINATION}
)

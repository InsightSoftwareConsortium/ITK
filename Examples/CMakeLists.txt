if(NOT ITK_BUILD_DEFAULT_MODULES)
  message(FATAL_ERROR "BUILD_EXAMPLES requires ITK_BUILD_DEFAULT_MODULES to be ON")
endif()

find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

set(ITK_TEST_DRIVER itkTestDriver)

add_subdirectory(Iterators)
add_subdirectory(DataRepresentation)
add_subdirectory(Filtering)
add_subdirectory(IO)
add_subdirectory(Numerics)
add_subdirectory(Segmentation)
add_subdirectory(SpatialObjects)
add_subdirectory(Statistics)
add_subdirectory(RegistrationITKv4)

if(ITKV3_COMPATIBILITY)
  add_subdirectory(RegistrationITKv3)
endif()

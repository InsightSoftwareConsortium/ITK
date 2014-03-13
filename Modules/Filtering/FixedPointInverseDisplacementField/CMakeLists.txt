# This is the root ITK CMakeLists file.
if(COMMAND CMAKE_POLICY)
endif()

# Find ITK.
find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

add_executable(InvertDeformationField InvertDeformationField.cxx)

target_link_libraries(InvertDeformationField ${ITK_LIBRARIES})

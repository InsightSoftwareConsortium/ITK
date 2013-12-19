# This is the root ITK CMakeLists file.
cmake_minimum_required(VERSION 2.6)
if(COMMAND CMAKE_POLICY)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND CMAKE_POLICY)

# This project is designed to be built outside the Insight source tree.
project(PhaseSymmetryImageFilter)

# Find ITK.
find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

add_executable(PhaseSymmetryImageFilter2D main2.cxx)
target_link_libraries(PhaseSymmetryImageFilter2D ${ITK_LIBRARIES})

add_executable(PhaseSymmetryImageFilter3D main3.cxx)
target_link_libraries(PhaseSymmetryImageFilter3D ${ITK_LIBRARIES})

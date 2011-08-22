cmake_minimum_required(VERSION 2.8)

project(RANSAC)

#
# Find ITK
#
find_package(ITK)
if(ITK_FOUND)
  include(${ITK_USE_FILE})
else(ITK_FOUND)
  message(FATAL_ERROR "Cannot find ITK.  Please set ITK_DIR.")
endif(ITK_FOUND)

include_directories(
  ${RANSAC_SOURCE_DIR}
  ${RANSAC_SOURCE_DIR}/Common
)

set(
  RANSAC_HDRS
  RANSAC.h
  RANSAC.txx
  ParametersEstimator.h
  ParametersEstimator.txx
)

set(EXAMPLES_DIRECTORY ${RANSAC_SOURCE_DIR}/Examples)

link_libraries(
  ITKCommon
  itkvnl
  itkvnl_algo
)

#
# First example program showing the use of RANSAC for plane fitting
#
set(
  PLANE_ESTIMATION_HDRS
  ${RANSAC_HDRS}
  PlaneParametersEstimator.h
  PlaneParametersEstimator.txx
)

set(PLANE_ESTIMATION_SRCS ${EXAMPLES_DIRECTORY}/planeEstimation.cxx)

add_executable(
  planeEstimation
  ${PLANE_ESTIMATION_HDRS}
  ${PLANE_ESTIMATION_SRCS}
)
target_link_libraries(planeEstimation ${LINK_LIBRARIES})

#
# Second example program showing the use of RANSAC for sphere fitting
#
set(
  SPHERE_ESTIMATION_HDRS
  ${RANSAC_HDRS}
  SphereParametersEstimator.h
  SphereParametersEstimator.txx
)

set(SPHERE_ESTIMATION_SRCS ${EXAMPLES_DIRECTORY}/sphereEstimation.cxx)

add_executable(
  sphereEstimation
  ${SPHERE_ESTIMATION_HDRS}
  ${SPHERE_ESTIMATION_SRCS}
)
target_link_libraries(sphereEstimation ${LINK_LIBRARIES})

#
# Optional testing of the plane and sphere estimation code
#
option(BUILD_TESTING "Build the Testing directory." ON)
if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(Testing)
endif(BUILD_TESTING)

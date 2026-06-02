project(ITKproxTV)

set(ITKproxTV_THIRD_PARTY 1)

# Nothing of proxTV is built from source kept in the ITK tree; the
# library is cloned and built at build time via ExternalProject.
set(ITKproxTV_NO_SRC 1)

if(ITK_USE_SYSTEM_proxTV)
  set(ITKproxTV_LIBRARIES proxTV::proxTV)

  set(ITKproxTV_SYSTEM_INCLUDE_DIRS ${proxTV_INCLUDE_DIRS})
  set(
    ITKproxTV_EXPORT_CODE_INSTALL
    "
find_package(proxTV REQUIRED CONFIG)
"
  )
  set(
    ITKproxTV_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)
  find_package(proxTV REQUIRED CONFIG)
endif()
"
  )
  itk_module_impl()
else()
  # 3.23 Required for proxTV FILE_SETS, which allows for implicit installation of header files with the library target.
  cmake_minimum_required(VERSION 3.23.0)

  set(ITKproxTV_LIBRARIES proxTV)

  set(
    ${PROJECT_NAME}_EXPORT_CODE_INSTALL
    "
find_package(OpenMP)
  "
  )
  set(
    ${PROJECT_NAME}_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)
  find_package(OpenMP)
endif()
  "
  )

  set(proxTV_USE_LAPACK 0)
  set(proxTV_Eigen_LIBRARIES ${ITKEigen3_LIBRARIES})

  include(${CMAKE_CURRENT_LIST_DIR}/proxTVGitTag.cmake)

  FetchContent_Declare(
    proxtv_fetch
    GIT_REPOSITORY ${proxTV_GIT_REPOSITORY}
    GIT_TAG ${proxTV_GIT_TAG}
  )

  # end proxTV options

  FetchContent_MakeAvailable(proxTV_fetch)
endif()

itk_module_impl()
# require because proxTV does not use the ITK macros for the
itk_module_target(proxTV)

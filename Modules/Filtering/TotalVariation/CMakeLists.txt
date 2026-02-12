# 3.23 Required for proxTV FILE_SETS
project(TotalVariation)

set(TotalVariation_LIBRARIES proxTV::proxTV)

include(itk-module-init.cmake)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
endif()

include(FetchContent)

message(
  STATUS
  "TotalVariation_proxTV_USE_EIGEN: ${TotalVariation_proxTV_USE_EIGEN}"
)

# if proxTV is built elsewhere
if(ITK_USE_SYSTEM_proxTV)
  find_package(proxTV REQUIRED CONFIG)

  # It is needed to EXPORT_CODE_BUILD when using external proxTV
  set(
    ${PROJECT_NAME}_EXPORT_CODE_BUILD
    "${${PROJECT_NAME}_EXPORT_CODE_BUILD}
if(NOT ITK_BINARY_DIR)
  find_package(proxTV REQUIRED CONFIG)
endif()
"
  )
  # When this module is loaded by an app, it is needed to find proxTV and OpenMP
  set(
    ${PROJECT_NAME}_EXPORT_CODE_INSTALL
    "${${PROJECT_NAME}_EXPORT_CODE_INSTALL}
  find_package(proxTV REQUIRED CONFIG)
  "
  )
else() # build proxTV here with the selected Eigen3
  # Build proxTV with C++14
  if(NOT CMAKE_CXX_STANDARD)
    #    set(CMAKE_CXX_STANDARD 14)
  endif()
  if(NOT CMAKE_CXX_STANDARD_REQUIRED)
    #    set(CMAKE_CXX_STANDARD_REQUIRED ON)
  endif()
  if(NOT CMAKE_CXX_EXTENSIONS)
    #    set(CMAKE_CXX_EXTENSIONS OFF)
  endif()

  set(proxTV_Eigen_LIBRARIES ITK::ITKEigen3Module)

  set(
    proxTV_GIT_REPOSITORY
    "https://github.com/InsightSoftwareConsortium/proxTV.git"
  )
  set(proxTV_GIT_TAG "for/itk-proxtv-3.3.0-69062fe5")
  FetchContent_Declare(
    proxtv_fetch
    GIT_REPOSITORY ${proxTV_GIT_REPOSITORY}
    GIT_TAG ${proxTV_GIT_TAG}
  )

  # proxTV options:
  if(TotalVariation_proxTV_USE_EIGEN)
    set(proxTV_USE_LAPACK 0)
  else()
    set(proxTV_USE_LAPACK 1)
  endif()

  # end proxTV options

  FetchContent_MakeAvailable(proxTV_fetch)

  if(proxTV_USE_LAPACK)
    set(
      ${PROJECT_NAME}_EXPORT_CODE_BUILD
      "${${PROJECT_NAME}_EXPORT_CODE_BUILD}
if(NOT ITK_BINARY_DIR)
  find_package(LAPACKE REQUIRED)
  find_package(LAPACK REQUIRED)
endif()
"
    )
    set(
      ${PROJECT_NAME}_EXPORT_CODE_INSTALL
      "${${PROJECT_NAME}_EXPORT_CODE_INSTALL}
  find_package(LAPACKE REQUIRED)
  find_package(LAPACK REQUIRED)
  "
    )
  endif()
endif()

# Populate module variables
if(NOT ITK_SOURCE_DIR)
  include(ITKModuleExternal)
else()
  set(ITK_DIR ${CMAKE_BINARY_DIR})
  itk_module_impl()
endif()

itk_module_target(proxTV NAMESPACE proxTV::)

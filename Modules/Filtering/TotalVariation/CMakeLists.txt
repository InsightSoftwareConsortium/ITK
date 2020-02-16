project(TotalVariation)

if(ITK_USE_SYSTEM_proxTV)
  set(TotalVariation_LIBRARIES proxTV::proxTV)
else()
  set(TotalVariation_LIBRARIES proxTV)
endif()

set(${PROJECT_NAME}_THIRD_PARTY 1)

include(itk-module-init.cmake)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
endif()

include(CMakeParseArguments)
include(FetchContent)

# Compile definition needed for proxTV headers
# This will propagate to wrapping.
# For wrapping to work, requires ITK with patch:
# https://github.com/InsightSoftwareConsortium/ITK/pull/707
add_compile_definitions(NOMATLAB)

message(
  STATUS
  "TotalVariation_proxTV_USE_EIGEN: ${TotalVariation_proxTV_USE_EIGEN}"
)
if(TotalVariation_proxTV_USE_EIGEN)
  if(NOT ITK_USE_SYSTEM_EIGEN)
    # Set Eigen3_DIR to the internal ITKEigen3 module.
    set(_internal_cmake_eigen3)
    list(GET ITKEigen3_INCLUDE_DIRS 0 _internal_cmake_eigen3)
    set(Eigen3_DIR "${_internal_cmake_eigen3}/itkeigen")
  endif()
  find_package(Eigen3 REQUIRED)
  set(
    ${PROJECT_NAME}_EXPORT_CODE_INSTALL
    "${${PROJECT_NAME}_EXPORT_CODE_INSTALL}
set(Eigen3_DIR \"${Eigen3_DIR}\")
find_package(Eigen3 REQUIRED CONFIG)
"
  )
  set(
    ${PROJECT_NAME}_EXPORT_CODE_BUILD
    "${${PROJECT_NAME}_EXPORT_CODE_BUILD}
if(NOT ITK_BINARY_DIR)
  set(Eigen3_DIR \"${Eigen3_DIR}\")
  find_package(Eigen3 REQUIRED CONFIG)
endif()
"
  )
else()
  # proxTV will be build with external system LAPACK instead of Eigen3
  set(proxTV_USE_LAPACK 1)
  add_compile_definitions(PROXTV_USE_LAPACK)
endif()

# _proxTV_lib will be `proxTV::proxTV` when find_package, and `proxTV` when add_subdirectory(proxTV_folder)
set(_proxTV_lib "")
# if proxTV is built elsewhere
if(ITK_USE_SYSTEM_proxTV)
  find_package(proxTV REQUIRED CONFIG)
  set(_proxTV_lib proxTV::proxTV)
  set(proxTV_DIR_INSTALL ${proxTV_DIR})
  set(proxTV_DIR_BUILD ${proxTV_DIR})
  # It is only needed to EXPORT_CODE_BUILD when using external proxTV
  set(
    ${PROJECT_NAME}_EXPORT_CODE_BUILD
    "${${PROJECT_NAME}_EXPORT_CODE_BUILD}
if(NOT ITK_BINARY_DIR)
  set(proxTV_DIR \"${proxTV_DIR_BUILD}\")
  find_package(proxTV REQUIRED CONFIG)
endif()
"
  )
else() # build proxTV here with the selected Eigen3
  # Build proxTV with C++11
  if(NOT CMAKE_CXX_STANDARD)
    set(CMAKE_CXX_STANDARD 11)
  endif()
  if(NOT CMAKE_CXX_STANDARD_REQUIRED)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
  endif()
  if(NOT CMAKE_CXX_EXTENSIONS)
    set(CMAKE_CXX_EXTENSIONS OFF)
  endif()

  set(proxTV_GIT_REPOSITORY "https://github.com/phcerdan/proxTV.git")
  set(proxTV_GIT_TAG "use_eigen")
  FetchContent_Declare(
    proxtv_fetch
    GIT_REPOSITORY ${proxTV_GIT_REPOSITORY}
    GIT_TAG ${proxTV_GIT_TAG}
  )
  FetchContent_GetProperties(proxTV_fetch)
  set(Eigen3_DIR "${Eigen3_DIR}") # This is not needed but explicit helps reader.
  set(proxTV_USE_LAPACK 0)
  if(NOT proxtv_fetch_POPULATED)
    FetchContent_Populate(proxTV_fetch)
    add_subdirectory(${proxtv_fetch_SOURCE_DIR} ${proxtv_fetch_BINARY_DIR})
  endif()
  # proxTV will generate a target proxTV::proxTV when using find_package,
  # or a library proxTV when using add_subdirectory
  set(_proxTV_lib proxTV) # proxTV generated in subdirectory
  set(proxTV_DIR_INSTALL "\${ITK_MODULES_DIR}/../proxTV")

  # When this module is loaded by an app, load proxTV too.
  set(
    ${PROJECT_NAME}_EXPORT_CODE_BUILD
    "${${PROJECT_NAME}_EXPORT_CODE_BUILD}
set(proxTV_DIR \"${proxtv_fetch_BINARY_DIR}\")
find_package(proxTV REQUIRED CONFIG)
"
  )
endif()

# When this module is loaded by an app, load proxTV too.
set(
  ${PROJECT_NAME}_EXPORT_CODE_INSTALL
  "${${PROJECT_NAME}_EXPORT_CODE_INSTALL}
set(proxTV_DIR \"${proxTV_DIR_INSTALL}\")
find_package(proxTV REQUIRED CONFIG)
"
)

set(_populate_include_dirs_for_swig TRUE)
if(${_populate_include_dirs_for_swig})
  # SWIG (wrapping) requires INCLUDE_DIRS
  get_target_property(
    proxTV_INCLUDE_DIRS
    ${_proxTV_lib}
    INTERFACE_INCLUDE_DIRECTORIES
  )
  string(
    REGEX REPLACE
    ".*BUILD_INTERFACE:(.*/proxtv_fetch-build/src/include).*"
    "\\1"
    proxTV_INCLUDE_DIRS_STRIP
    ${proxTV_INCLUDE_DIRS}
  )
  # message(STATUS "proxTV_INCLUDE_DIRS: ${proxTV_INCLUDE_DIRS}")
  # message(STATUS "proxTV_INCLUDE_DIRS_STRIP: ${proxTV_INCLUDE_DIRS_STRIP}")
  set(TotalVariation_INCLUDE_DIRS ${proxTV_INCLUDE_DIRS_STRIP})
endif()

# Populate module variables
if(NOT ITK_SOURCE_DIR)
  include(ITKModuleExternal)
else()
  set(ITK_DIR ${CMAKE_BINARY_DIR})
  itk_module_impl()
endif()

# Add the proxTV library to Modules/Targets/TotalVariationTargets.cmake
itk_module_target(${_proxTV_lib} NO_INSTALL)

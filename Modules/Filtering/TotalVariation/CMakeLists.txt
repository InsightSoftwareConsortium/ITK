project(TotalVariation)

set(TotalVariation_LIBRARIES IsotropicWavelets)

set(${PROJECT_NAME}_THIRD_PARTY 1)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
endif()

include(CMakeParseArguments)
include(FetchContent)

# This be here, just in case, in the future we switch to use lapack instead of eigen for proxTV.
if(NOT DEFINED TotalVariation_proxTV_USE_EIGEN)
  option(
    TotalVariation_proxTV_USE_EIGEN
    "proxTV in TotalVariation uses EIGEN"
    ON
  )
endif()
# Add the option to use Eigen from outside, or the reduced Eigen install in ITK.
if(NOT DEFINED ITK_USE_SYSTEM_EIGEN3)
  option(
    ITK_USE_SYSTEM_Eigen3
    "Uses external Eigen3, when OFF uses the internal Eigen3 from ITK."
    ON
  )
endif()

if(TotalVariation_proxTV_USE_EIGEN)
  if(NOT ITK_USE_SYSTEM_EIGEN3)
    # TODO(phcerdan): Set Eigen3_DIR to the internal ITK.
    # Requires "installing" Eigen3 in ITK to generate the Config.cmake
    # The good thing is that find_package is compatible with both, external and internal usage.
    # XXX ${ITKEigen3_INCLUDE_DIRECTORIES} might include 2 folders in build mode, we are interested in the SOURCE (not BINARY) one.
    # set(Eigen3_DIR "${ITKEigen3_INCLUDE_DIRECTORIES}/itkeigen")
  else()
    # We do nothing, Eigen3_DIR can be populated by the user, or found in the system.
  endif()
else()
  # proxTV will be build with external system LAPACK instead of Eigen3
  set(proxTV_USE_LAPACK 1)
endif()

# This will have `proxTV::proxTV` when find_package, and `proxTV` when add_subdirectory(proxTV_folder)
set(_proxTV_lib "")
# if proxTV is built elsewhere
if(ITK_USE_SYSTEM_proxTV)
  find_package(proxTV REQUIRED CONFIG)
  set(_proxTV_lib proxTV::proxTV)

  # When this module is loaded by an app, load proxTV too.
  set(
    ${PROJECT_NAME}_EXPORT_CODE_INSTALL
    "
set(proxTV_DIR \"${proxTV_DIR}\")
find_package(proxTV REQUIRED CONFIG)
"
  )
  set(
    ${PROJECT_NAME}_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)
  set(CMAKE_MODULE_PATH
    \"${CMAKE_CURRENT_SOURCE_DIR}/CMake\" \${CMAKE_MODULE_PATH})
  set(proxTV_DIR \"${proxTV_DIR}\")
  find_package(proxTV REQUIRED CONFIG)
endif()
"
  )

  if(NOT ITK_SOURCE_DIR)
    include(ITKModuleExternal)
  else()
    itk_module_impl()
  endif()
else(ITK_USE_SYSTEM_proxTV) # build proxTV here with the selected Eigen3
  set(proxTV_EPNAME ${PROJECT_NAME}_ExtProject)
  set(lib_dir ${CMAKE_CURRENT_BINARY_DIR}/${proxTV_EPNAME}-build/lib)

  set(proxTV_GIT_REPOSITORY "https://github.com/phcerdan/proxTV.git")
  set(proxTV_GIT_TAG "use_eigen")
  # itk_download_attempt_check(${proxTV_EPNAME})
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

  # Populate module variables
  if(NOT ITK_SOURCE_DIR)
    include(ITKModuleExternal)
  else()
    itk_module_impl()
  endif()

  # Appending proxTV to PUBLIC_DEPENDS. Used by itk_module_link_dependencies() in itk_add_library()
  list(APPEND ITK_MODULE_TotalVariation_PUBLIC_DEPENDS ${_proxTV_lib})
  message(
    STATUS
    "ITK_MODULE_TotalVariation_PUBLIC_DEPENDS: ${ITK_MODULE_TotalVariation_PUBLIC_DEPENDS}"
  )
  # At build time, a proxTVTargets.cmake is generated. We will export append this to the module TotalVariationTargets.cmake. This will include Eigen, and all the dependencies.
  # Export 'proxTVTargets.cmake' for a build tree
  message(
    STATUS
    "Exporting proxTV targets to: ${itk-module}-targets-build: ${${itk-module}-targets-build}."
  )
  if(NOT EXISTS ${${itk-module}-targets-build})
    message(
      SEND_ERROR
      "External module targets_file ${itk-module}-targets-build not found in: ${${itk-module}-targets-build}."
    )
  endif()

  export(
    TARGETS
      ${_proxTV_lib}
    APPEND
    FILE ${${itk-module}-targets-build}
    NAMESPACE proxTV::
  )

  ############ INSTALL UNHANDLED in EXTERNAL modules: #########
  # Tell CPack to install proxTV stuff
  # list(APPEND CPACK_INSTALL_CMAKE_PROJECTS
  #   "${CMAKE_CURRENT_BINARY_DIR}/${proxTV_EPNAME}-build;proxTV;ALL;/")
  #
  # # run proxTV's cmake install script
  # install(SCRIPT
  #   ${CMAKE_CURRENT_BINARY_DIR}/${proxTV_EPNAME}-build/cmake_install.cmake)
endif(ITK_USE_SYSTEM_proxTV)

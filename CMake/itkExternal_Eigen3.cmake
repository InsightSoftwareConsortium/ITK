#
# Encapsulates building Eigen3 as an External Project.
include(ITK_CheckCCompilerFlag)

set(_additional_external_project_args)

# Because the header-only nature of Eigen3, EIGEN_MPL2_ONLY definition could be leaked outside ITK.
# This would wrongly enforce EIGEN_MPL2_ONLY to other libraries using Eigen.
# We wrap this definition in ITK_USE_EIGEN_MPL2_ONLY, and only enabling it internally in the dashboards and CI,
# to avoid introducing GPL code from Eigen3 internally in ITK.
option(ITK_USE_EIGEN_MPL2_ONLY "Set compile definition EIGEN_MPL2_ONLY for ITKInternalEigen3." OFF)
mark_as_advanced(ITK_USE_EIGEN_MPL2_ONLY)
if(ITK_USE_EIGEN_MPL2_ONLY)
  list(APPEND _additional_external_project_args "-DITK_USE_EIGEN_MPL2_ONLY:BOOL=ON")
endif()

if(ITK_USE_SYSTEM_EIGEN)
  find_package( Eigen3 )
else()
  # Set variables used to configure and install Eigen
  # _eigen3_cmake_install_prefix work also with relative CMAKE_INSTALL_PREFIX
  get_filename_component(_eigen3_cmake_install_prefix
    ${CMAKE_INSTALL_PREFIX} ABSOLUTE BASE_DIR ${PROJECT_BINARY_DIR})
  set(_eigen3_cmake_install_includedir ${ITK_INSTALL_INCLUDE_DIR})
  set(_eigen3_cmake_install_datadir ${ITK_INSTALL_PACKAGE_DIR}/Modules)
  set(_eigen3_source_dir "${ITK_SOURCE_DIR}/Modules/ThirdParty/Eigen3/src/itkeigen")
  set(_eigen3_build_dir "${ITK_BINARY_DIR}/ITKInternalEigen3-build")
  # ExternalProject_add creates a target for the build tree,
  # too late for our purposes of find_package at configure time.
  # We execute the configuration manually to create the Config and Targets.cmake files.
  # Nothing to link or compile, so no need to pass compiler flags.
  # However, generators and c,cxx compilers have to be explictily passed
  # for CMake configuration to work.
  # Configure Eigen
  file(MAKE_DIRECTORY ${_eigen3_build_dir})
  execute_process(
    COMMAND
    ${CMAKE_COMMAND} ${_eigen3_source_dir}
    "-DCMAKE_INSTALL_PREFIX=${_eigen3_cmake_install_prefix}"
    "-DCMAKE_INSTALL_INCLUDEDIR=${_eigen3_cmake_install_includedir}"
    "-DCMAKE_INSTALL_DATADIR=${_eigen3_cmake_install_datadir}"
    "-DCMAKE_GENERATOR=${CMAKE_GENERATOR}"
    "-DCMAKE_SH=${CMAKE_SH}"
    "-DCMAKE_GENERATOR_TOOLSET=${CMAKE_GENERATOR_TOOLSET}"
    "-DCMAKE_SH=${CMAKE_SH}"
    "-DCMAKE_GENERATOR_PLATFORM=${CMAKE_GENERATOR_PLATFORM}"
    "-DCMAKE_GENERATOR_INSTANCE=${CMAKE_GENERATOR_INSTANCE}"
    "-DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}"
    "-DCMAKE_C_FLAGS=${CMAKE_C_FLAGS}"
    "-DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}"
    "-DCMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS}"
    ${_additional_external_project_args}
    WORKING_DIRECTORY ${_eigen3_build_dir}
    OUTPUT_VARIABLE ITKEigen3Config_STDOUT
    ERROR_VARIABLE ITKEigen3Config_STDERR
    )
  set(ITKInternalEigen3_DIR ${_eigen3_build_dir})
  find_package( ITKInternalEigen3 )
  if(NOT ITKInternalEigen3_FOUND)
    message(FATAL_ERROR "ITKInternalEigen3 configuration faileed\nREPORT:\n${ITKEigen3Config_STDOUT}\n${ITKEigen3Config_STDERR}")
  endif()
  install(CODE
  "execute_process(
    COMMAND
    ${CMAKE_COMMAND} --build . --target install
    WORKING_DIRECTORY ${_eigen3_build_dir}
    )"
    COMPONENT Development)
endif()

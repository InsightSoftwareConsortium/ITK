#
# Encapsulates building Eigen3 as an External Project.
include(ITK_CheckCCompilerFlag)

set(_additional_external_project_args)
if(APPLE)
  if(DEFINED CMAKE_APPLE_SILICON_PROCESSOR)
    list(APPEND _additional_external_project_args
         -DCMAKE_APPLE_SILICON_PROCESSOR:STRING=${CMAKE_APPLE_SILICON_PROCESSOR})
  endif()
endif()

if(NOT CMAKE_CONFIGURATION_TYPES)
  list(APPEND _additional_external_project_args -DCMAKE_BUILD_TYPE:STRING=Release)
endif()

if(CMAKE_SH AND CMAKE_VERSION VERSION_LESS "3.17.0")
  # Setting CMAKE_SH is required when using "MinGW Makefiles" generator with CMake < 3.17
  # See https://github.com/InsightSoftwareConsortium/ITK/issues/66#issuecomment-424374973
  list(APPEND _additional_external_project_args -DCMAKE_SH:PATH=${CMAKE_SH})
endif()

# Because the header-only nature of Eigen3, EIGEN_MPL2_ONLY definition could be leaked outside ITK.
# This would wrongly enforce EIGEN_MPL2_ONLY to other libraries using Eigen.
# We wrap this definition in ITK_USE_EIGEN_MPL2_ONLY, and only enabling it internally in the dashboards and CI,
# to avoid introducing GPL code from Eigen3 internally in ITK.
option(ITK_USE_EIGEN_MPL2_ONLY "Set compile definition EIGEN_MPL2_ONLY for ITKInternalEigen3." OFF)
mark_as_advanced(ITK_USE_EIGEN_MPL2_ONLY)

if(ITK_USE_SYSTEM_EIGEN)
  find_package(Eigen3)
else()
  # Set variables used to configure and install Eigen
  # _eigen3_cmake_install_prefix work also with relative CMAKE_INSTALL_PREFIX
  get_filename_component(
    _eigen3_cmake_install_prefix
    ${CMAKE_INSTALL_PREFIX}
    ABSOLUTE
    BASE_DIR
    ${PROJECT_BINARY_DIR})
  set(_eigen3_cmake_install_includedir ${ITK_INSTALL_INCLUDE_DIR})
  set(_eigen3_cmake_install_datadir ${ITK_INSTALL_PACKAGE_DIR}/Modules)
  set(_eigen3_source_dir "${ITK_SOURCE_DIR}/Modules/ThirdParty/Eigen3/src/itkeigen")
  set(_eigen3_build_dir "${ITK_BINARY_DIR}/ITKInternalEigen3-build")
  # ExternalProject_add creates a target for the build tree,
  # too late for our purposes of find_package at configure time.
  # We execute the configuration manually to create the Config and Targets.cmake files.
  # Nothing to link or compile, so no need to pass compiler flags.
  # However, generators and c,cxx compilers have to be explicitly passed
  # for CMake configuration to work.
  # Configure Eigen
  execute_process(
    COMMAND
      ${CMAKE_COMMAND} -DCMAKE_SYSTEM_VERSION:STRING=${CMAKE_SYSTEM_VERSION} -DCMAKE_GENERATOR:STRING=${CMAKE_GENERATOR}
      -DCMAKE_GENERATOR_TOOLSET:STRING=${CMAKE_GENERATOR_TOOLSET}
      -DCMAKE_GENERATOR_PLATFORM:STRING=${CMAKE_GENERATOR_PLATFORM}
      -DCMAKE_GENERATOR_INSTANCE:STRING=${CMAKE_GENERATOR_INSTANCE} -DCMAKE_C_COMPILER:PATH=${CMAKE_C_COMPILER}
      -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS} -DCMAKE_CXX_COMPILER:PATH=${CMAKE_CXX_COMPILER}
      -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS} -DCMAKE_OSX_ARCHITECTURES:STRING=${CMAKE_OSX_ARCHITECTURES}
      -DCMAKE_INSTALL_PREFIX:PATH=${_eigen3_cmake_install_prefix}
      -DCMAKE_INSTALL_INCLUDEDIR:PATH=${_eigen3_cmake_install_includedir}
      -DCMAKE_INSTALL_DATADIR:PATH=${_eigen3_cmake_install_datadir}
      -DITK_USE_EIGEN_MPL2_ONLY:BOOL=${ITK_USE_EIGEN_MPL2_ONLY} ${_additional_external_project_args} -S
      ${_eigen3_source_dir} -B ${_eigen3_build_dir}
    OUTPUT_VARIABLE _ITKEigen3Config_OUTPUT
    ERROR_VARIABLE  _ITKEigen3Config_OUTPUT
    RESULT_VARIABLE _ITKEigen3Config_RESULT
    )
  if(NOT _ITKEigen3Config_RESULT EQUAL 0)
    string(REPLACE "\n" "\n  " _ITKEigen3Config_OUTPUT "${_ITKEigen3Config_OUTPUT}")
    message(FATAL_ERROR
      "ITKInternalEigen3 configuration failed:\n"
      "  ${_ITKEigen3Config_OUTPUT}\n"
      )
  endif()
  if(NOT EXISTS "${_eigen3_build_dir}/ITKInternalEigen3Config.cmake")
    message(FATAL_ERROR
      "ITKInternalEigen3 configuration did not produce expected file:\n"
      "  ${_eigen3_build_dir}/ITKInternalEigen3Config.cmake\n"
      )
  endif()
  set(ITKInternalEigen3_DIR ${_eigen3_build_dir})
  find_package(ITKInternalEigen3 CONFIG REQUIRED QUIET)
  install(
    CODE "execute_process(
    COMMAND \${CMAKE_COMMAND} --build . --config Release --target install
    WORKING_DIRECTORY \"${_eigen3_build_dir}\"
    )"
    COMPONENT Development)
endif()

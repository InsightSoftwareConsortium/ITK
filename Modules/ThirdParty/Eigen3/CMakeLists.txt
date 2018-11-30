project(ITKEigen3)
set(ITKEigen3_THIRD_PARTY 1)
set(ITKEigen3_LIBRARIES Eigen3::Eigen)
# Assume that Eigen generates a Eigen3Config.cmake
set(_Eigen3_min_version 3.3)

option(ITK_USE_SYSTEM_EIGEN "Use an outside build of Eigen3." ${ITK_USE_SYSTEM_LIBRARIES})
mark_as_advanced(ITK_USE_SYSTEM_EIGEN)

if(ITK_USE_SYSTEM_EIGEN)
  find_package(Eigen3 ${_Eigen3_min_version} REQUIRED CONFIG)
  set(Eigen3_DIR_INSTALL ${Eigen3_DIR})
  set(Eigen3_DIR_BUILD ${Eigen3_DIR})
else()
  set(Eigen3_DIR_INSTALL "${CMAKE_INSTALL_PREFIX}/${ITK_INSTALL_LIBRARY_DIR}/cmake/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Modules")
  set(Eigen3_DIR_BUILD "${CMAKE_CURRENT_BINARY_DIR}/src/itkeigen")
endif()

# Eigen3 is header only, but there are compile definitions that we want to provide
# to enforce use of MPL only code, and to disable warnings.
# We only need the BINARY_DIR, we rely on the target Eigen3::Eigen for the include headers.
# SOURCE_DIR is added to be consistent with the way ITKModules, wrapping does not work without it.
set(ITKEigen3_INCLUDE_DIRS
  ${ITKEigen3_BINARY_DIR}/src
  ${ITKEigen3_SOURCE_DIR}/src
  )

if(ITK_USE_SYSTEM_EIGEN)
  get_target_property(Eigen_INCLUDE_DIRS Eigen3::Eigen INTERFACE_INCLUDE_DIRECTORIES)
  set(ITKEigen3_SYSTEM_INCLUDE_DIRS
    ${Eigen_INCLUDE_DIRS}
    )
endif()

# When this module is loaded by an app, load Eigen too.
set(ITKEigen3_EXPORT_CODE_INSTALL "
set(Eigen3_DIR \"${Eigen3_DIR_INSTALL}\")
find_package(Eigen3 ${_Eigen3_min_version} REQUIRED CONFIG)
")
set(ITKEigen3_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(Eigen3_DIR \"${Eigen3_DIR_BUILD}\")
  find_package(Eigen3 ${_Eigen3_min_version} REQUIRED CONFIG)
endif()
")

configure_file(src/itk_eigen.h.in src/itk_eigen.h)
itk_module_impl()

install(FILES ${ITKEigen3_BINARY_DIR}/src/itk_eigen.h
  DESTINATION ${ITKEigen3_INSTALL_INCLUDE_DIR}
  COMPONENT Development
  )

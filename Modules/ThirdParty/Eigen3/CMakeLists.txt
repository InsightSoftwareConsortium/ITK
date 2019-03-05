project(ITKEigen3)
set(ITKEigen3_THIRD_PARTY 1)
if(ITK_USE_SYSTEM_EIGEN)
  set(_eigen_itk_target Eigen3::Eigen)
else()
  set(_eigen_itk_target ITKInternalEigen3::Eigen)
endif()
set(ITKEigen3_LIBRARIES ${_eigen_itk_target})
# Assume that Eigen generates a Eigen3Config.cmake
set(_Eigen3_min_version 3.3)

option(ITK_USE_SYSTEM_EIGEN "Use an outside build of Eigen3." ${ITK_USE_SYSTEM_LIBRARIES})
mark_as_advanced(ITK_USE_SYSTEM_EIGEN)

if(ITK_USE_SYSTEM_EIGEN)
  set(_Eigen3_SYSTEM_OR_INTERNAL "Eigen3")
else()
  set(_Eigen3_SYSTEM_OR_INTERNAL "ITKInternalEigen3")
endif()

if(ITK_USE_SYSTEM_EIGEN)
  message(STATUS "_Eigen3_SYSTEM_OR_INTERNAL: ${_Eigen3_SYSTEM_OR_INTERNAL}")
  find_package(${_Eigen3_SYSTEM_OR_INTERNAL} ${_Eigen3_min_version} REQUIRED CONFIG)
  set(Eigen3_DIR_INSTALL ${Eigen3_DIR})
  set(Eigen3_DIR_BUILD ${Eigen3_DIR})
else()
  set(Eigen3_DIR_INSTALL "\${ITK_MODULES_DIR}")
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
# Load ITKInternalEigen3 or Eigen3 depending on ITK_USE_SYSTEM_EIGEN
set(ITKEigen3_EXPORT_CODE_INSTALL "
set(${_Eigen3_SYSTEM_OR_INTERNAL}_DIR \"${Eigen3_DIR_INSTALL}\")
find_package(${_Eigen3_SYSTEM_OR_INTERNAL} ${_Eigen3_min_version} REQUIRED CONFIG)
")
set(ITKEigen3_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(${_Eigen3_SYSTEM_OR_INTERNAL}_DIR \"${Eigen3_DIR_BUILD}\")
  find_package(${_Eigen3_SYSTEM_OR_INTERNAL} ${_Eigen3_min_version} REQUIRED CONFIG)
endif()
")

itk_module_impl()

configure_file(src/itk_eigen.h.in src/itk_eigen.h)
install(FILES ${ITKEigen3_BINARY_DIR}/src/itk_eigen.h
  DESTINATION ${ITKEigen3_INSTALL_INCLUDE_DIR}
  COMPONENT Development
  )

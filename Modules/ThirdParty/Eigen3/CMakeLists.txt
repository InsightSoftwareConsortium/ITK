project(ITKEigen3)
set(ITKEigen3_THIRD_PARTY 1)

option(
  ITK_USE_SYSTEM_EIGEN
  "Use an outside build of Eigen3."
  ${ITK_USE_SYSTEM_LIBRARIES}
)
mark_as_advanced(ITK_USE_SYSTEM_EIGEN)

option(
  ITK_USE_EIGEN_MPL2_ONLY
  "Set compile definition EIGEN_MPL2_ONLY for eigen_internal."
  OFF
)
mark_as_advanced(ITK_USE_EIGEN_MPL2_ONLY)

set(_Eigen3_min_version 3.3)

if(ITK_USE_SYSTEM_EIGEN)
  set(_Eigen3_SYSTEM_OR_INTERNAL "Eigen3")
  find_package(${_Eigen3_SYSTEM_OR_INTERNAL} REQUIRED CONFIG)
  set(ITKEigen3_LIBRARIES Eigen3::Eigen)

  set(ITKEigen3_NO_SRC 1)

  if(${_Eigen3_SYSTEM_OR_INTERNAL}_VERSION VERSION_LESS ${_Eigen3_min_version})
    message(
      FATAL_ERROR
      "Eigen3 version ${_Eigen3_min_version} or later is required."
    )
  endif()

  # When this module is loaded by an app, load Eigen too.
  # Load ITKInternalEigen3 or Eigen3 depending on ITK_USE_SYSTEM_EIGEN
  set(
    ITKEigen3_EXPORT_CODE_INSTALL
    "
    find_package(\"Eigen3\" REQUIRED CONFIG)
    if(Eigen3_VERSION VERSION_LESS ${_Eigen3_min_version})
      message(FATAL_ERROR \"Eigen3 version ${_Eigen3_min_version} or later is required.\")
    endif()
"
  )
  set(
    ITKEigen3_EXPORT_CODE_BUILD
    "
    if(NOT ITK_BINARY_DIR)
      find_package(\"Eigen3\" REQUIRED CONFIG)
      if(Eigen3_VERSION VERSION_LESS ${_Eigen3_min_version})
        message(FATAL_ERROR \"Eigen3 version ${_Eigen3_min_version} or later is required.\")
      endif()
    endif()
"
  )
else()
  set(ITKEigen3_LIBRARIES eigen_internal)
  set(ITKEigen3_INCLUDE_DIRS ${ITKEigen3_SOURCE_DIR}/src)
endif()

# For the generated itk_eigen.h
list(APPEND ITKEigen3_INCLUDE_DIRS ${ITKEigen3_BINARY_DIR}/src)

# Eigen3 targets are not installed if ITK_USE_SYSTEM_EIGEN==True
itk_module_impl()

configure_file(src/itk_eigen.h.in src/itk_eigen.h)
install(
  FILES
    ${ITKEigen3_BINARY_DIR}/src/itk_eigen.h
  DESTINATION ${ITKEigen3_INSTALL_INCLUDE_DIR}
  COMPONENT Development
)

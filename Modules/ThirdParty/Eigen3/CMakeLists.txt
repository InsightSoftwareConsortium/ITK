project(ITKEigen3)
set(ITKEigen3_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_EIGEN "Use an outside build of Eigen3." ${ITK_USE_SYSTEM_LIBRARIES})
mark_as_advanced(ITK_USE_SYSTEM_EIGEN)

if(ITK_USE_SYSTEM_EIGEN)
  set(_eigen_itk_target Eigen3::Eigen)
else()
  set(_eigen_itk_target ITKInternalEigen3::Eigen)
endif()
set(ITKEigen3_LIBRARIES ${_eigen_itk_target})
# Assume that Eigen generates a Eigen3Config.cmake
set(_Eigen3_min_version 3.3)

# Docs:
# find_package(ITK REQUIRED COMPONENTS
# ITKEigen3
# )
# When this module is required, the following cmake is invoked:
# find_package(Eigen3 CONFIG) if ITK_USE_SYSTEM_EIGEN=ON
# Or
# find_package(ITKInternalEigen3 CONFIG) otherwise
#
# ITKInternalEigen3 does NOT allow the use of #include <Eigen/Core>, it requires #include <itkeigen/Eigen/Core>
# This logic is handled automatically by the macro used internally `#include ITK_EIGEN(Core)`

# However, this does not scale well for external ITK modules that want
# to link to third party libraries which are already using Eigen3.
# To solve it, ITK also creates a Eigen3Config.cmake for external consumers (i.e. in ExternalModules).
# This Eigen3Config in ITK/Modules points to the same internal headers than ITKInternalEigen,
# but #include <Eigen/Core> can now be used.
#
# External consumers just need the following extra logic to reuse the internal ITK Eigen, but
# without any change on the include of Eigen3 headers:
#
# Minimal Example:
# CMakeLists.txt
#
# find_package(ITK REQUIRED COMPONENTS
#   ITKEigen3
#   ITKCommon
# )
# set(ITK_EIGEN_LIBRARIES "") # No extra libraries are required if ITK is using system Eigen3
# if(DEFINED ITKInternalEigen3_DIR) # Equivalent to if(NOT ITK_USE_SYSTEM_EIGEN)
#   set(Eigen3_DIR ${ITKInternalEigen3_DIR})
#   find_package(Eigen3 REQUIRED CONFIG)
#   list(APPEND ITK_EIGEN_LIBRARIES Eigen3::Eigen)
# endif()
# add_executable(script file_with_itk_and_eigen3.cpp)
# target_link_library(script ${ITK_LIBRARIES})
# target_link_library(script ${ITK_EIGEN_LIBRARIES})

if(ITK_USE_SYSTEM_EIGEN)
  set(_Eigen3_SYSTEM_OR_INTERNAL "Eigen3")
  find_package(${_Eigen3_SYSTEM_OR_INTERNAL} ${_Eigen3_min_version} REQUIRED CONFIG)
  set(Eigen3_DIR_INSTALL ${Eigen3_DIR})
  set(Eigen3_DIR_BUILD ${Eigen3_DIR})
else()
  set(_Eigen3_SYSTEM_OR_INTERNAL "ITKInternalEigen3")
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
set(${_Eigen3_SYSTEM_OR_INTERNAL}_DIR \"${Eigen3_DIR_BUILD}\")
if(NOT ITK_BINARY_DIR OR ITK_USE_SYSTEM_EIGEN)
  find_package(${_Eigen3_SYSTEM_OR_INTERNAL} ${_Eigen3_min_version} REQUIRED CONFIG)
endif()
")

# Eigen3 targets are not installed if ITK_USE_SYSTEM_EIGEN==True
itk_module_impl()

configure_file(src/itk_eigen.h.in src/itk_eigen.h)
install(FILES ${ITKEigen3_BINARY_DIR}/src/itk_eigen.h
  DESTINATION ${ITKEigen3_INSTALL_INCLUDE_DIR}
  COMPONENT Development
  )

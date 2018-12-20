set(ITK_VERSION_MAJOR 5)
set(ITK3P_INSTALL_EXPORT_NAME "${ITKNIFTI-targets}")
set(ITK3P_INSTALL_INCLUDE_DIR "${ITKNIFTI_INSTALL_INCLUDE_DIR}")
set(ITK3P_INSTALL_RUNTIME_DIR "${ITKNIFTI_INSTALL_RUNTIME_DIR}")
set(ITK3P_INSTALL_LIBRARY_DIR "${ITKNIFTI_INSTALL_LIBRARY_DIR}")
set(ITK3P_INSTALL_ARCHIVE_DIR "${ITKNIFTI_INSTALL_ARCHIVE_DIR}")

set( USE_NIFTI2_CODE OFF CACHE BOOL "Do not build NIFTI2 provided by NIFTI" FORCE )
set( USE_FSL_CODE OFF CACHE BOOL "Do not build FSL provided by NIFTI" FORCE )
set( USE_CIFTI_CODE OFF  CACHE BOOL "Do not build CIFTI provided by NIFTI" FORCE )
set( USE_NIFTICDF_CODE OFF CACHE BOOL "Do not build NIFTICDF provided by NIFTI" FORCE )
set( NIFTI_USE_PACKAGING OFF CACHE BOOL "Do not use packaging from nifti when included as part of another package" FORCE )
set( NIFTI_BUILD_APPLICATIONS OFF CACHE BOOL "Do not build applications provided by NIFTI" FORCE )
# Testing is not compatible with ITK at the moment
set( NIFTI_BUILD_TESTING OFF CACHE BOOL "Do not build nifit testing applications" FORCE)

add_definitions(-DITKZLIB)
set(NIFTI_INSTALL_EXPORT_NAME ${ITKNIFTI-targets} CACHE STRING "Set the nifti package export target name " FORCE)
set(NIFTI_PACKAGE_PREFIX "ITK" CACHE STRING "Set the nifti package prefix" FORCE)
set(NIFTI_LIBRARY_PROPERTIES ${ITK_LIBRARY_PROPERTIES} CACHE STRING "Set the nifti library properties to match ITK" FORCE)
set(NIFTI_INSTALL_RUNTIME_DIR "${ITKNIFTI_INSTALL_RUNTIME_DIR}" CACHE PATH "Set nifti install binaries directory" FORCE)
set(NIFTI_INSTALL_LIBRARY_DIR "${ITKNIFTI_INSTALL_LIBRARY_DIR}" CACHE PATH "Set nifti install dynamic libraries directory" FORCE)
set(NIFTI_INSTALL_ARCHIVE_DIR "${ITKNIFTI_INSTALL_ARCHIVE_DIR}" CACHE PATH "Set nifti install dynamic libraries directory" FORCE)

set(NIFTI_INSTALL_INCLUDE_DIR "${ITK_INSTALL_INCLUDE_DIR}/Utilities")

set(NIFTI_ZLIB_LIBRARIES ${ITKZLIB_LIBRARIES} CACHE STRING "Set the zlib library associated with ITK" FORCE)
set(NIFTI_INSTALL_NO_DEVELOPMENT ${ITK_INSTALL_NO_DEVELOPMENT} CACHE BOOL "nifti development install preference" FORCE)
set(NIFTI_INSTALL_NO_LIBRARIES ${ITK_INSTALL_NO_LIBRARIES} CACHE BOOL "nifti libraries install preference" FORCE)

add_subdirectory(nifti)
itk_module_target(ITKznz NO_INSTALL)
itk_module_target(ITKniftiio NO_INSTALL)

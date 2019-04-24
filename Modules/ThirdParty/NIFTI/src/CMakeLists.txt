set(ITK_VERSION_MAJOR 5)
set(ITK3P_INSTALL_EXPORT_NAME "${ITKNIFTI-targets}")
set(ITK3P_INSTALL_INCLUDE_DIR "${ITKNIFTI_INSTALL_INCLUDE_DIR}")
set(ITK3P_INSTALL_RUNTIME_DIR "${ITKNIFTI_INSTALL_RUNTIME_DIR}")
set(ITK3P_INSTALL_LIBRARY_DIR "${ITKNIFTI_INSTALL_LIBRARY_DIR}")
set(ITK3P_INSTALL_ARCHIVE_DIR "${ITKNIFTI_INSTALL_ARCHIVE_DIR}")

set(USE_NIFTI2_CODE OFF CACHE BOOL "Do not build NIFTI2 provided by NIFTI" FORCE)
set(USE_FSL_CODE OFF CACHE BOOL "Do not build FSL provided by NIFTI" FORCE)
set(USE_CIFTI_CODE OFF  CACHE BOOL "Do not build CIFTI provided by NIFTI" FORCE)
set(USE_NIFTICDF_CODE OFF CACHE BOOL "Do not build NIFTICDF provided by NIFTI" FORCE)
set(NIFTI_USE_PACKAGING OFF CACHE BOOL "Do not use packaging from nifti when included as part of another package" FORCE)
set(NIFTI_BUILD_APPLICATIONS OFF CACHE BOOL "Do not build applications provided by NIFTI" FORCE)
# Testing is not compatible with ITK at the moment
set(NIFTI_BUILD_TESTING OFF) # "Do not build nifit testing applications"

add_definitions(-DITKZLIB)
set(NIFTI_INSTALL_EXPORT_NAME ${ITKNIFTI-targets}) # "Set the nifti package export target name"
set(NIFTI_PACKAGE_PREFIX "ITK") # "Set the nifti package prefix"
set(NIFTI_LIBRARY_PROPERTIES ${ITK_LIBRARY_PROPERTIES}) # "Set the nifti library properties to match ITK"
set(NIFTI_INSTALL_RUNTIME_DIR "${ITKNIFTI_INSTALL_RUNTIME_DIR}") # "Set nifti install binaries directory"
set(NIFTI_INSTALL_LIBRARY_DIR "${ITKNIFTI_INSTALL_LIBRARY_DIR}") # "Set nifti install dynamic libraries directory"
set(NIFTI_INSTALL_ARCHIVE_DIR "${ITKNIFTI_INSTALL_ARCHIVE_DIR}") # "Set nifti install dynamic libraries directory"

set(NIFTI_INSTALL_INCLUDE_DIR "${ITK_INSTALL_INCLUDE_DIR}/Utilities")

set(NIFTI_ZLIB_LIBRARIES ${ITKZLIB_LIBRARIES}) # "Set the zlib library associated with ITK"
set(NIFTI_INSTALL_NO_DEVELOPMENT ${ITK_INSTALL_NO_DEVELOPMENT}) # "nifti development install preference"
set(NIFTI_INSTALL_NO_LIBRARIES ${ITK_INSTALL_NO_LIBRARIES}) # "nifti libraries install preference"

add_subdirectory(nifti)
itk_module_target(ITKznz NO_INSTALL)
itk_module_target(ITKniftiio NO_INSTALL)

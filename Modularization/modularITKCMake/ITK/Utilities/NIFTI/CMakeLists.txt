cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-NIFTI)
find_package(itk-base 4.0 EXACT REQUIRED)

option(ITK_USE_SYSTEM_NIFTI "Use an outside build of NIFTI." OFF)
mark_as_advanced(ITK_USE_SYSTEM_NIFTI)

if(ITK_USE_SYSTEM_NIFTI)
  find_package(NIFTI REQUIRED)
  set(ITK-NIFTI_INCLUDE_DIRS
    ${NIFTI_INCLUDE_DIRS}
    )
  set(ITK-NIFTI_LIBRARIES ITKznz ITKniftiio)
  set(itk-module-config-install "
list(APPEND ITK-NIFTI_INCLUDE_DIRS \"${ITK-NIFTI_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-NIFTI_INCLUDE_DIRS \"${ITK-NIFTI_INCLUDE_DIRS}\")
")
  set(itk-module-no-include 1)
  set(itk-module-no-src 1)
else()
  set(ITK-NIFTI-target ITKniftiio)
  set(ITK-NIFTI_INCLUDE_DIRS
    ${ITK-NIFTI_BINARY_DIR}
    ${ITK-NIFTI_SOURCE_DIR}/src/nifti/niftilib
    ${ITK-NIFTI_SOURCE_DIR}/src/nifti/znzlib
    )
  set(ITK-NIFTI_LIBRARIES ITKznz ITKniftiio)
endif()

itk_module_impl()

if (NOT ITK_USE_SYSTEM_NIFTI)
  itk_module_target_name(ITKznz)
  itk_module_target_export(ITKznz)
  itk_module_target_name(ITKniftiio)
  itk_module_target_export(ITKniftiio)
endif()

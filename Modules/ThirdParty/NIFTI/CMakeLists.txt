project(ITK-NIFTI)
set(ITK-NIFTI_THIRD_PARTY 1)

set(ITK-NIFTI_INCLUDE_DIRS
  ${ITK-NIFTI_SOURCE_DIR}/src/nifti/niftilib
  ${ITK-NIFTI_SOURCE_DIR}/src/nifti/znzlib
  )
set(ITK-NIFTI_LIBRARIES ITKznz ITKniftiio)

itk_module_impl()

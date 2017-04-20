project(ITKNIFTI)
set(ITKNIFTI_THIRD_PARTY 1)

set(ITKNIFTI_INCLUDE_DIRS
  ${ITKNIFTI_SOURCE_DIR}/src/nifti/niftilib
  ${ITKNIFTI_SOURCE_DIR}/src/nifti/znzlib
  )
set(ITKNIFTI_LIBRARIES ITKznz ITKniftiio)

itk_module_impl()

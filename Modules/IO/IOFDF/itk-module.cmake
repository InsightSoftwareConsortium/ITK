set(
  DOCUMENTATION
  "This modules contains an ImageIO class to read or write the
FDF image format."
)

itk_module(
  IOFDF
  ENABLE_SHARED
  DEPENDS
    ITKNIFTI
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKTransform
  DESCRIPTION "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
)

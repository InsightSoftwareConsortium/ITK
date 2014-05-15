set(
  DOCUMENTATION
  "This modules contains an ImageIO class to read or write the
FDF image format."
)

itk_module(
  ITKIOFDF
  DEPENDS
    ITKNIFTI
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKTransform
  DESCRIPTION "${DOCUMENTATION}"
)

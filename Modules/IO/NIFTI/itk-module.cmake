set(DOCUMENTATION "This modules contains an ImageIO class to read or write the
<a href=\"http://niftilib.sourceforge.net/\">nifti</a> medical image format.")

itk_module(ITKIONIFTI
  ENABLE_SHARED
  DEPENDS
    ITKNIFTI
  PRIVATE_DEPENDS
    ITKIOImageBase
    ITKTransform
  TEST_DEPENDS
    ITKTestKernel
    ITKNIFTI
    ITKTransform
  DESCRIPTION
    "${DOCUMENTATION}"
)

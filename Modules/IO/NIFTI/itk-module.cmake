set(DOCUMENTATION "This modules contains an ImageIO class to read or write the
<a href=\"http://niftilib.sourceforge.net/\">nifti</a> medical image format.")

itk_module(ITKIONIFTI
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKIOImageBase
    ITKTransform
    ITKNIFTI
  TEST_DEPENDS
    ITKTestKernel
    ITKNIFTI
    ITKTransform
  FACTORY_NAMES
    ImageIO::Nifti
  DESCRIPTION
    "${DOCUMENTATION}"
)

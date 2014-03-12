set(DOCUMENTATION "This modules contains an ImageIO class to read or write the
  AnalyzeObjectMap file format.")

itk_module(AnalyzeObjectMapIO
  DEPENDS
    ITKThresholding
    ITKIOImageBase
    ITKZLIB
  TEST_DEPENDS
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION
    "${DOCUMENTATION}"
)

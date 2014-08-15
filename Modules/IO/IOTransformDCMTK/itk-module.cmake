set(
  DOCUMENTATION
  "This modules contains classes for the reading of transforms
stored in DICOM files."
)

itk_module(
  IOTransformDCMTK
  DEPENDS
    ITKIOTransformBase
    ITKDCMTK
  TEST_DEPENDS
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)

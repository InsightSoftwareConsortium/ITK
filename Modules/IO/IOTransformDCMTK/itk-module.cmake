set(
  DOCUMENTATION
  "This modules contains classes for the reading of transforms
stored in DICOM files."
)

itk_module(
  IOTransformDCMTK
  ENABLE_SHARED
  DEPENDS
    ITKIOTransformBase
    ITKDCMTK
  TEST_DEPENDS
    ITKTestKernel
    ITKIODCMTK
    ITKIOGDCM
    ITKImageGrid
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)

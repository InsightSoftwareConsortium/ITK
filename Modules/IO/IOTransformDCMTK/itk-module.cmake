# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in IOTransformDCMTK
# The testing module in IOTransformDCMTK depends on ITKTestKernel
# By convention those modules outside of ITK are not prefixed with
# ITK

# define the dependencies of the include module and the tests
itk_module(
  IOTransformDCMTK
  ENABLE_SHARED
  DEPENDS
    ITKIOTransformBase
    ITKDCMTK
  TEST_DEPENDS
    ITKIOTransformBase
    ITKTestKernel
    ITKIODCMTK
    ITKIOGDCM
    ITKImageGrid
  FACTORY_NAMES
    TransformIO::DCMTK
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "Module ingested from upstream."
)

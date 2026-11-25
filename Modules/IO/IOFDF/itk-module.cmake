itk_module(
  IOFDF
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKTransform
  FACTORY_NAMES
    ImageIO::FDF
  DESCRIPTION "FDF image format ImageIO plugin for ITK."
  EXCLUDE_FROM_DEFAULT
)

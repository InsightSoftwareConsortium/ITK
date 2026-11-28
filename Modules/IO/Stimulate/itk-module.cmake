set(
  DOCUMENTATION
  "This module contains a class for reading
Stimulate (SDT/SPR) images. The format is described in this module's README.md."
)

itk_module(
  ITKIOStimulate
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKGoogleTest
  FACTORY_NAMES
    ImageIO::Stimulate
  DESCRIPTION "${DOCUMENTATION}"
)

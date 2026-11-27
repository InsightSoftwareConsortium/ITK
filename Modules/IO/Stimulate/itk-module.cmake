set(
  DOCUMENTATION
  "This model contains a class for reading
<a href=\"https://downloads.cmrr.umn.edu/software\">
Stimulate (SDT/SPR)</a> images."
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

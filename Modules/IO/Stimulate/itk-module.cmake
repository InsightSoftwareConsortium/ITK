set(DOCUMENTATION "This model contains a class for reading
<a href=\"http://www.cmrr.umn.edu/stimulate/stimUsersGuide/node57.html\">
Stimulate (SDT/SPR)</a> images.")

itk_module(ITKIOStimulate
  DEPENDS
    ITKIOBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)

set(DOCUMENTATION "This module contains ImageIO classes for reading <a
href=\"http://www.bio-rad.com/\">Bio-Rad images.</a> The Bio-Rad file format is
used by confocal microscopes like MRC 1024 and MRC 600.")

itk_module(ITKIOBioRad
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)

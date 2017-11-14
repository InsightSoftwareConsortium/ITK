set(DOCUMENTATION "This module contains classes that read Bruker image files.")

itk_module(ITKIOBruker
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
  FACTORY_NAMES
    ImageIO::Bruker2dseq
  DESCRIPTION
    "${DOCUMENTATION}")

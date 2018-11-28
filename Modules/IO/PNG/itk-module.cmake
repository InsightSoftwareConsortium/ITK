set(DOCUMENTATION "This module contains an ImageIO class for reading and writing
files in the Portable Network Graphics (PNG) format.")

itk_module(ITKIOPNG
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  PRIVATE_DEPENDS
    ITKPNG
  TEST_DEPENDS
    ITKTestKernel
  FACTORY_NAMES
    ImageIO::PNG
  DESCRIPTION
    "${DOCUMENTATION}"
)

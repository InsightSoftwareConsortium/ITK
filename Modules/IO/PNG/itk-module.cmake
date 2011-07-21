set(DOCUMENTATION "This module contains an ImageIO class for reading and writing
files in the Portable Network Graphics (PNG) format.")

itk_module(ITKIOPNG
  DEPENDS
    ITKPNG
    ITKIOBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)

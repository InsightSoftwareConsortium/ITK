set(DOCUMENTATION "This module contains classes for reading and writing image
files in the Tagged Image File Format (TIFF).")

itk_module(ITKIOTIFF
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  PRIVATE_DEPENDS
    ITKTIFF
  TEST_DEPENDS
    ITKTestKernel
  FACTORY_NAMES
    ImageIO::TIFF
  DESCRIPTION
    "${DOCUMENTATION}"
)

set(DOCUMENTATION "This module contains classes for reading and writing image
files in the Tagged Image File Format (TIFF).")

itk_module(ITKIOTIFF
  DEPENDS
    ITKTIFF
    ITKIOBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)

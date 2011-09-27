set(DOCUMENTATION "This module contains ImageIO classes for reading the
Microsoft Bitmap File Format (BMP).")

itk_module(ITKIOBMP
  DEPENDS
    ITKIOBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)

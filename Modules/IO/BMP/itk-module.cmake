set(DOCUMENTATION "This module contains ImageIO classes for reading the
Microsoft Bitmap File Format (BMP).")

itk_module(ITKIOBMP
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
  FACTORY_NAMES
    ImageIO::BMP
  DESCRIPTION
    "${DOCUMENTATION}"
)

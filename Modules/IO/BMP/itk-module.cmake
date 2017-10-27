set(DOCUMENTATION "This module contains ImageIO classes for reading the
Microsoft Bitmap File Format (BMP).")

itk_module(ITKIOBMP
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
  FACTORY_NAMES
    ImageIO::BMP
  DESCRIPTION
    "${DOCUMENTATION}"
)

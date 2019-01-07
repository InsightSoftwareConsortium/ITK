set(DOCUMENTATION "This module contains ImageIO classes for reading the Gipl
(Guys Image Processing Lab) image format.")

itk_module(ITKIOGIPL
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  PRIVATE_DEPENDS
    ITKZLIB
  TEST_DEPENDS
    ITKTestKernel
  FACTORY_NAMES
    ImageIO::Gipl
  DESCRIPTION
    "${DOCUMENTATION}"
)

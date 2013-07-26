set(DOCUMENTATION "This module contains ImageIO classes for reading the Gipl
(Guys Image Processing Lab) image format.")

itk_module(ITKIOGIPL
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
    ITKZLIB
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)

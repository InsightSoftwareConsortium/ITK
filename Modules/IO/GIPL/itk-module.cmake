set(DOCUMENTATION "This module contains ImageIO classes for reading the Gipl
(Guys Image Processing Lab) image format.")

itk_module(ITKIOGIPL
  DEPENDS
    ITKIOBase
    ITKZLIB
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)

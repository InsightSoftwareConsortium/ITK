set(DOCUMENTATION "This module contains an ImageIO class for reading and
writing files in the JPEG format.")

itk_module(ITKIOJPEG
  DEPENDS
    ITKJPEG
    ITKIOBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)

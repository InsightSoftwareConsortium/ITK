set(DOCUMENTATION "This module contains an ImageIO class for reading and
writing files in the JPEG format.")

itk_module(ITKIOJPEG
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKIOImageBase
    ITKJPEG
  TEST_DEPENDS
    ITKTestKernel
  FACTORY_NAMES
    ImageIO::JPEG
  DESCRIPTION
    "${DOCUMENTATION}"
)

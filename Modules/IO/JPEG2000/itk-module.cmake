set(DOCUMENTATION "This module contains an ImageIO class for reading and
writing files in the JPEG2000 format.")

itk_module(ITKIOJPEG2000
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  PRIVATE_DEPENDS
    ITKOpenJPEG
  TEST_DEPENDS
    ITKTestKernel
    ITKImageGrid
  FACTORY_NAMES
    ImageIO::JPEG2000
  DESCRIPTION
    "${DOCUMENTATION}"
)

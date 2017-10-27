set(DOCUMENTATION "This module contains classes for reading and writing image
files in the MRC file format. https://en.wikipedia.org/wiki/MRC_(file_format) ")

itk_module(ITKIOMRC
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKImageSources
  FACTORY_NAMES
    ImageIO::MRC
  DESCRIPTION
    "${DOCUMENTATION}"
)

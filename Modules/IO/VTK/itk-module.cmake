set(DOCUMENTATION "This module contains classes for reading and writing image
files in the \"legacy\" (non-XML) VTK file format.")

itk_module(ITKIOVTK
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKImageSources
  FACTORY_NAMES
    ImageIO::VTK
  DESCRIPTION
    "${DOCUMENTATION}"
)

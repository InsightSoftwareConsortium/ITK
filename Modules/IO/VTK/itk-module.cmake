set(
  DOCUMENTATION
  "This module contains classes for reading and writing image
files in the \"legacy\" (non-XML) VTK file format and the VTK XML
ImageData (.vti) file format."
)

itk_module(
  ITKIOVTK
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  PRIVATE_DEPENDS
    ITKExpat
  TEST_DEPENDS
    ITKTestKernel
    ITKImageSources
  FACTORY_NAMES
    ImageIO::VTK
    ImageIO::VTI
  DESCRIPTION "${DOCUMENTATION}"
)

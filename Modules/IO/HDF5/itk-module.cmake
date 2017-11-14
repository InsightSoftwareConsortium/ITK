set(DOCUMENTATION "This module contains an ImageIO class for reading and writing
ITK Images stored in the <a href=\"http://www.hdfgroup.org/HDF5/\">HDF5</a>
data model and file format.")

itk_module(ITKIOHDF5
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKIOImageBase
    ITKHDF5
  TEST_DEPENDS
    ITKTestKernel
    ITKImageSources
  FACTORY_NAMES
    ImageIO::HDF5
  DESCRIPTION
    "${DOCUMENTATION}"
)

set(DOCUMENTATION "This module contains an ImageIO class for reading and writing
ITK Images stored in the <a href=\"http://www.hdfgroup.org/HDF5/\">HDF5</a>
data model and file format.")

itk_module(ITKIOHDF5
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
    ITKHDF5
  COMPILE_DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
    ITKImageSources
  DESCRIPTION
    "${DOCUMENTATION}"
)

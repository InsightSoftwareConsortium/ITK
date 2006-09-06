BEGIN_WRAPPER_LIBRARY(Base)

SET(WRAPPER_LIBRARY_GROUPS
    itkOffset
    itkArray
    itkContinuousIndex
    itkCovariantVector
    itkVector
    itkPoint
    itkRGBPixel
    itkVariableLengthVector
    itkImage
    itkVectorImage
    itkLevelSet
    itkDefaultStaticMeshTraits
    ITKCommonBase
    ITKRegions
    SwigExtras)

SET(WRAPPER_LIBRARY_DEPENDS VXLNumerics)

SET(WRAPPER_LIBRARY_SWIG_INPUTS "${CMAKE_CURRENT_SOURCE_DIR}/SwigExtras.i") 
SET(WRAPPER_LIBRARY_CXX_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/itkStringStream.cxx") 

SET(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon ITKNumerics)

WRAPPER_LIBRARY_CREATE_WRAP_FILES()

WRAPPER_LIBRARY_CREATE_LIBRARY()




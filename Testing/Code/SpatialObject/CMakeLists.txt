LINK_LIBRARIES(
  ITKCommon
  ITKSpatialObject
  VXLNumerics
)

IF (TCL_TCLSH)
ADD_TEST(PrintSelf-SpatialObject ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
         ${ITK_SOURCE_DIR}/Code/SpatialObject)
ENDIF (TCL_TCLSH)

INCLUDE_DIRECTORIES(
  ${ITK_SOURCE_DIR}/Code/SpatialObject
  ${ITK_SOURCE_DIR}/Code/Common
  ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vnl
)

ADD_TEST( itkTubeSpatialObjectTest itkSpatialObjectTests itkTubeSpatialObjectTest )
ADD_TEST( itkImageSpatialObjectTest itkSpatialObjectTests itkImageSpatialObjectTest )

SOURCE_FILES(
  SpatialObject_SRCS
  itkTubeSpatialObjectTest
  itkImageSpatialObjectTest
  itkSpatialObjectTests
)

ADD_EXECUTABLE( itkSpatialObjectTests itkSpatialObjectTests.cxx SpatialObject_SRCS)

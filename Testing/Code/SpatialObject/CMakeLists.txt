INCLUDE_DIRECTORIES(
  ${ITK_SOURCE_DIR}/Code/SpatialObject
  ${ITK_SOURCE_DIR}/Code/Common
  ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vnl
)

LINK_LIBRARIES(
  ITKCommon
#  ITKSpatialObject
  VXLNumerics
)

IF (TCL_TCLSH)
ADD_TEST(PrintSelf-SpatialObject ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
         ${ITK_SOURCE_DIR}/Code/SpatialObject)
ENDIF (TCL_TCLSH)

ADD_TEST( itkTubeSpatialObjectTest itkSpatialObjectTests itkTubeSpatialObjectTest )
ADD_TEST( itkImageSpatialObjectTest itkSpatialObjectTests itkImageSpatialObjectTest )
ADD_TEST( itkEllipseSpatialObjectTest itkSpatialObjectTests itkEllipseSpatialObjectTest)
ADD_TEST( itkBlobSpatialObjectTest itkSpatialObjectTests itkBlobSpatialObjectTest )
ADD_TEST( itkSurfaceSpatialObjectTest itkSpatialObjectTests itkSurfaceSpatialObjectTest )
ADD_TEST( itkLineSpatialObjectTest itkSpatialObjectTests itkLineSpatialObjectTest )

ADD_TEST(itkSpatialObjectHeaderTest itkSpatialObjectHeaderTest)

SOURCE_FILES(
  SpatialObject_SRCS
  itkTubeSpatialObjectTest
  itkImageSpatialObjectTest
  itkEllipseSpatialObjectTest
  itkBlobSpatialObjectTest
  itkSurfaceSpatialObjectTest  
  itkLineSpatialObjectTest
)

ADD_EXECUTABLE( itkSpatialObjectTests itkSpatialObjectTests.cxx SpatialObject_SRCS)
ADD_EXECUTABLE(itkSpatialObjectHeaderTest itkSpatialObjectHeaderTest.cxx)

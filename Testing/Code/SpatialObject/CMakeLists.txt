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

SET(SpatialObject_SRCS
  itkTubeSpatialObjectTest.cxx
  itkImageSpatialObjectTest.cxx
  itkEllipseSpatialObjectTest.cxx
  itkBlobSpatialObjectTest.cxx
  itkSurfaceSpatialObjectTest.cxx
  itkLineSpatialObjectTest.cxx
)

ADD_EXECUTABLE(itkSpatialObjectTests itkSpatialObjectTests.cxx ${SpatialObject_SRCS})
ADD_EXECUTABLE(itkSpatialObjectHeaderTest itkSpatialObjectHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkSpatialObjectHeaderTest ITKCommon)
TARGET_LINK_LIBRARIES(itkSpatialObjectTests ITKCommon)


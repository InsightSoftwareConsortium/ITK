IF (TCL_TCLSH)
ADD_TEST(PrintSelf-SpatialObject ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
         ${ITK_SOURCE_DIR}/Code/SpatialObject)
ENDIF (TCL_TCLSH)

IF( CXX_TEST_PATH )
  SET(SPATIAL_OBJECT_TESTS        ${CXX_TEST_PATH}/itkSpatialObjectTests)
  SET(SPATIAL_OBJECT_HEADER_TEST  ${CXX_TEST_PATH}/itkSpatialObjectHeaderTest)
ELSE( CXX_TEST_PATH )
  SET(SPATIAL_OBJECT_TESTS        itkSpatialObjectTests)
  SET(SPATIAL_OBJECT_HEADER_TEST  itkSpatialObjectHeaderTest)
ENDIF(CXX_TEST_PATH )

ADD_TEST( itkTubeSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkTubeSpatialObjectTest )
ADD_TEST( itkImageSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkImageSpatialObjectTest )
ADD_TEST( itkEllipseSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkEllipseSpatialObjectTest)
ADD_TEST( itkBlobSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkBlobSpatialObjectTest )
ADD_TEST( itkLandmarkSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkLandmarkSpatialObjectTest )
ADD_TEST( itkSurfaceSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkSurfaceSpatialObjectTest )
ADD_TEST( itkLineSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkLineSpatialObjectTest )

ADD_TEST(itkSpatialObjectHeaderTest ${SPATIAL_OBJECT_HEADER_TEST})

SET(SpatialObject_SRCS
  itkTubeSpatialObjectTest.cxx
  itkImageSpatialObjectTest.cxx
  itkEllipseSpatialObjectTest.cxx
  itkBlobSpatialObjectTest.cxx
  itkLandmarkSpatialObjectTest.cxx
  itkSurfaceSpatialObjectTest.cxx
  itkLineSpatialObjectTest.cxx
)

ADD_EXECUTABLE(itkSpatialObjectTests itkSpatialObjectTests.cxx ${SpatialObject_SRCS})
ADD_EXECUTABLE(itkSpatialObjectHeaderTest itkSpatialObjectHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkSpatialObjectHeaderTest ITKCommon)
TARGET_LINK_LIBRARIES(itkSpatialObjectTests ITKCommon)


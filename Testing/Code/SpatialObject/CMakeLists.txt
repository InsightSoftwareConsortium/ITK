IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-SpatialObject ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/SpatialObject)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

IF( NOT ITK_DISABLE_CXX_TESTING )

SET(SPATIAL_OBJECT_TESTS        ${CXX_TEST_PATH}/itkSpatialObjectTests)
SET(SPATIAL_OBJECT_HEADER_TEST  ${CXX_TEST_PATH}/itkSpatialObjectHeaderTest)

ADD_TEST(itkSpatialObjectPrintTest ${SPATIAL_OBJECT_TESTS} itkSpatialObjectPrintTest)
ADD_TEST( itkAffineGeometryFrameTest ${SPATIAL_OBJECT_TESTS} itkAffineGeometryFrameTest )
ADD_TEST( itkBlobSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkBlobSpatialObjectTest )
ADD_TEST( itkEllipseSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkEllipseSpatialObjectTest)
ADD_TEST( itkGaussianSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkGaussianSpatialObjectTest)
ADD_TEST( itkImageMaskSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkImageMaskSpatialObjectTest )
ADD_TEST( itkImageSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkImageSpatialObjectTest )
ADD_TEST( itkLandmarkSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkLandmarkSpatialObjectTest )
ADD_TEST( itkLineSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkLineSpatialObjectTest )
ADD_TEST( itkMeshSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkMeshSpatialObjectTest )
ADD_TEST( itkPlaneSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkPlaneSpatialObjectTest )
ADD_TEST( itkPolygonGroupSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkPolygonGroupSpatialObjectTest )
ADD_TEST( itkSceneSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkSceneSpatialObjectTest )
ADD_TEST( itkSpatialObjectHeaderTest ${SPATIAL_OBJECT_HEADER_TEST})
ADD_TEST( itkSpatialObjectTreeContainerTest ${SPATIAL_OBJECT_TESTS} itkSpatialObjectTreeContainerTest )
ADD_TEST( itkSurfaceSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkSurfaceSpatialObjectTest )
ADD_TEST( itkTubeSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkTubeSpatialObjectTest )
ADD_TEST( itkArrowSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkArrowSpatialObjectTest )

SET(SpatialObject_SRCS
  itkArrowSpatialObjectTest.cxx
  itkSpatialObjectPrintTest.cxx
  itkTubeSpatialObjectTest.cxx
  itkImageSpatialObjectTest.cxx
  itkImageMaskSpatialObjectTest.cxx
  itkEllipseSpatialObjectTest.cxx
  itkGaussianSpatialObjectTest.cxx
  itkBlobSpatialObjectTest.cxx
  itkLandmarkSpatialObjectTest.cxx
  itkSurfaceSpatialObjectTest.cxx
  itkLineSpatialObjectTest.cxx
  itkMeshSpatialObjectTest.cxx
  itkPolygonGroupSpatialObjectTest.cxx
  itkPlaneSpatialObjectTest.cxx
  itkSceneSpatialObjectTest.cxx
  itkAffineGeometryFrameTest.cxx
  itkSpatialObjectTreeContainerTest.cxx
)

ADD_EXECUTABLE(itkSpatialObjectTests itkSpatialObjectTests.cxx ${SpatialObject_SRCS})
ADD_EXECUTABLE(itkSpatialObjectHeaderTest itkSpatialObjectHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkSpatialObjectHeaderTest ITKCommon)
TARGET_LINK_LIBRARIES(itkSpatialObjectTests ITKIO ITKCommon)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )


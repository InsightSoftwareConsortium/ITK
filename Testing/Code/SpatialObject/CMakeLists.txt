IF( NOT ITK_DISABLE_CXX_TESTING )
IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-SpatialObject ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/SpatialObject)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)


SET(SPATIAL_OBJECT_TESTS        ${CXX_TEST_PATH}/itkSpatialObjectTests)
SET(SPATIAL_OBJECT_TESTS2       ${CXX_TEST_PATH}/itkSpatialObjectTests2)
SET(SPATIAL_OBJECT_TESTS3       ${CXX_TEST_PATH}/itkSpatialObjectTests3)
SET(SPATIAL_OBJECT_HEADER_TEST  ${CXX_TEST_PATH}/itkSpatialObjectHeaderTest)

ADD_TEST( itkAffineGeometryFrameTest ${SPATIAL_OBJECT_TESTS} itkAffineGeometryFrameTest )
ADD_TEST( itkArrowSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkArrowSpatialObjectTest )

ADD_TEST( itkBoxSpatialObjectTest ${SPATIAL_OBJECT_TESTS3} 
  --compare ${ITK_DATA_ROOT}/Baseline/SpatialObjects/BoxSpatialObjectTest.png
            ${ITK_TEST_OUTPUT_DIR}/BoxSpatialObjectTest.png
  itkBoxSpatialObjectTest 
            ${ITK_TEST_OUTPUT_DIR}/BoxSpatialObjectTest.png)

ADD_TEST( itkBlobSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkBlobSpatialObjectTest )
ADD_TEST( itkCylinderSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkCylinderSpatialObjectTest )
ADD_TEST( itkEllipseSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkEllipseSpatialObjectTest)
ADD_TEST( itkGaussianSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkGaussianSpatialObjectTest)
ADD_TEST( itkImageMaskSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkImageMaskSpatialObjectTest )
ADD_TEST( itkImageSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkImageSpatialObjectTest )
ADD_TEST( itkLandmarkSpatialObjectTest ${SPATIAL_OBJECT_TESTS3} itkLandmarkSpatialObjectTest )
ADD_TEST( itkLineSpatialObjectTest ${SPATIAL_OBJECT_TESTS3} itkLineSpatialObjectTest )

ADD_TEST( itkMeshSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkMeshSpatialObjectTest )
ADD_TEST( itkPlaneSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkPlaneSpatialObjectTest )
ADD_TEST( itkPolygonGroupSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkPolygonGroupSpatialObjectTest )
ADD_TEST( itkSceneSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkSceneSpatialObjectTest )
ADD_TEST( itkSpatialObjectPrintTest ${SPATIAL_OBJECT_TESTS3} itkSpatialObjectPrintTest)
ADD_TEST( itkSpatialObjectTreeContainerTest ${SPATIAL_OBJECT_TESTS2} itkSpatialObjectTreeContainerTest )
ADD_TEST( itkSurfaceSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkSurfaceSpatialObjectTest )
ADD_TEST( itkTubeSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkTubeSpatialObjectTest )

ADD_TEST( itkSpatialObjectHeaderTest ${SPATIAL_OBJECT_HEADER_TEST})

SET(SpatialObject_SRCS
  itkAffineGeometryFrameTest.cxx
  itkArrowSpatialObjectTest.cxx
  itkBlobSpatialObjectTest.cxx
  itkCylinderSpatialObjectTest.cxx
  itkEllipseSpatialObjectTest.cxx
  itkGaussianSpatialObjectTest.cxx
  itkImageMaskSpatialObjectTest.cxx
  itkImageSpatialObjectTest.cxx
)

SET(SpatialObject_SRCS2
  itkMeshSpatialObjectTest.cxx
  itkPlaneSpatialObjectTest.cxx
  itkPolygonGroupSpatialObjectTest.cxx
  itkSceneSpatialObjectTest.cxx
  itkSpatialObjectTreeContainerTest.cxx
  itkSurfaceSpatialObjectTest.cxx
  itkTubeSpatialObjectTest.cxx
)

SET(SpatialObject_SRCS3
  itkBoxSpatialObjectTest.cxx
  itkLandmarkSpatialObjectTest.cxx
  itkLineSpatialObjectTest.cxx
  itkSpatialObjectPrintTest.cxx
)

ADD_EXECUTABLE(itkSpatialObjectTests itkSpatialObjectTests.cxx ${SpatialObject_SRCS})
TARGET_LINK_LIBRARIES(itkSpatialObjectTests ITKIO ITKCommon ITKSpatialObject)

ADD_EXECUTABLE(itkSpatialObjectTests2 itkSpatialObjectTests2.cxx ${SpatialObject_SRCS2})
TARGET_LINK_LIBRARIES(itkSpatialObjectTests2 ITKIO ITKCommon)

ADD_EXECUTABLE(itkSpatialObjectTests3 itkSpatialObjectTests3.cxx ${SpatialObject_SRCS3})
TARGET_LINK_LIBRARIES(itkSpatialObjectTests3 ITKIO ITKCommon)

ADD_EXECUTABLE(itkSpatialObjectHeaderTest itkSpatialObjectHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkSpatialObjectHeaderTest ITKCommon)


ENDIF( NOT ITK_DISABLE_CXX_TESTING )


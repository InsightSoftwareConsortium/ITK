if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-SpatialObject ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/SpatialObject)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)


set(SPATIAL_OBJECT_TESTS        ${CXX_TEST_PATH}/itkSpatialObjectTests)
set(SPATIAL_OBJECT_TESTS2       ${CXX_TEST_PATH}/itkSpatialObjectTests2)
set(SPATIAL_OBJECT_TESTS3       ${CXX_TEST_PATH}/itkSpatialObjectTests3)
set(SPATIAL_OBJECT_TESTS4       ${CXX_TEST_PATH}/itkSpatialObjectTests4)
set(SPATIAL_OBJECT_TESTS5       ${CXX_TEST_PATH}/itkSpatialObjectTests5)
set(SPATIAL_OBJECT_HEADER_TEST  ${CXX_TEST_PATH}/itkSpatialObjectHeaderTest)

add_test( itkAffineGeometryFrameTest ${SPATIAL_OBJECT_TESTS} itkAffineGeometryFrameTest )
add_test( itkArrowSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkArrowSpatialObjectTest )

add_test( itkBoxSpatialObjectTest ${SPATIAL_OBJECT_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/SpatialObjects/BoxSpatialObjectTest.png
            ${ITK_TEST_OUTPUT_DIR}/BoxSpatialObjectTest.png
  itkBoxSpatialObjectTest
            ${ITK_TEST_OUTPUT_DIR}/BoxSpatialObjectTest.png)

add_test( itkBlobSpatialObjectTest ${SPATIAL_OBJECT_TESTS} itkBlobSpatialObjectTest )
add_test( itkCylinderSpatialObjectTest ${SPATIAL_OBJECT_TESTS5} itkCylinderSpatialObjectTest )
add_test( itkEllipseSpatialObjectTest ${SPATIAL_OBJECT_TESTS5} itkEllipseSpatialObjectTest)
add_test( itkGaussianSpatialObjectTest ${SPATIAL_OBJECT_TESTS5} itkGaussianSpatialObjectTest)
add_test( itkImageMaskSpatialObjectTest ${SPATIAL_OBJECT_TESTS4} itkImageMaskSpatialObjectTest )
add_test( itkImageMaskSpatialObjectTest2 ${SPATIAL_OBJECT_TESTS4} itkImageMaskSpatialObjectTest2 )
add_test( itkImageSpatialObjectTest ${SPATIAL_OBJECT_TESTS4} itkImageSpatialObjectTest )

add_test( itkMeshSpatialObjectTest ${SPATIAL_OBJECT_TESTS3} itkMeshSpatialObjectTest )
add_test( itkPlaneSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkPlaneSpatialObjectTest )
add_test( itkPolygonGroupSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkPolygonGroupSpatialObjectTest )
add_test( itkPolygonSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkPolygonSpatialObjectTest )
add_test( itkSceneSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkSceneSpatialObjectTest )
add_test( itkSpatialObjectTreeContainerTest ${SPATIAL_OBJECT_TESTS2} itkSpatialObjectTreeContainerTest )
add_test( itkSurfaceSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkSurfaceSpatialObjectTest )
add_test( itkTubeSpatialObjectTest ${SPATIAL_OBJECT_TESTS2} itkTubeSpatialObjectTest )

add_test( itkLandmarkSpatialObjectTest ${SPATIAL_OBJECT_TESTS3} itkLandmarkSpatialObjectTest )
add_test( itkLineSpatialObjectTest ${SPATIAL_OBJECT_TESTS3} itkLineSpatialObjectTest )
add_test( itkSpatialObjectDuplicatorTest ${SPATIAL_OBJECT_TESTS3} itkSpatialObjectDuplicatorTest )
add_test( itkSpatialObjectPrintTest ${SPATIAL_OBJECT_TESTS3} itkSpatialObjectPrintTest)
add_test( itkSpatialObjectHeaderTest ${SPATIAL_OBJECT_HEADER_TEST})

add_test( itkMetaArrowConverterTest ${SPATIAL_OBJECT_TESTS5} itkMetaArrowConverterTest
  ${ITK_TEST_OUTPUT_DIR}/MetaArrowConverterTestFile.mha)
add_test( itkContourSpatialObjectTest ${SPATIAL_OBJECT_TESTS5} itkContourSpatialObjectTest)

set(SpatialObject_SRCS
  itkAffineGeometryFrameTest.cxx
  itkArrowSpatialObjectTest.cxx
  itkBlobSpatialObjectTest.cxx
)

set(SpatialObject_SRCS5
  itkCylinderSpatialObjectTest.cxx
  itkEllipseSpatialObjectTest.cxx
  itkGaussianSpatialObjectTest.cxx
  itkMetaArrowConverterTest.cxx
  itkContourSpatialObjectTest.cxx
)

set(SpatialObject_SRCS4
  itkImageMaskSpatialObjectTest.cxx
  itkImageMaskSpatialObjectTest2.cxx
  itkImageSpatialObjectTest.cxx
)

set(SpatialObject_SRCS2
  itkPlaneSpatialObjectTest.cxx
  itkPolygonGroupSpatialObjectTest.cxx
  itkPolygonSpatialObjectTest.cxx
  itkSceneSpatialObjectTest.cxx
  itkSpatialObjectTreeContainerTest.cxx
  itkSurfaceSpatialObjectTest.cxx
  itkTubeSpatialObjectTest.cxx
)

set(SpatialObject_SRCS3
  itkBoxSpatialObjectTest.cxx
  itkLandmarkSpatialObjectTest.cxx
  itkLineSpatialObjectTest.cxx
  itkMeshSpatialObjectTest.cxx
  itkSpatialObjectPrintTest.cxx
  itkSpatialObjectDuplicatorTest.cxx
)

add_executable(itkSpatialObjectTests itkSpatialObjectTests.cxx ${SpatialObject_SRCS})
target_link_libraries(itkSpatialObjectTests ITKIO ITKSpatialObject)

add_executable(itkSpatialObjectTests2 itkSpatialObjectTests2.cxx ${SpatialObject_SRCS2})
target_link_libraries(itkSpatialObjectTests2 ITKIO)

add_executable(itkSpatialObjectTests3 itkSpatialObjectTests3.cxx ${SpatialObject_SRCS3})
target_link_libraries(itkSpatialObjectTests3 ITKIO ITKSpatialObject)

add_executable(itkSpatialObjectTests4 itkSpatialObjectTests4.cxx ${SpatialObject_SRCS4})
target_link_libraries(itkSpatialObjectTests4 ITKIO ITKSpatialObject)

add_executable(itkSpatialObjectTests5 itkSpatialObjectTests5.cxx ${SpatialObject_SRCS5})
target_link_libraries(itkSpatialObjectTests5 ITKIO ITKSpatialObject)

add_executable(itkSpatialObjectHeaderTest itkSpatialObjectHeaderTest.cxx)
target_link_libraries(itkSpatialObjectHeaderTest ITKCommon)

if(ITK_OBJCXX_COMPILER_WORKS)
  set(SPATIALOBJECT_OBJCXX_TEST ${CXX_TEST_PATH}/itkSpatialObjectHeaderObjCxxTest)
  add_test(itkSpatialObjectHeaderObjCxxTest ${SPATIALOBJECT_OBJCXX_TEST})
  add_executable(itkSpatialObjectHeaderObjCxxTest itkSpatialObjectHeaderObjCxxTest.mm)
  target_link_libraries(itkSpatialObjectHeaderObjCxxTest ITKCommon)
endif(ITK_OBJCXX_COMPILER_WORKS)

endif( NOT ITK_DISABLE_CXX_TESTING )


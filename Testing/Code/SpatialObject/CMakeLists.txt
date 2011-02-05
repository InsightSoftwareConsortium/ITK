if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-SpatialObject ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/SpatialObject)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)


############################################################
# CXX Testing
#
set(KIT SpatialObject)
set(KIT_LIBS ITKIO ITKSpatialObject)
set(KitTests
  itkAffineGeometryFrameTest
  itkArrowSpatialObjectTest
  itkBlobSpatialObjectTest
  itkBoxSpatialObjectTest
  itkContourSpatialObjectTest
  itkCylinderSpatialObjectTest
  itkEllipseSpatialObjectTest
  itkGaussianSpatialObjectTest
  itkImageMaskSpatialObjectTest
  itkImageMaskSpatialObjectTest2
  itkImageSpatialObjectTest
  itkLandmarkSpatialObjectTest
  itkLineSpatialObjectTest
  itkMeshSpatialObjectTest
  itkMetaArrowConverterTest
  itkPlaneSpatialObjectTest
  itkPolygonGroupSpatialObjectTest
  itkPolygonSpatialObjectTest
  itkSceneSpatialObjectTest
  itkSpatialObjectDuplicatorTest
  itkSpatialObjectPrintTest
  itkSpatialObjectTreeContainerTest
  itkSurfaceSpatialObjectTest
  itkTubeSpatialObjectTest
)
# Given KIT, KIT_LIBS and KitTests, build TestDriver
CreateTestDriver(${KIT} "${KIT_LIBS}" "${KitTests}" TestDriver)


#############################################################
# Tests that use the Test Driver
#
add_test( itkAffineGeometryFrameTest ${TestDriver} itkAffineGeometryFrameTest )
add_test( itkArrowSpatialObjectTest ${TestDriver} itkArrowSpatialObjectTest )

add_test( itkBoxSpatialObjectTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/SpatialObjects/BoxSpatialObjectTest.png
            ${ITK_TEST_OUTPUT_DIR}/BoxSpatialObjectTest.png
  itkBoxSpatialObjectTest
            ${ITK_TEST_OUTPUT_DIR}/BoxSpatialObjectTest.png)

add_test( itkBlobSpatialObjectTest ${TestDriver} itkBlobSpatialObjectTest )
add_test( itkCylinderSpatialObjectTest ${TestDriver} itkCylinderSpatialObjectTest )
add_test( itkEllipseSpatialObjectTest ${TestDriver} itkEllipseSpatialObjectTest)
add_test( itkGaussianSpatialObjectTest ${TestDriver} itkGaussianSpatialObjectTest)
add_test( itkImageMaskSpatialObjectTest ${TestDriver} itkImageMaskSpatialObjectTest )
add_test( itkImageMaskSpatialObjectTest2 ${TestDriver} itkImageMaskSpatialObjectTest2 )
add_test( itkImageSpatialObjectTest ${TestDriver} itkImageSpatialObjectTest )

add_test( itkMeshSpatialObjectTest ${TestDriver} itkMeshSpatialObjectTest )
add_test( itkPlaneSpatialObjectTest ${TestDriver} itkPlaneSpatialObjectTest )
add_test( itkPolygonGroupSpatialObjectTest ${TestDriver} itkPolygonGroupSpatialObjectTest )
add_test( itkPolygonSpatialObjectTest ${TestDriver} itkPolygonSpatialObjectTest )
add_test( itkSceneSpatialObjectTest ${TestDriver} itkSceneSpatialObjectTest )
add_test( itkSpatialObjectTreeContainerTest ${TestDriver} itkSpatialObjectTreeContainerTest )
add_test( itkSurfaceSpatialObjectTest ${TestDriver} itkSurfaceSpatialObjectTest )
add_test( itkTubeSpatialObjectTest ${TestDriver} itkTubeSpatialObjectTest )

add_test( itkLandmarkSpatialObjectTest ${TestDriver} itkLandmarkSpatialObjectTest )
add_test( itkLineSpatialObjectTest ${TestDriver} itkLineSpatialObjectTest )
add_test( itkSpatialObjectDuplicatorTest ${TestDriver} itkSpatialObjectDuplicatorTest )
add_test( itkSpatialObjectPrintTest ${TestDriver} itkSpatialObjectPrintTest)

add_test( itkMetaArrowConverterTest ${TestDriver} itkMetaArrowConverterTest
  ${ITK_TEST_OUTPUT_DIR}/MetaArrowConverterTestFile.mha)
add_test( itkContourSpatialObjectTest ${TestDriver} itkContourSpatialObjectTest)


############################################################
# Tests that have their own executable
#
set(SPATIAL_OBJECT_HEADER_TEST  ${CXX_TEST_PATH}/itkSpatialObjectHeaderTest)
add_test( itkSpatialObjectHeaderTest ${SPATIAL_OBJECT_HEADER_TEST})

add_executable(itkSpatialObjectHeaderTest itkSpatialObjectHeaderTest.cxx)
target_link_libraries(itkSpatialObjectHeaderTest ITKCommon)

if(ITK_OBJCXX_COMPILER_WORKS)
  set(SPATIALOBJECT_OBJCXX_TEST ${CXX_TEST_PATH}/itkSpatialObjectHeaderObjCxxTest)
  add_test(itkSpatialObjectHeaderObjCxxTest ${SPATIALOBJECT_OBJCXX_TEST})
  add_executable(itkSpatialObjectHeaderObjCxxTest itkSpatialObjectHeaderObjCxxTest.mm)
  target_link_libraries(itkSpatialObjectHeaderObjCxxTest ITKCommon)
endif(ITK_OBJCXX_COMPILER_WORKS)

endif( NOT ITK_DISABLE_CXX_TESTING )


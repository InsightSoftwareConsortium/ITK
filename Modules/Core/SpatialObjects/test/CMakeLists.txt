itk_module_test()
set(ITK-SpatialObjectsTests
itkSpatialObjectToImageFilterTest.cxx
itkLandmarkSpatialObjectTest.cxx
itkSpatialObjectsHeaderTest.cxx
itkPolygonGroupSpatialObjectTest.cxx
itkImageSpatialObjectTest.cxx
itkSpatialObjectToImageStatisticsCalculatorTest.cxx
itkImageMaskSpatialObjectTest.cxx
itkImageMaskSpatialObjectTest2.cxx
itkImageMaskSpatialObjectTest3.cxx
itkBlobSpatialObjectTest.cxx
itkContourSpatialObjectTest.cxx
itkSceneSpatialObjectTest.cxx
itkSurfaceSpatialObjectTest.cxx
itkMetaArrowConverterTest.cxx
itkTubeSpatialObjectTest.cxx
itkSpatialObjectToPointSetFilterTest.cxx
itkSpatialObjectDuplicatorTest.cxx
itkPlaneSpatialObjectTest.cxx
itkCylinderSpatialObjectTest.cxx
itkSpatialObjectTreeContainerTest.cxx
itkAffineGeometryFrameTest.cxx
itkBoxSpatialObjectTest.cxx
itkGaussianSpatialObjectTest.cxx
itkPolygonSpatialObjectTest.cxx
itkEllipseSpatialObjectTest.cxx
itkMeshSpatialObjectTest.cxx
itkArrowSpatialObjectTest.cxx
itkLineSpatialObjectTest.cxx
)

CreateTestDriver(ITK-SpatialObjects  "${ITK-SpatialObjects-Test_LIBRARIES}" "${ITK-SpatialObjectsTests}")

add_test(NAME itkSpatialObjectsHeaderTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectsHeaderTest)
add_test(NAME itkSpatialObjectToImageFilterTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectToImageFilterTest)
add_test(NAME itkLandmarkSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkLandmarkSpatialObjectTest)
add_test(NAME itkPolygonGroupSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkPolygonGroupSpatialObjectTest)
add_test(NAME itkImageSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkImageSpatialObjectTest)
add_test(NAME itkSpatialObjectToImageStatisticsCalculatorTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectToImageStatisticsCalculatorTest)
add_test(NAME itkImageMaskSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkImageMaskSpatialObjectTest)
add_test(NAME itkImageMaskSpatialObjectTest2
      COMMAND ITK-SpatialObjectsTestDriver itkImageMaskSpatialObjectTest2)
add_test(NAME itkImageMaskSpatialObjectTest3
      COMMAND ITK-SpatialObjectsTestDriver itkImageMaskSpatialObjectTest3)
add_test(NAME itkBlobSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkBlobSpatialObjectTest)
add_test(NAME itkContourSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkContourSpatialObjectTest)
add_test(NAME itkSceneSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkSceneSpatialObjectTest)
add_test(NAME itkSurfaceSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkSurfaceSpatialObjectTest)
add_test(NAME itkMetaArrowConverterTest
      COMMAND ITK-SpatialObjectsTestDriver itkMetaArrowConverterTest
              ${ITK_TEST_OUTPUT_DIR}/MetaArrowConverterTestFile.mha)
add_test(NAME itkTubeSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkTubeSpatialObjectTest)
add_test(NAME itkSpatialObjectToPointSetFilterTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectToPointSetFilterTest)
add_test(NAME itkSpatialObjectDuplicatorTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectDuplicatorTest)
add_test(NAME itkPlaneSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkPlaneSpatialObjectTest)
add_test(NAME itkCylinderSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkCylinderSpatialObjectTest)
add_test(NAME itkSpatialObjectTreeContainerTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectTreeContainerTest)
add_test(NAME itkAffineGeometryFrameTest
      COMMAND ITK-SpatialObjectsTestDriver itkAffineGeometryFrameTest)
add_test(NAME itkBoxSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/SpatialObjects/BoxSpatialObjectTest.png
              ${ITK_TEST_OUTPUT_DIR}/BoxSpatialObjectTest.png
    itkBoxSpatialObjectTest ${ITK_TEST_OUTPUT_DIR}/BoxSpatialObjectTest.png)
add_test(NAME itkGaussianSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkGaussianSpatialObjectTest)
add_test(NAME itkPolygonSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkPolygonSpatialObjectTest)
add_test(NAME itkEllipseSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkEllipseSpatialObjectTest)
add_test(NAME itkMeshSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkMeshSpatialObjectTest)
add_test(NAME itkArrowSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkArrowSpatialObjectTest)
add_test(NAME itkLineSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkLineSpatialObjectTest)

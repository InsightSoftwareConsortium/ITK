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
itkSpatialObjectTreeContainerIteratorTest.cxx
itkNewMetaObjectTypeTest.cxx
)

CreateTestDriver(ITK-SpatialObjects  "${ITK-SpatialObjects-Test_LIBRARIES}" "${ITK-SpatialObjectsTests}")

itk_add_test(NAME itkSpatialObjectsHeaderTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectsHeaderTest)
itk_add_test(NAME itkSpatialObjectToImageFilterTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectToImageFilterTest)
itk_add_test(NAME itkLandmarkSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkLandmarkSpatialObjectTest)
itk_add_test(NAME itkPolygonGroupSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkPolygonGroupSpatialObjectTest)
itk_add_test(NAME itkImageSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkImageSpatialObjectTest)
itk_add_test(NAME itkSpatialObjectToImageStatisticsCalculatorTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectToImageStatisticsCalculatorTest)
itk_add_test(NAME itkImageMaskSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkImageMaskSpatialObjectTest)
itk_add_test(NAME itkImageMaskSpatialObjectTest2
      COMMAND ITK-SpatialObjectsTestDriver itkImageMaskSpatialObjectTest2)
itk_add_test(NAME itkImageMaskSpatialObjectTest3
      COMMAND ITK-SpatialObjectsTestDriver itkImageMaskSpatialObjectTest3)
itk_add_test(NAME itkBlobSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkBlobSpatialObjectTest)
itk_add_test(NAME itkContourSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkContourSpatialObjectTest)
itk_add_test(NAME itkSceneSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkSceneSpatialObjectTest)
itk_add_test(NAME itkSurfaceSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkSurfaceSpatialObjectTest)
itk_add_test(NAME itkMetaArrowConverterTest
      COMMAND ITK-SpatialObjectsTestDriver itkMetaArrowConverterTest
              ${ITK_TEST_OUTPUT_DIR}/MetaArrowConverterTestFile.mha)
itk_add_test(NAME itkTubeSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkTubeSpatialObjectTest)
itk_add_test(NAME itkSpatialObjectToPointSetFilterTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectToPointSetFilterTest)
itk_add_test(NAME itkSpatialObjectDuplicatorTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectDuplicatorTest)
itk_add_test(NAME itkPlaneSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkPlaneSpatialObjectTest)
itk_add_test(NAME itkCylinderSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkCylinderSpatialObjectTest)
itk_add_test(NAME itkSpatialObjectTreeContainerTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectTreeContainerTest)
itk_add_test(NAME itkAffineGeometryFrameTest
      COMMAND ITK-SpatialObjectsTestDriver itkAffineGeometryFrameTest)
itk_add_test(NAME itkBoxSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/SpatialObjects/BoxSpatialObjectTest.png
              ${ITK_TEST_OUTPUT_DIR}/BoxSpatialObjectTest.png
    itkBoxSpatialObjectTest ${ITK_TEST_OUTPUT_DIR}/BoxSpatialObjectTest.png)
itk_add_test(NAME itkGaussianSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkGaussianSpatialObjectTest)
itk_add_test(NAME itkPolygonSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkPolygonSpatialObjectTest)
itk_add_test(NAME itkEllipseSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkEllipseSpatialObjectTest)
itk_add_test(NAME itkMeshSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkMeshSpatialObjectTest)
itk_add_test(NAME itkArrowSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkArrowSpatialObjectTest)
itk_add_test(NAME itkLineSpatialObjectTest
      COMMAND ITK-SpatialObjectsTestDriver itkLineSpatialObjectTest)
itk_add_test(NAME itkSpatialObjectTreeContainerIteratorTest
      COMMAND ITK-SpatialObjectsTestDriver itkSpatialObjectTreeContainerIteratorTest)
itk_add_test(NAME itkNewMetaObjectTypeTest
      COMMAND ITK-SpatialObjectsTestDriver itkNewMetaObjectTypeTest
      ${ITK_TEST_OUTPUT_DIR}/NewMetaObjectType.meta)

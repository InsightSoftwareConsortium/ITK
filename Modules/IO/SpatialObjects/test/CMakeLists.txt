itk_module_test()

set(ITK-IO-SpatialObjectsTests
itkPolygonGroupSpatialObjectXMLFileTest.cxx
itkIOSpatialobjectsHeaderTest.cxx
itkReadWriteSpatialObjectTest.cxx
)

CreateTestDriver(ITK-IO-SpatialObjects  "${ITK-IO-SpatialObjects-Test_LIBRARIES}" "${ITK-IO-SpatialObjectsTests}")

itk_add_test(NAME itkIOSpatialobjectsHeaderTest
      COMMAND ITK-IO-SpatialObjectsTestDriver itkIOSpatialobjectsHeaderTest)
itk_add_test(NAME itkReadWriteSpatialObjectTest
      COMMAND ITK-IO-SpatialObjectsTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects.meta)
itk_add_test(NAME itkReadWriteSpatialObjectTest1
      COMMAND ITK-IO-SpatialObjectsTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects1.meta binary)
itk_add_test(NAME itkReadWriteSpatialObjectTest2
      COMMAND ITK-IO-SpatialObjectsTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects2.meta ${ITK_DATA_ROOT}/Input/SpatialObjects.meta)
itk_add_test(NAME itkPolygonGroupSpatialObjectXMLFileTest
      COMMAND ITK-IO-SpatialObjectsTestDriver itkPolygonGroupSpatialObjectXMLFileTest
              ${ITK_TEST_OUTPUT_DIR})

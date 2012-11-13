itk_module_test()

set(ITKIOSpatialObjectsTests
itkPolygonGroupSpatialObjectXMLFileTest.cxx
itkReadWriteSpatialObjectTest.cxx
itkReadVesselTubeSpatialObjectTest.cxx
)

CreateTestDriver(ITKIOSpatialObjects  "${ITKIOSpatialObjects-Test_LIBRARIES}" "${ITKIOSpatialObjectsTests}")

itk_add_test(NAME itkReadWriteSpatialObjectTest
      COMMAND ITKIOSpatialObjectsTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects.meta)
itk_add_test(NAME itkReadWriteSpatialObjectTest1
      COMMAND ITKIOSpatialObjectsTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects1.meta binary)
itk_add_test(NAME itkReadWriteSpatialObjectTest2
      COMMAND ITKIOSpatialObjectsTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects2.meta DATA{${ITK_DATA_ROOT}/Input/SpatialObjects.meta})
itk_add_test(NAME itkPolygonGroupSpatialObjectXMLFileTest
      COMMAND ITKIOSpatialObjectsTestDriver itkPolygonGroupSpatialObjectXMLFileTest
              ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkReadVesselTubeSpatialObjectTest
      COMMAND ITKIOSpatialObjectsTestDriver itkReadVesselTubeSpatialObjectTest
              DATA{Input/Branch-truth.tre})

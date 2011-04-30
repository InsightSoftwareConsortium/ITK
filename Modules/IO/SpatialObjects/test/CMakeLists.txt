itk_module_test()
set(ITK-IO-SpatialObjectsTests
itkPolygonGroupSpatialObjectXMLFileTest.cxx
itkIOSpatialobjectsHeaderTest.cxx
)

CreateTestDriver(ITK-IO-SpatialObjects  "${ITK-IO-SpatialObjects-Test_LIBRARIES}" "${ITK-IO-SpatialObjectsTests}")

itk_add_test(NAME itkIOSpatialobjectsHeaderTest
      COMMAND ITK-IO-SpatialObjectsTestDriver itkIOSpatialobjectsHeaderTest)
itk_add_test(NAME itkPolygonGroupSpatialObjectXMLFileTest
      COMMAND ITK-IO-SpatialObjectsTestDriver itkPolygonGroupSpatialObjectXMLFileTest
              ${ITK_TEST_OUTPUT_DIR})

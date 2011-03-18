itk_module_test()
set(ITK-IO-SpatialobjectsTests
itkPolygonGroupSpatialObjectXMLFileTest.cxx
itkIOSpatialobjectsHeaderTest.cxx
)

CreateTestDriver(ITK-IO-Spatialobjects  "${ITK-IO-Spatialobjects-Test_LIBRARIES}" "${ITK-IO-SpatialobjectsTests}")

add_test(NAME itkIOSpatialobjectsHeaderTest
      COMMAND ITK-IO-SpatialobjectsTestDriver itkIOSpatialobjectsHeaderTest)
add_test(NAME itkPolygonGroupSpatialObjectXMLFileTest
      COMMAND ITK-IO-SpatialobjectsTestDriver itkPolygonGroupSpatialObjectXMLFileTest
              ${ITK_TEST_OUTPUT_DIR})

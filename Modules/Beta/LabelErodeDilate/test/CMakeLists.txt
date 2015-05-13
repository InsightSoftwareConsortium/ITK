itk_module_test()
set(LabelErodeDilateTests
itkLabelSetDilateTest.cxx
)

SET(INPUT_IMAGE2D ${CMAKE_CURRENT_SOURCE_DIR}/images/axial.png)

set(ITK_TEST_DRIVER itkTestDriver)

CreateTestDriver(LabelErodeDilate "${LabelErodeDilate-Test_LIBRARIES}" "${LabelErodeDilateTests}")

itk_add_test(NAME itkLabelDilateTest2D_5 
  COMMAND LabelErodeDilateTestDriver 
itkLabelSetDilateTest ${INPUT_IMAGE2D} 5 axialdilate5.png )

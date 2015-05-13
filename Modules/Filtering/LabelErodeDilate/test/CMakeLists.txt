itk_module_test()
set(LabelErodeDilateTests
itkLabelSetDilateTest.cxx
)

SET(INPUT_IMAGE ${CMAKE_CURRENT_SOURCE_DIR}/images/cthead1.png)

set(ITK_TEST_DRIVER itkTestDriver)

CreateTestDriver(LabelErodeDilate "${LabelErodeDilate-Test_LIBRARIES}" "${LabelErodeDilateTests}")


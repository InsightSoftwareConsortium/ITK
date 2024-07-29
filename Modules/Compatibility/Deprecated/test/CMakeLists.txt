itk_module_test()
set(ITKDeprecatedTests
    itkOrientImageFilterTest.cxx
    itkOrientImageFilterTest2.cxx
    itkSpatialOrientationTest.cxx
)

createtestdriver(ITKDeprecated "${ITKDeprecated-Test_LIBRARIES}" "${ITKDeprecatedTests}")

#set(BASELINE "${ITK_DATA_ROOT}/Baseline/Common")
#set(TEMP ${ITK_TEST_OUTPUT_DIR})
itk_add_test(
  NAME
  itkSpatialOrientationTest
  COMMAND
  ITKDeprecatedTestDriver
  itkSpatialOrientationTest)
itk_add_test(
  NAME
  itkOrientImageFilterTest
  COMMAND
  ITKDeprecatedTestDriver
  itkOrientImageFilterTest
  0)
itk_add_test(
  NAME
  itkOrientImageFilterTest2
  COMMAND
  ITKDeprecatedTestDriver
  itkOrientImageFilterTest2)

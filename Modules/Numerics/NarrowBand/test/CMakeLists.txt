itk_module_test()
set(
  ITKNarrowBandTests
  itkNarrowBandImageFilterBaseTest.cxx
  itkNarrowBandTest.cxx
)

createtestdriver(ITKNarrowBand "${ITKNarrowBand-Test_LIBRARIES}" "${ITKNarrowBandTests}")

itk_add_test(
  NAME itkNarrowBandTest
  COMMAND
    ITKNarrowBandTestDriver
    itkNarrowBandTest
)
itk_add_test(
  NAME itkNarrowBandImageFilterBaseTest
  COMMAND
    ITKNarrowBandTestDriver
    itkNarrowBandImageFilterBaseTest
    ${ITK_TEST_OUTPUT_DIR}/itkNarrowBandImageFilterBaseTest.png
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkNarrowBandImageFilterBaseTest.png
)

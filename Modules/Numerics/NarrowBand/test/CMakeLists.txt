itk_module_test()
set(ITKNarrowBandTests
itkNarrowBandTest.cxx
itkNarrowBandImageFilterBaseTest.cxx
)

CreateTestDriver(ITKNarrowBand  "${ITKNarrowBand-Test_LIBRARIES}" "${ITKNarrowBandTests}")

itk_add_test(NAME itkNarrowBandTest
      COMMAND ITKNarrowBandTestDriver itkNarrowBandTest)
itk_add_test(NAME itkNarrowBandImageFilterBaseTest
      COMMAND ITKNarrowBandTestDriver itkNarrowBandImageFilterBaseTest
              ${ITK_TEST_OUTPUT_DIR}/itkNarrowBandImageFilterBaseTest.png)

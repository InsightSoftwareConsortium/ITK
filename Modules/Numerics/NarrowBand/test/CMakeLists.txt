itk_module_test()
set(ITK-NarrowBandTests
itkNarrowBandTest.cxx
itkNarrowBandImageFilterBaseTest.cxx
itkNarrowBandHeaderTest.cxx
)

CreateTestDriver(ITK-NarrowBand  "${ITK-NarrowBand-Test_LIBRARIES}" "${ITK-NarrowBandTests}")

itk_add_test(NAME itkNarrowBandHeaderTest
      COMMAND ITK-NarrowBandTestDriver itkNarrowBandHeaderTest)
itk_add_test(NAME itkNarrowBandTest
      COMMAND ITK-NarrowBandTestDriver itkNarrowBandTest)
itk_add_test(NAME itkNarrowBandImageFilterBaseTest
      COMMAND ITK-NarrowBandTestDriver itkNarrowBandImageFilterBaseTest
              ${ITK_TEST_OUTPUT_DIR}/itkNarrowBandImageFilterBaseTest.png)

itk_module_test()
set(ITK-NarrowBandTests
itkNarrowBandTest.cxx
itkNarrowBandImageFilterBaseTest.cxx
itkNarrowBandHeaderTest.cxx
)

CreateTestDriver(ITK-NarrowBand  "${ITK-NarrowBand-Test_LIBRARIES}" "${ITK-NarrowBandTests}")

add_test(NAME itkNarrowBandHeaderTest
      COMMAND ITK-NarrowBandTestDriver itkNarrowBandHeaderTest)
add_test(NAME itkNarrowBandTest
      COMMAND ITK-NarrowBandTestDriver itkNarrowBandTest)
add_test(NAME itkNarrowBandImageFilterBaseTest
      COMMAND ITK-NarrowBandTestDriver itkNarrowBandImageFilterBaseTest
              ${ITK_TEST_OUTPUT_DIR}/itkNarrowBandImageFilterBaseTest.png)

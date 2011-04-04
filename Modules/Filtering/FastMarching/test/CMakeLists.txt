itk_module_test()
set(ITK-FastMarchingTests
itkFastMarchingHeaderTest.cxx
itkFastMarchingExtensionImageFilterTest.cxx
itkFastMarchingTest.cxx
itkFastMarchingTest2.cxx
itkFastMarchingUpwindGradientTest.cxx
)

CreateTestDriver(ITK-FastMarching "${ITK-FastMarching-Test_LIBRARIES}" "${ITK-FastMarchingTests}")

add_test(NAME itkFastMarchingHeaderTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingHeaderTest)
add_test(NAME itkFastMarchingExtensionImageFilterTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingExtensionImageFilterTest)
add_test(NAME itkFastMarchingTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingTest)
add_test(NAME itkFastMarchingTest2
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingTest2)
add_test(NAME itkFastMarchingUpwindGradientTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingUpwindGradientTest)

itk_module_test()
set(ITKEigenTests
itkEigenAnalysis2DImageFilterTest.cxx
)

CreateTestDriver(ITKEigen  "${ITKEigen-Test_LIBRARIES}" "${ITKEigenTests}")

itk_add_test(NAME itkEigenAnalysis2DImageFilterTest
      COMMAND ITKEigenTestDriver itkEigenAnalysis2DImageFilterTest)

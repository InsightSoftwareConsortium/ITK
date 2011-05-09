itk_module_test()
set(ITK-EigenTests
itkEigenHeaderTest.cxx
itkEigenAnalysis2DImageFilterTest.cxx
itkSymmetricEigenSystemTest.cxx
)

CreateTestDriver(ITK-Eigen  "${ITK-Eigen-Test_LIBRARIES}" "${ITK-EigenTests}")

itk_add_test(NAME itkEigenHeaderTest
      COMMAND ITK-EigenTestDriver itkEigenHeaderTest)
itk_add_test(NAME itkEigenAnalysis2DImageFilterTest
      COMMAND ITK-EigenTestDriver itkEigenAnalysis2DImageFilterTest)
itk_add_test(NAME itkSymmetricEigenSystemTest
      COMMAND ITK-EigenTestDriver itkSymmetricEigenSystemTest)

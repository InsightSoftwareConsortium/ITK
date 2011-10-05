itk_module_test()
set(ITKFEMRegistrationTests
itkFEMFiniteDifferenceFunctionLoadTest.cxx
itkFEMRegistrationFilterTest.cxx
itkMIRegistrationFunctionTest.cxx
)

CreateTestDriver(ITKFEMRegistration  "${ITKFEMRegistration-Test_LIBRARIES}" "${ITKFEMRegistrationTests}")

itk_add_test(NAME itkFEMFiniteDifferenceFunctionLoadTest
      COMMAND ITKFEMRegistrationTestDriver itkFEMFiniteDifferenceFunctionLoadTest
      ${ITK_TEST_OUTPUT_DIR}/FiniteDifferenceFunctionLoadTest)
itk_add_test(NAME itkFEMRegistrationFilterTest
      COMMAND ITKFEMRegistrationTestDriver itkFEMRegistrationFilterTest)
itk_add_test(NAME itkMIRegistrationFunctionTest
      COMMAND ITKFEMRegistrationTestDriver itkMIRegistrationFunctionTest)

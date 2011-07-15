itk_module_test()
set(ITKFEMRegistrationTests
itkFEMRegistrationFilterTest.cxx
itkMIRegistrationFunctionTest.cxx
itkFEMRegistrationHeaderTest.cxx
)

CreateTestDriver(ITKFEMRegistration  "${ITKFEMRegistration-Test_LIBRARIES}" "${ITKFEMRegistrationTests}")

itk_add_test(NAME itkFEMRegistrationHeaderTest
      COMMAND ITKFEMRegistrationTestDriver itkFEMRegistrationHeaderTest)
itk_add_test(NAME itkFEMRegistrationFilterTest
      COMMAND ITKFEMRegistrationTestDriver itkFEMRegistrationFilterTest)
itk_add_test(NAME itkMIRegistrationFunctionTest
      COMMAND ITKFEMRegistrationTestDriver itkMIRegistrationFunctionTest)

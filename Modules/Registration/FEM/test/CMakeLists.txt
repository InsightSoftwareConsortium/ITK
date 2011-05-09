itk_module_test()
set(ITK-FEMRegistrationTests
itkFEMRegistrationFilterTest.cxx
itkMIRegistrationFunctionTest.cxx
itkFEMRegistrationHeaderTest.cxx
)

CreateTestDriver(ITK-FEMRegistration  "${ITK-FEMRegistration-Test_LIBRARIES}" "${ITK-FEMRegistrationTests}")

itk_add_test(NAME itkFEMRegistrationHeaderTest
      COMMAND ITK-FEMRegistrationTestDriver itkFEMRegistrationHeaderTest)
itk_add_test(NAME itkFEMRegistrationFilterTest
      COMMAND ITK-FEMRegistrationTestDriver itkFEMRegistrationFilterTest)
itk_add_test(NAME itkMIRegistrationFunctionTest
      COMMAND ITK-FEMRegistrationTestDriver itkMIRegistrationFunctionTest)

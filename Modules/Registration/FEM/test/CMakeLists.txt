itk_module_test()
set(ITK-FEMRegistrationTests
itkFEMRegistrationFilterTest.cxx
itkMIRegistrationFunctionTest.cxx
itkFEMRegistrationHeaderTest.cxx
)

CreateTestDriver(ITK-FEMRegistration  "${ITK-FEMRegistration-Test_LIBRARIES}" "${ITK-FEMRegistrationTests}")

add_test(NAME itkFEMRegistrationHeaderTest
      COMMAND ITK-FEMRegistrationTestDriver itkFEMRegistrationHeaderTest)
add_test(NAME itkFEMRegistrationFilterTest
      COMMAND ITK-FEMRegistrationTestDriver itkFEMRegistrationFilterTest)
add_test(NAME itkMIRegistrationFunctionTest
      COMMAND ITK-FEMRegistrationTestDriver itkMIRegistrationFunctionTest)

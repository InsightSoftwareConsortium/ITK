itk_module_test()
set(ITKFEMRegistrationTests
itkFEMFiniteDifferenceFunctionLoadTest.cxx
itkFEMRegistrationFilterTest.cxx
itkFEMRegistrationFilter2DTest.cxx
itkMIRegistrationFunctionTest.cxx
itkFEMRegistrationFilterTest2.cxx
)

CreateTestDriver(ITKFEMRegistration  "${ITKFEMRegistration-Test_LIBRARIES}" "${ITKFEMRegistrationTests}")

itk_add_test(NAME itkFEMFiniteDifferenceFunctionLoadTest
      COMMAND ITKFEMRegistrationTestDriver itkFEMFiniteDifferenceFunctionLoadTest
      ${ITK_TEST_OUTPUT_DIR}/FiniteDifferenceFunctionLoadTest)
itk_add_test(NAME itkFEMRegistrationFilterTest
      COMMAND ITKFEMRegistrationTestDriver itkFEMRegistrationFilterTest
      ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField3D ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage3D)
itk_add_test(NAME itkFEMRegistrationFilter2DTest
      COMMAND ITKFEMRegistrationTestDriver itkFEMRegistrationFilter2DTest
      ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField2D ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage2D)
itk_add_test(NAME itkMIRegistrationFunctionTest
      COMMAND ITKFEMRegistrationTestDriver itkMIRegistrationFunctionTest)
itk_add_test(NAME itkFEMRegistrationFilterTest2
      COMMAND ITKFEMRegistrationTestDriver itkFEMRegistrationFilterTest2
      ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField3DMultiRes ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage3DMultiRes)

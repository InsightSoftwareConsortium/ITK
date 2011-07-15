itk_module_test()
set(ITKDeformationFieldTests
itkDisplacementFieldJacobianDeterminantFilterTest.cxx
itkDeformationFieldHeaderTest.cxx
itkDeformationFieldJacobianDeterminantFilterTest.cxx
itkIterativeInverseDeformationFieldImageFilterTest.cxx
itkDeformationFieldSourceTest.cxx
itkInverseDeformationFieldImageFilterTest.cxx
)

CreateTestDriver(ITKDeformationField  "${ITKDeformationField-Test_LIBRARIES}" "${ITKDeformationFieldTests}")

itk_add_test(NAME itkDeformationFieldHeaderTest
      COMMAND ITKDeformationFieldTestDriver itkDeformationFieldHeaderTest)
itk_add_test(NAME itkDisplacementFieldJacobianDeterminantFilterTest
      COMMAND ITKDeformationFieldTestDriver itkDisplacementFieldJacobianDeterminantFilterTest)
itk_add_test(NAME itkDeformationFieldJacobianDeterminantFilterTest
      COMMAND ITKDeformationFieldTestDriver itkDeformationFieldJacobianDeterminantFilterTest)
itk_add_test(NAME itkIterativeInverseDeformationFieldImageFilterTest
      COMMAND ITKDeformationFieldTestDriver itkIterativeInverseDeformationFieldImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkIterativeInverseDeformationFieldImageFilterTest.mha)
itk_add_test(NAME itkDeformationFieldSourceTest
      COMMAND ITKDeformationFieldTestDriver itkDeformationFieldSourceTest
              ${ITK_TEST_OUTPUT_DIR}/itkDeformationFieldSourceTestLandmarks.txt ${ITK_TEST_OUTPUT_DIR}/itkDeformationFieldSourceTest.mha)
itk_add_test(NAME itkInverseDeformationFieldImageFilterTest
      COMMAND ITKDeformationFieldTestDriver itkInverseDeformationFieldImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkInverseDeformationFieldImageFilterTest.mha)

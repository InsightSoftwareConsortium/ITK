itk_module_test()
set(ITK-DeformationFieldTests
itkDisplacementFieldJacobianDeterminantFilterTest.cxx
itkDeformationFieldHeaderTest.cxx
itkDeformationFieldJacobianDeterminantFilterTest.cxx
itkIterativeInverseDeformationFieldImageFilterTest.cxx
itkDeformationFieldSourceTest.cxx
itkInverseDeformationFieldImageFilterTest.cxx
)

CreateTestDriver(ITK-DeformationField  "${ITK-DeformationField-Test_LIBRARIES}" "${ITK-DeformationFieldTests}")

itk_add_test(NAME itkDeformationFieldHeaderTest
      COMMAND ITK-DeformationFieldTestDriver itkDeformationFieldHeaderTest)
itk_add_test(NAME itkDisplacementFieldJacobianDeterminantFilterTest
      COMMAND ITK-DeformationFieldTestDriver itkDisplacementFieldJacobianDeterminantFilterTest)
itk_add_test(NAME itkDeformationFieldJacobianDeterminantFilterTest
      COMMAND ITK-DeformationFieldTestDriver itkDeformationFieldJacobianDeterminantFilterTest)
itk_add_test(NAME itkIterativeInverseDeformationFieldImageFilterTest
      COMMAND ITK-DeformationFieldTestDriver itkIterativeInverseDeformationFieldImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkIterativeInverseDeformationFieldImageFilterTest.mha)
itk_add_test(NAME itkDeformationFieldSourceTest
      COMMAND ITK-DeformationFieldTestDriver itkDeformationFieldSourceTest
              ${ITK_TEST_OUTPUT_DIR}/itkDeformationFieldSourceTestLandmarks.txt ${ITK_TEST_OUTPUT_DIR}/itkDeformationFieldSourceTest.mha)
itk_add_test(NAME itkInverseDeformationFieldImageFilterTest
      COMMAND ITK-DeformationFieldTestDriver itkInverseDeformationFieldImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkInverseDeformationFieldImageFilterTest.mha)

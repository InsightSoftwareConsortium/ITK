itk_module_test()
set(ITKDisplacementFieldTests
itkDisplacementFieldJacobianDeterminantFilterTest.cxx
itkDeformationFieldJacobianDeterminantFilterTest.cxx
itkIterativeInverseDisplacementFieldImageFilterTest.cxx
itkLandmarkDisplacementFieldSourceTest.cxx
itkInverseDisplacementFieldImageFilterTest.cxx
)

CreateTestDriver(ITKDisplacementField  "${ITKDisplacementField-Test_LIBRARIES}" "${ITKDisplacementFieldTests}")

itk_add_test(NAME itkDisplacementFieldJacobianDeterminantFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkDisplacementFieldJacobianDeterminantFilterTest)
itk_add_test(NAME itkDeformationFieldJacobianDeterminantFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkDeformationFieldJacobianDeterminantFilterTest)
itk_add_test(NAME itkIterativeInverseDisplacementFieldImageFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkIterativeInverseDisplacementFieldImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkIterativeInverseDisplacementFieldImageFilterTest.mha)
itk_add_test(NAME itkLandmarkDisplacementFieldSourceTest
      COMMAND ITKDisplacementFieldTestDriver itkLandmarkDisplacementFieldSourceTest
              ${ITK_TEST_OUTPUT_DIR}/itkLandmarkDisplacementFieldSourceTestLandmarks.txt ${ITK_TEST_OUTPUT_DIR}/itkLandmarkDisplacementFieldSourceTest.mha)
itk_add_test(NAME itkInverseDisplacementFieldImageFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkInverseDisplacementFieldImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkInverseDisplacementFieldImageFilterTest.mha)

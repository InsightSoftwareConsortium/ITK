itk_module_test()
set(ITKV3CompatibilityTests
  itkDeformationFieldSourceTest.cxx
  itkExponentialDeformationFieldImageFilterTest.cxx
  itkInverseDeformationFieldImageFilterTest.cxx
  itkIterativeInverseDeformationFieldImageFilterTest.cxx
  itkTransformToDeformationFieldSourceTest.cxx
  itkTransformToDeformationFieldSourceTest1.cxx
  itkDeformationFieldJacobianDeterminantFilterTest.cxx
)

CreateTestDriver(ITKV3Compatibility  "${ITKV3Compatibility-Test_LIBRARIES}" "${ITKV3CompatibilityTests}")

# DEFORMATION
itk_add_test(NAME itkDeformationFieldSourceTest
      COMMAND ITKV3CompatibilityTestDriver itkDeformationFieldSourceTest
              ${ITK_TEST_OUTPUT_DIR}/itkDeformationFieldSourceTestLandmarks.txt ${ITK_TEST_OUTPUT_DIR}/itkDeformationFieldSourceTest.mha)
itk_add_test(NAME itkInverseDeformationFieldImageFilterTest
      COMMAND ITKV3CompatibilityTestDriver itkInverseDeformationFieldImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkInverseDeformationFieldImageFilterTest.mha)
itk_add_test(NAME itkIterativeInverseDeformationFieldImageFilterTest
      COMMAND ITKV3CompatibilityTestDriver itkIterativeInverseDeformationFieldImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkIterativeInverseDeformationFieldImageFilterTest.mha)
itk_add_test(NAME itkDeformationFieldJacobianDeterminantFilterTest
  COMMAND ITKV3CompatibilityTestDriver itkDeformationFieldJacobianDeterminantFilterTest)
itk_add_test(NAME itkTransformToDeformationFieldSourceTest03
      COMMAND ITKV3CompatibilityTestDriver
      --compare ${ITK_TEST_OUTPUT_DIR}/transformedImage.nii
                ${ITK_TEST_OUTPUT_DIR}/warpedImage.nii
  --compareNumberOfPixelsTolerance 20
  itkTransformToDeformationFieldSourceTest1
  ${ITK_TEST_OUTPUT_DIR}/transformedImage.nii ${ITK_TEST_OUTPUT_DIR}/warpedImage.nii)
itk_add_test(NAME itkTransformToDeformationFieldSourceTest01
      COMMAND ITKV3CompatibilityTestDriver itkTransformToDeformationFieldSourceTest
              Affine ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestField01.mha ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestImage01.mha)
itk_add_test(NAME itkTransformToDeformationFieldSourceTest02
      COMMAND ITKV3CompatibilityTestDriver itkTransformToDeformationFieldSourceTest
              BSpline ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestField02.mha ${ITK_DATA_ROOT}/Input/parametersBSpline.txt ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestImage02.mha)
itk_add_test(NAME itkExponentialDeformationFieldImageFilterTest
      COMMAND ITKV3CompatibilityTestDriver itkExponentialDeformationFieldImageFilterTest)

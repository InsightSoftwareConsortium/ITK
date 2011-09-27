itk_module_test()
set(ITKV3CompatibilityTests
  itkDeformationFieldSourceTest.cxx
  itkExponentialDeformationFieldImageFilterTest.cxx
  itkInverseDeformationFieldImageFilterTest.cxx
  itkIterativeInverseDeformationFieldImageFilterTest.cxx
  itkTransformToDeformationFieldSourceTest.cxx
  itkTransformToDeformationFieldSourceTest1.cxx
  itkDeformationFieldJacobianDeterminantFilterTest.cxx
  itkBSplineDeformableTransformTest.cxx
  itkBSplineDeformableTransformTest2.cxx
  itkBSplineDeformableTransformTest3.cxx
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


## Tests for ITKv3 version of BSplineDeformableTransforms
itk_add_test(NAME itkBSplineDeformableTransformTest
      COMMAND ITKV3CompatibilityTestsDriver itkBSplineDeformableTransformTest)
itk_add_test(NAME itkBSplineDeformableTransformTest2
      COMMAND ITKV3CompatibilityTestsDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest2PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2PixelCentered.png
    itkBSplineDeformableTransformTest2 DATA{Baseline/BSplineDeformationTransformDisplacements5.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2DeformationFieldPixelCentered.mhd 1)
itk_add_test(NAME itkBSplineDeformableTransformTest3
      COMMAND ITKV3CompatibilityTestsDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest3PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3PixelCentered.png
    itkBSplineDeformableTransformTest2 DATA{Baseline/BSplineDeformationTransformDisplacements3.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3DeformationFieldPixelCentered.mhd 2)
itk_add_test(NAME itkBSplineDeformableTransformTest4
      COMMAND ITKV3CompatibilityTestsDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest4PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4PixelCentered.png
    itkBSplineDeformableTransformTest2 DATA{Baseline/BSplineDeformationTransformDisplacements1.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4DeformationFieldPixelCentered.mhd 3)
itk_add_test(NAME itkBSplineDeformableTransformTest5
      COMMAND ITKV3CompatibilityTestsDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest5PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5PixelCentered.png
    itkBSplineDeformableTransformTest3 DATA{Baseline/BSplineDeformationTransformDisplacements1.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5DeformationFieldPixelCentered.mhd 0)
itk_add_test(NAME itkBSplineDeformableTransformTest6
      COMMAND ITKV3CompatibilityTestsDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest5PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6PixelCentered.png
    itkBSplineDeformableTransformTest3 DATA{Baseline/BSplineDeformationTransformDisplacements1.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6DeformationFieldPixelCentered.mhd 1)
itk_add_test(NAME itkBSplineDeformableTransformTest7
      COMMAND ITKV3CompatibilityTestsDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest5PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7PixelCentered.png
    itkBSplineDeformableTransformTest3 DATA{Baseline/BSplineDeformationTransformDisplacements1.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7DeformationFieldPixelCentered.mhd 2)

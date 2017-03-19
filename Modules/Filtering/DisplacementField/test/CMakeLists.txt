itk_module_test()
set(ITKDisplacementFieldTests
itkComposeDisplacementFieldsImageFilterTest.cxx
itkDisplacementFieldJacobianDeterminantFilterTest.cxx
itkIterativeInverseDisplacementFieldImageFilterTest.cxx
itkLandmarkDisplacementFieldSourceTest.cxx
itkInverseDisplacementFieldImageFilterTest.cxx
itkInvertDisplacementFieldImageFilterTest.cxx
itkDisplacementFieldToBSplineImageFilterTest.cxx
itkDisplacementFieldTransformTest.cxx
itkGaussianSmoothingOnUpdateDisplacementFieldTransformTest.cxx
itkBSplineSmoothingOnUpdateDisplacementFieldTransformTest.cxx
itkGaussianExponentialDiffeomorphicTransformTest.cxx
itkBSplineExponentialDiffeomorphicTransformTest.cxx
itkTimeVaryingVelocityFieldTransformTest.cxx
itkTimeVaryingVelocityFieldIntegrationImageFilterTest.cxx
itkTimeVaryingBSplineVelocityFieldTransformTest.cxx
itkTransformToDisplacementFieldFilterTest.cxx
itkTransformToDisplacementFieldFilterTest1.cxx
itkDisplacementFieldTransformCloneTest.cxx
itkExponentialDisplacementFieldImageFilterTest.cxx
)

CreateTestDriver(ITKDisplacementField  "${ITKDisplacementField-Test_LIBRARIES}" "${ITKDisplacementFieldTests}")

itk_add_test(NAME itkComposeDisplacementFieldsImageFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkComposeDisplacementFieldsImageFilterTest )
itk_add_test(NAME itkDisplacementFieldJacobianDeterminantFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkDisplacementFieldJacobianDeterminantFilterTest)
itk_add_test(NAME itkIterativeInverseDisplacementFieldImageFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkIterativeInverseDisplacementFieldImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkIterativeInverseDisplacementFieldImageFilterTest.mha)
itk_add_test(NAME itkLandmarkDisplacementFieldSourceTest
      COMMAND ITKDisplacementFieldTestDriver itkLandmarkDisplacementFieldSourceTest
              ${ITK_TEST_OUTPUT_DIR}/itkLandmarkDisplacementFieldSourceTestLandmarks.txt ${ITK_TEST_OUTPUT_DIR}/itkLandmarkDisplacementFieldSourceTest.mha)
itk_add_test(NAME itkInverseDisplacementFieldImageFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkInverseDisplacementFieldImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkInverseDisplacementFieldImageFilterTest.mha)
itk_add_test(NAME itkDisplacementFieldTransformTest
      COMMAND ITKDisplacementFieldTestDriver itkDisplacementFieldTransformTest 1e-6 1e-6)
itk_add_test(NAME itkGaussianSmoothingOnUpdateDisplacementFieldTransformTest
      COMMAND ITKDisplacementFieldTestDriver
      itkGaussianSmoothingOnUpdateDisplacementFieldTransformTest)
itk_add_test(NAME itkBSplineSmoothingOnUpdateDisplacementFieldTransformTest
      COMMAND ITKDisplacementFieldTestDriver
      itkBSplineSmoothingOnUpdateDisplacementFieldTransformTest)
itk_add_test(NAME itkGaussianExponentialDiffeomorphicTransformTest
      COMMAND ITKDisplacementFieldTestDriver
      itkGaussianExponentialDiffeomorphicTransformTest)
itk_add_test(NAME itkBSplineExponentialDiffeomorphicTransformTest
      COMMAND ITKDisplacementFieldTestDriver
      itkBSplineExponentialDiffeomorphicTransformTest)
itk_add_test(NAME itkTimeVaryingVelocityFieldTransformTest
      COMMAND ITKDisplacementFieldTestDriver itkTimeVaryingVelocityFieldTransformTest )
itk_add_test(NAME itkTimeVaryingVelocityFieldIntegrationImageFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkTimeVaryingVelocityFieldIntegrationImageFilterTest )
itk_add_test(NAME itkTimeVaryingBSplineVelocityFieldTransformTest
      COMMAND ITKDisplacementFieldTestDriver itkTimeVaryingBSplineVelocityFieldTransformTest )
itk_add_test(NAME itkInvertDisplacementFieldImageFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkInvertDisplacementFieldImageFilterTest )
itk_add_test(NAME itkDisplacementFieldToBSplineImageFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkDisplacementFieldToBSplineImageFilterTest )
itk_add_test(NAME itkTransformToDisplacementFieldFilterTest01
      COMMAND ITKDisplacementFieldTestDriver
        --compare ${ITK_TEST_OUTPUT_DIR}/itkTransformToDisplacementFieldFilterTestField01.mha
                  DATA{Baseline/itkTransformToDisplacementFieldFilterTestField01.mha}
        itkTransformToDisplacementFieldFilterTest
            Affine
            ${ITK_TEST_OUTPUT_DIR}/itkTransformToDisplacementFieldFilterTestField01.mha)
itk_add_test(NAME itkTransformToDisplacementFieldFilterTest02
      COMMAND ITKDisplacementFieldTestDriver
        --compare ${ITK_TEST_OUTPUT_DIR}/itkTransformToDisplacementFieldFilterTestField02.mha
                  DATA{Baseline/itkTransformToDisplacementFieldFilterTestField02.mha}
        itkTransformToDisplacementFieldFilterTest
              BSpline
              ${ITK_TEST_OUTPUT_DIR}/itkTransformToDisplacementFieldFilterTestField02.mha
              DATA{${ITK_DATA_ROOT}/Input/parametersBSpline.txt} )
itk_add_test(NAME itkTransformToDisplacementFieldFilterTest03
      COMMAND ITKDisplacementFieldTestDriver
        --compare ${ITK_TEST_OUTPUT_DIR}/transformedImage.nii
                  ${ITK_TEST_OUTPUT_DIR}/warpedImage.nii
        --compareNumberOfPixelsTolerance 20
        itkTransformToDisplacementFieldFilterTest1 ${ITK_TEST_OUTPUT_DIR}/transformedImage.nii ${ITK_TEST_OUTPUT_DIR}/warpedImage.nii)
itk_add_test(NAME itkDisplacementFieldTransformCloneTest
  COMMAND ITKDisplacementFieldTestDriver itkDisplacementFieldTransformCloneTest)
itk_add_test(NAME itkExponentialDisplacementFieldImageFilterTest
      COMMAND ITKDisplacementFieldTestDriver itkExponentialDisplacementFieldImageFilterTest)

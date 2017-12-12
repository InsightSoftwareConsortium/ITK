itk_module_test()
set(ITKRegistrationMethodsv4Tests
itkImageRegistrationSamplingTest.cxx
itkSimpleImageRegistrationTest.cxx
itkSimpleImageRegistrationTest2.cxx
itkSimpleImageRegistrationTest3.cxx
itkSimpleImageRegistrationTest4.cxx
itkSimpleImageRegistrationTestWithMaskAndSampling.cxx
itkSimplePointSetRegistrationTest.cxx
itkExponentialImageRegistrationTest.cxx
itkBSplineExponentialImageRegistrationTest.cxx
itkTimeVaryingBSplineVelocityFieldImageRegistrationTest.cxx
itkTimeVaryingVelocityFieldImageRegistrationTest.cxx
itkSyNImageRegistrationTest.cxx
itkSyNPointSetRegistrationTest.cxx
itkBSplineSyNImageRegistrationTest.cxx
itkBSplineSyNPointSetRegistrationTest.cxx
itkTimeVaryingBSplineVelocityFieldPointSetRegistrationTest.cxx
itkQuasiNewtonOptimizerv4RegistrationTest.cxx
itkBSplineImageRegistrationTest.cxx
)

set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKRegistrationMethodsv4  "${ITKRegistrationMethodsv4-Test_LIBRARIES}" "${ITKRegistrationMethodsv4Tests}")

itk_add_test(NAME itkImageRegistrationSamplingTest
      COMMAND ITKRegistrationMethodsv4TestDriver
      itkImageRegistrationSamplingTest
      )

itk_add_test(NAME itkSimpleImageRegistrationTestDouble
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
      --compareIntensityTolerance 3.0
      --compareNumberOfPixelsTolerance 8
      --compare DATA{Baseline/itkSimpleImageRegistrationTest.nii,:}
              ${TEMP}/itkSimpleImageRegistrationTestDouble.nii
              itkSimpleImageRegistrationTest
              double
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkSimpleImageRegistrationTestDouble.nii
              100 # number of affine iterations
              10 # number of deformable iterations
              )
set_property(TEST itkSimpleImageRegistrationTestDouble APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkSimpleImageRegistrationTestFloat
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
      --compareIntensityTolerance 3.0
      --compareNumberOfPixelsTolerance 8
      --compare DATA{Baseline/itkSimpleImageRegistrationTest.nii,:}
              ${TEMP}/itkSimpleImageRegistrationTestFloat.nii
              itkSimpleImageRegistrationTest
              float
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkSimpleImageRegistrationTestFloat.nii
              100 # number of affine iterations
              10 # number of deformable iterations
              )
set_property(TEST itkSimpleImageRegistrationTestFloat APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkSimpleImageRegistrationTestWithMaskAndSamplingDouble
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
              itkSimpleImageRegistrationTestWithMaskAndSampling
              double
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkSimpleImageRegistrationTestWithMaskAndSamplingDouble.nii.gz
              100 # number of affine iterations
              10 # number of deformable iterations
              )
set_property(TEST itkSimpleImageRegistrationTestWithMaskAndSamplingDouble APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkSimpleImageRegistrationTestWithMaskAndSamplingFloat
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
              itkSimpleImageRegistrationTestWithMaskAndSampling
              float
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkSimpleImageRegistrationTestWithMaskAndSamplingFloat.nii.gz
              100 # number of affine iterations
              10 # number of deformable iterations
              )
set_property(TEST itkSimpleImageRegistrationTestWithMaskAndSamplingFloat APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkSimpleImageRegistrationTest2
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
              itkSimpleImageRegistrationTest2
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkSimpleImageRegistrationTest2New.nii.gz
              100 # number of affine iterations
              )
set_property(TEST itkSimpleImageRegistrationTest2 APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkSimpleImageRegistrationTest3
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
              itkSimpleImageRegistrationTest3
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkSimpleImageRegistrationTest3New.nii.gz
              )
set_property(TEST itkSimpleImageRegistrationTest3 APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkSimpleImageRegistrationTest4
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
              itkSimpleImageRegistrationTest4
              2 # number of dimensions
              DATA{Input/Blob1.mha}
              DATA{Input/Blob2.mha}
              )


itk_add_test(NAME itkSimplePointSetRegistrationTest
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
      itkSimplePointSetRegistrationTest
      )
set_property(TEST itkSimplePointSetRegistrationTest APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkSyNPointSetRegistrationTest
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
      itkSyNPointSetRegistrationTest
      )
set_property(TEST itkSyNPointSetRegistrationTest APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkBSplineSyNPointSetRegistrationTest
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
      itkBSplineSyNPointSetRegistrationTest
      )
set_property(TEST itkBSplineSyNPointSetRegistrationTest APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkTimeVaryingBSplineVelocityFieldPointSetRegistrationTest
      COMMAND ITKRegistrationMethodsv4TestDriver
      itkTimeVaryingBSplineVelocityFieldPointSetRegistrationTest
      )
set_property(TEST itkTimeVaryingBSplineVelocityFieldPointSetRegistrationTest APPEND PROPERTY LABELS RUNS_LONG)


itk_add_test(NAME itkExponentialImageRegistrationTest
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
      --compareIntensityTolerance 3.0
      --compareNumberOfPixelsTolerance 4
#       --compare DATA{Baseline/itkSimpleImageRegistrationTestNew.nii.gz}
#                 ${TEMP}/itkExponentialImageRegistrationTestNew.nii.gz
              itkExponentialImageRegistrationTest
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkExponentialImageRegistrationTestNew.nii.gz
              100 # number of affine iterations
              10 # number of deformable iterations
              )
set_property(TEST itkExponentialImageRegistrationTest APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkBSplineExponentialImageRegistrationTest
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
      --compareIntensityTolerance 3.0
      --compareNumberOfPixelsTolerance 4
#       --compare DATA{Baseline/itkSimpleImageRegistrationTestNew.nii.gz}
#                 ${TEMP}/itkBSplineExponentialImageRegistrationTestNew.nii.gz
              itkBSplineExponentialImageRegistrationTest
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkBSplineExponentialImageRegistrationTestNew.nii.gz
              100 # number of affine iterations
              10 # number of deformable iterations
              )
set_property(TEST itkBSplineExponentialImageRegistrationTest APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkTimeVaryingVelocityFieldImageRegistrationTest
      COMMAND ITKRegistrationMethodsv4TestDriver
              itkTimeVaryingVelocityFieldImageRegistrationTest
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkTimeVaryingVelocityFieldImageRegistrationTest
              2  # number of optimization iterations of the affine transform
              1  # number of iterations for first level of the displacement field
              1  # number of iterations for second level of the displacement field
              1  # number of iterations for third level of the displacement field
              0.5 # learning rate
              )
set_property(TEST itkTimeVaryingVelocityFieldImageRegistrationTest APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkTimeVaryingBSplineVelocityFieldImageRegistrationTest
      COMMAND ITKRegistrationMethodsv4TestDriver
              itkTimeVaryingBSplineVelocityFieldImageRegistrationTest
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkTimeVaryingBSplineVelocityFieldImageRegistrationTest
              2  # number of optimization iterations of the affine transform
              1  # number of iterations for first level of the displacement field
              1  # number of iterations for second level of the displacement field
              1  # number of iterations for third level of the displacement field
              0.5 # learning rate
              )

set_property(TEST itkTimeVaryingBSplineVelocityFieldImageRegistrationTest APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME itkSyNImageRegistrationTest
      COMMAND ITKRegistrationMethodsv4TestDriver
              itkSyNImageRegistrationTest
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkSyNImageRegistrationTest
              10  # number of optimization iterations of the displacement field
              0.5 # learning rate
              )

itk_add_test(NAME itkBSplineSyNImageRegistrationTest
      COMMAND ITKRegistrationMethodsv4TestDriver
              itkBSplineSyNImageRegistrationTest
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkBSplineSyNImageRegistrationTest
              10  # number of optimization iterations of the displacement field
              0.5 # learning rate
              )

itk_add_test(NAME itkQuasiNewtonOptimizerv4RegistrationTest1
      COMMAND ITKRegistrationMethodsv4TestDriver
              itkQuasiNewtonOptimizerv4RegistrationTest
              2 # number of dimensions
              ms # MeanSquares metric
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkQuasiNewtonOptimizerv4RegistrationTest1.nii.gz
              5 2 )

itk_add_test(NAME itkQuasiNewtonOptimizerv4RegistrationTest2
      COMMAND ITKRegistrationMethodsv4TestDriver
              itkQuasiNewtonOptimizerv4RegistrationTest
              2 # number of dimensions
              mi # metric of joint histogram mutual information
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkQuasiNewtonOptimizerv4RegistrationTest2.nii.gz
              5 2 )

itk_add_test(NAME itkQuasiNewtonOptimizerv4RegistrationTest3
      COMMAND ITKRegistrationMethodsv4TestDriver
              itkQuasiNewtonOptimizerv4RegistrationTest
              2 # number of dimensions
              anc # metric of ants neighborhood correlation
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkQuasiNewtonOptimizerv4RegistrationTest3.nii.gz
              5 2 )

itk_add_test(NAME itkBSplineImageRegistrationTest
      COMMAND ITKRegistrationMethodsv4TestDriver
      --with-threads 1
      --compareIntensityTolerance 3.0
      --compareNumberOfPixelsTolerance 4
#       --compare DATA{Baseline/itkSimpleImageRegistrationTestNew.nii.gz}
#                 ${TEMP}/itkBSplineImageRegistrationTestNew.nii.gz
              itkBSplineImageRegistrationTest
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkBSplineImageRegistrationTestNew.nii.gz
              100 # number of affine iterations
              10 # number of deformable iterations
              )
set_property(TEST itkBSplineImageRegistrationTest APPEND PROPERTY LABELS RUNS_LONG)

itk_module_test()
set(ITKHighDimensionalRegistrationMethodsTests
itkSimpleImageRegistrationTest.cxx
itkTimeVaryingVelocityFieldImageRegistrationTest.cxx
itkQuasiNewtonObjectOptimizerRegistrationTest.cxx
)

set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKHighDimensionalRegistrationMethods  "${ITKHighDimensionalRegistrationMethods-Test_LIBRARIES}" "${ITKHighDimensionalRegistrationMethodsTests}")

itk_add_test(NAME itkSimpleImageRegistrationTest
      COMMAND ITKHighDimensionalRegistrationMethodsTestDriver
      --with-threads 2
      --compare DATA{Baseline/itkSimpleImageRegistrationTest.nii.gz}
                ${TEMP}/itkSimpleImageRegistrationTest.nii.gz
              itkSimpleImageRegistrationTest
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkSimpleImageRegistrationTest.nii.gz
              100 # number of affine iterations
              10 # number of deformable iterations
              )

itk_add_test(NAME itkTimeVaryingVelocityFieldImageRegistrationTest
      COMMAND ITKHighDimensionalRegistrationMethodsTestDriver
              itkTimeVaryingVelocityFieldImageRegistrationTest
              2 # number of dimensions
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkTimeVaryingVelocityFieldImageRegistrationTest
              10  # number of optimization iterations of the displacement field
              0.5 # learning rate
              )

itk_add_test(NAME itkQuasiNewtonObjectOptimizerRegistrationTest1
      COMMAND ITKHighDimensionalRegistrationMethodsTestDriver
              itkQuasiNewtonObjectOptimizerRegistrationTest
              2 # number of dimensions
              dm # Demons metric
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkQuasiNewtonObjectOptimizerRegistrationTest1.nii.gz
              5 2 )

itk_add_test(NAME itkQuasiNewtonObjectOptimizerRegistrationTest2
      COMMAND ITKHighDimensionalRegistrationMethodsTestDriver
              itkQuasiNewtonObjectOptimizerRegistrationTest
              2 # number of dimensions
              mi # metric of joint histogram mutual information
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkQuasiNewtonObjectOptimizerRegistrationTest2.nii.gz
              5 2 )

itk_add_test(NAME itkQuasiNewtonObjectOptimizerRegistrationTest3
      COMMAND ITKHighDimensionalRegistrationMethodsTestDriver
              itkQuasiNewtonObjectOptimizerRegistrationTest
              2 # number of dimensions
              anc # metric of ants neighborhood correlation
              DATA{Input/r16slice_rigid.nii.gz}
              DATA{Input/r64slice.nii.gz}
              ${TEMP}/itkQuasiNewtonObjectOptimizerRegistrationTest3.nii.gz
              5 2 )

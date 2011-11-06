itk_module_test()
set(ITKHighDimensionalRegistrationMethodsTests
itkSimpleImageRegistrationTest.cxx
)

set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKHighDimensionalRegistrationMethods  "${ITKHighDimensionalRegistrationMethods-Test_LIBRARIES}" "${ITKHighDimensionalRegistrationMethodsTests}")

if(0)  ##HACK:  Test data not uploaded
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
endif(0)

itk_module_test()
set(ITKImageGradientTests
itkGradientImageFilterTest.cxx
itkGradientImageFilterTest2.cxx
itkVectorGradientMagnitudeImageFilterTest1.cxx
itkVectorGradientMagnitudeImageFilterTest2.cxx
itkVectorGradientMagnitudeImageFilterTest3.cxx
itkGradientMagnitudeImageFilterTest.cxx
itkGradientMagnitudeRecursiveGaussianFilterTest.cxx
itkGradientRecursiveGaussianFilterTest.cxx
itkGradientRecursiveGaussianFilterTest2.cxx
itkGradientRecursiveGaussianFilterTest3.cxx
itkGradientRecursiveGaussianFilterTest4.cxx
itkDifferenceOfGaussiansGradientTest.cxx
itkGradientRecursiveGaussianFilterSpeedTest.cxx
)

CreateTestDriver(ITKImageGradient  "${ITKImageGradient-Test_LIBRARIES}" "${ITKImageGradientTests}")

itk_add_test(NAME itkGradientImageFilterTest
      COMMAND ITKImageGradientTestDriver itkGradientImageFilterTest)
itk_add_test(NAME itkGradientImageFilterTest2
      COMMAND ITKImageGradientTestDriver
   --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/GradientMagnitudeImageFilterTest2.mha}
              ${ITK_TEST_OUTPUT_DIR}/GradientMagnitudeImageFilterTest2.mha
      itkGradientImageFilterTest2
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/GradientMagnitudeImageFilterTest2.mha )

itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest1a
      COMMAND ITKImageGradientTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1a.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png
    itkVectorGradientMagnitudeImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png 0)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest1b
      COMMAND ITKImageGradientTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1b.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
    itkVectorGradientMagnitudeImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png 1)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest2
      COMMAND ITKImageGradientTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
    itkVectorGradientMagnitudeImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw} ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png 0 5)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest2b
      COMMAND ITKImageGradientTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2b.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
    itkVectorGradientMagnitudeImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw} ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png 1 5)
itk_add_test(NAME itkGradientMagnitudeImageFilterTest
      COMMAND ITKImageGradientTestDriver itkGradientMagnitudeImageFilterTest)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest3
      COMMAND ITKImageGradientTestDriver itkVectorGradientMagnitudeImageFilterTest3
              DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw} ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest3.mha 1)
itk_add_test(NAME itkGradientMagnitudeRecursiveGaussianFilterTest
      COMMAND ITKImageGradientTestDriver itkGradientMagnitudeRecursiveGaussianFilterTest)
itk_add_test(NAME itkGradientRecursiveGaussianFilterTest
      COMMAND ITKImageGradientTestDriver itkGradientRecursiveGaussianFilterTest)
itk_add_test(NAME itkGradientRecursiveGaussianFilterTest2
      COMMAND ITKImageGradientTestDriver  itkGradientRecursiveGaussianFilterTest2)

itk_add_test(NAME itkGradientRecursiveGaussianFilterTest3
      COMMAND ITKImageGradientTestDriver
      --compare ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3a.nii.gz
                ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3b.nii.gz
      --compare ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3c.nii.gz
                ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3d.nii.gz
      --compare ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3c.nii.gz
                ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3e.nii.gz
      --compare DATA{Baseline/itkGradientRecursiveGaussianFilterTest3c.nii.gz}
                ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3c.nii.gz
      --compare ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3f.nii.gz
                ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3g.nii.gz
      itkGradientRecursiveGaussianFilterTest3
      ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3a.nii.gz
      ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3b.nii.gz
      ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3c.nii.gz
      ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3d.nii.gz
      ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3e.nii.gz
      ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3f.nii.gz
      ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest3g.nii.gz )

itk_add_test(NAME itkGradientRecursiveGaussianFilterTest4
      COMMAND ITKImageGradientTestDriver
      --compare DATA{Baseline/itkGradientRecursiveGaussianFilterTest4.mha}
                ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest4.mha
      itkGradientRecursiveGaussianFilterTest4
        DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/itkGradientRecursiveGaussianFilterTest4.mha )

itk_add_test(NAME itkDifferenceOfGaussiansGradientTest

      COMMAND ITKImageGradientTestDriver itkDifferenceOfGaussiansGradientTest)

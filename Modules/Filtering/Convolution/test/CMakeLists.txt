itk_module_test()
set(ITKConvolutionTests
  itkConvolutionImageFilterTest.cxx
  itkConvolutionImageFilterTestInt.cxx
  itkConvolutionImageFilterDeltaFunctionTest.cxx
  itkFFTConvolutionImageFilterTest.cxx
  itkFFTConvolutionImageFilterTestInt.cxx
  itkFFTConvolutionImageFilterDeltaFunctionTest.cxx
  itkNormalizedCorrelationImageFilterTest.cxx
  itkMaskedFFTNormalizedCorrelationImageFilterTest.cxx
  itkFFTNormalizedCorrelationImageFilterTest.cxx
)

CreateTestDriver(ITKConvolution  "${ITKConvolution-Test_LIBRARIES}" "${ITKConvolutionTests}")

itk_add_test(NAME itkConvolutionImageFilterTestSobelX
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTestSobelX.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTestSobelX.nrrd
    itkConvolutionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/sobel_x.nii.gz} ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTestSobelX.nrrd)
itk_add_test(NAME itkConvolutionImageFilterTestSobelY
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTestSobelY.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTestSobelY.nrrd
    itkConvolutionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/sobel_y.nii.gz} ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTestSobelY.nrrd)
itk_add_test(NAME itkConvolutionImageFilterTest4x4Mean
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest4x4Mean.png}
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x4Mean.png
    itkConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{Input/4x4-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x4Mean.png 1 SAME)
itk_add_test(NAME itkConvolutionImageFilterTest4x5Mean
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest4x5Mean.png}
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x5Mean.png
    itkConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{Input/4x5-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x5Mean.png 1 SAME)
itk_add_test(NAME itkConvolutionImageFilterTest5x5Mean
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest5x5Mean.png}
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest5x5Mean.png
    itkConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/5x5-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest5x5Mean.png 1 SAME)
itk_add_test(NAME itkConvolutionImageFilterTest4x4MeanValidRegion
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest4x4MeanValidRegion.png}
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x4MeanValidRegion.png
      itkConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{Input/4x4-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x4MeanValidRegion.png 1 VALID)
itk_add_test(NAME itkConvolutionImageFilterTest4x5MeanValidRegion
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest4x5MeanValidRegion.png}
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x5MeanValidRegion.png
      itkConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{Input/4x5-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x5MeanValidRegion.png 1 VALID)
itk_add_test(NAME itkConvolutionImageFilterTest5x5MeanValidRegion
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest5x5MeanValidRegion.png}
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest5x5MeanValidRegion.png
      itkConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/5x5-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest5x5MeanValidRegion.png 1 VALID)
itk_add_test(NAME itkConvolutionImageFilterDeltaFunctionTest
      COMMAND ITKConvolutionTestDriver
   --compare DATA{${ITK_DATA_ROOT}/Input/level.png}
             ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterDeltaFunctionTest.png
      itkConvolutionImageFilterDeltaFunctionTest DATA{${ITK_DATA_ROOT}/Input/level.png} ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterDeltaFunctionTest.png)

# FFT convolution tests
itk_add_test(NAME itkFFTConvolutionImageFilterTestSobelX
      COMMAND ITKConvolutionTestDriver
      --compare DATA{Baseline/itkFFTConvolutionImageFilterTestSobelX.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTestSobelX.nrrd
    itkFFTConvolutionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/sobel_x.nii.gz} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTestSobelX.nrrd)
itk_add_test(NAME itkFFTConvolutionImageFilterTestSobelY
      COMMAND ITKConvolutionTestDriver
      --compare DATA{Baseline/itkFFTConvolutionImageFilterTestSobelY.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTestSobelY.nrrd
    itkFFTConvolutionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/sobel_y.nii.gz} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTestSobelY.nrrd)
itk_add_test(NAME itkFFTConvolutionImageFilterTestSobelYPeriodic
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTestSobelYPeriodic.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTestSobelYPeriodic.nrrd
      itkFFTConvolutionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/sobel_y.nii.gz} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTestSobelYPeriodic.nrrd 2 0 SAME PERIODIC)
itk_add_test(NAME itkFFTConvolutionImageFilterTestSobelXConstant
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTestSobelXConstant.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTestSobelXConstant.nrrd
      itkFFTConvolutionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/sobel_x.nii.gz} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTestSobelXConstant.nrrd 2 0 SAME CONSTANT)
itk_add_test(NAME itkFFTConvolutionImageFilterTestSobelXZeroFluxNeumann
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTestSobelXZeroFluxNeumann.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTestSobelXZeroFluxNeumann.nrrd
      itkFFTConvolutionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/sobel_x.nii.gz} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTestSobelXZeroFluxNeumann.nrrd 2 0 VALID ZEROFLUXNEUMANN)
itk_add_test(NAME itkFFTConvolutionImageFilterTest4x4Mean
      COMMAND ITKConvolutionTestDriver
     --compare DATA{Baseline/itkFFTConvolutionImageFilterTest4x4Mean.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest4x4Mean.png
    itkFFTConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{Input/4x4-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest4x4Mean.png 1 SAME)
itk_add_test(NAME itkFFTConvolutionImageFilterTest4x5Mean
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest4x5Mean.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest4x5Mean.png
    itkFFTConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{Input/4x5-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest4x5Mean.png 1 SAME)
itk_add_test(NAME itkFFTConvolutionImageFilterTest5x5Mean
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest5x5Mean.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest5x5Mean.png
    itkFFTConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/5x5-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest5x5Mean.png 1 SAME)
itk_add_test(NAME itkFFTConvolutionImageFilterTest4x4MeanValidRegion
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkFFTConvolutionImageFilterTest4x4MeanValidRegion.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest4x4MeanValidRegion.png
      itkFFTConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{Input/4x4-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest4x4MeanValidRegion.png 1 VALID)
itk_add_test(NAME itkFFTConvolutionImageFilterTest4x5MeanValidRegion
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest4x5MeanValidRegion.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest4x5MeanValidRegion.png
      itkFFTConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{Input/4x5-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest4x5MeanValidRegion.png 1 VALID)
itk_add_test(NAME itkFFTConvolutionImageFilterTest5x5MeanValidRegion
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest5x5MeanValidRegion.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest5x5MeanValidRegion.png
      itkFFTConvolutionImageFilterTestInt DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/5x5-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterTest5x5MeanValidRegion.png 1 VALID)
itk_add_test(NAME itkFFTConvolutionImageFilterDeltaFunctionTest
      COMMAND ITKConvolutionTestDriver
   --compare DATA{${ITK_DATA_ROOT}/Input/level.png}
             ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterDeltaFunctionTest.png
      itkFFTConvolutionImageFilterDeltaFunctionTest DATA{${ITK_DATA_ROOT}/Input/level.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTConvolutionImageFilterDeltaFunctionTest.png 5)

# NCC tests
itk_add_test(NAME itkNormalizedCorrelationImageFilterTest
      COMMAND ITKConvolutionTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/NormalizedCorrelationImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/NormalizedCorrelationImageFilterTest.png
    itkNormalizedCorrelationImageFilterTest DATA{${ITK_DATA_ROOT}/Input/sf4.png} DATA{${ITK_DATA_ROOT}/Input/circle.png} ${ITK_TEST_OUTPUT_DIR}/NormalizedCorrelationImageFilterTest.png)

# Masked FFT NCC tests
# Test with three different shapes
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest1
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest1.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest1.png 0.1 DATA{Input/FixedRectangleMask1.png} DATA{Input/MovingRectanglesMask.png})
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest2
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest2.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest2.png 0.1 DATA{Input/FixedRectangleMask2.png} DATA{Input/MovingRectanglesMask.png})
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest3
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest3.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest3.png 0.1 DATA{Input/FixedRectangleMask3.png} DATA{Input/MovingRectanglesMask.png})
# Test with different size images
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest4
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest4.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest4.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectanglesCropped.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest4.png 0.1 DATA{Input/FixedRectangleMask3.png})
# Test with not passing the fixed mask or moving mask.
# Also test with requiring no minimum overlapping pixels. This leads to more noise on the borders.
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest5
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest5.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest5.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest5.png 0)
# Test with not passing the moving mask. This should be the same as Test3 since the moving mask was set to all ones.
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest6
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest6.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest6.png 0.1 DATA{Input/FixedRectangleMask3.png})

# Standard FFT NCC tests
# Test with three different shapes.
# These are the shapes from FixedRectangles.png used in the MaskedFFTNormalizedCorrelation.
# In this case, FixedRectangles has first been masked with the three masks used.
# Thus, the output of masking first and then correlation can be directly compared with masked correlation.
# It is clear from the resulting images that these results are different and that masked correlation
# outputs the correct results.
itk_add_test(NAME itkFFTNormalizedCorrelationImageFilterTest1
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkFFTNormalizedCorrelationImageFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTNormalizedCorrelationImageFilterTest1.png
    itkFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangle1.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTNormalizedCorrelationImageFilterTest1.png 0.1)
itk_add_test(NAME itkFFTNormalizedCorrelationImageFilterTest2
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkFFTNormalizedCorrelationImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTNormalizedCorrelationImageFilterTest2.png
    itkFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangle2.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTNormalizedCorrelationImageFilterTest2.png 0.1)
itk_add_test(NAME itkFFTNormalizedCorrelationImageFilterTest3
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkFFTNormalizedCorrelationImageFilterTest3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTNormalizedCorrelationImageFilterTest3.png
    itkFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangle3.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTNormalizedCorrelationImageFilterTest3.png 0.1)
# Test with different size images
itk_add_test(NAME itkFFTNormalizedCorrelationImageFilterTest4
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkFFTNormalizedCorrelationImageFilterTest4.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTNormalizedCorrelationImageFilterTest4.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangle3.png} DATA{Input/MovingRectanglesCropped.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTNormalizedCorrelationImageFilterTest4.png 0.1)
# Test NCC of the original fixed and moving images, without masking the fixed image first.
# The result should be the same as Test5 of MaskedFFTNormalizedCorrelation
itk_add_test(NAME itkFFTNormalizedCorrelationImageFilterTest5
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest5.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTNormalizedCorrelationImageFilterTest5.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTNormalizedCorrelationImageFilterTest5.png 0)

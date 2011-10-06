itk_module_test()
set(ITKConvolutionTests
  itkConvolutionImageFilterTest.cxx
  itkConvolutionImageFilterTestInt.cxx
  itkConvolutionImageFilterDeltaFunctionTest.cxx
  itkMaskedFFTNormalizedCorrelationImageFilterTest.cxx
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

# Masked FFT NCC tests
# Test with three different shapes
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest1
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest1.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest1.png 20 DATA{Input/FixedRectangleMask1.png} DATA{Input/MovingRectanglesMask.png})
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest2
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest2.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest2.png 20 DATA{Input/FixedRectangleMask2.png} DATA{Input/MovingRectanglesMask.png})
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest3
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest3.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest3.png 20 DATA{Input/FixedRectangleMask3.png} DATA{Input/MovingRectanglesMask.png})
# Test with different size images
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest4
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest4.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest4.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectanglesCropped.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest4.png 20 DATA{Input/FixedRectangleMask3.png})
# Test with not passing the moving mask.  This should be the same as Test3 since the moving mask was set to all ones.
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest5
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest5.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest5.png 20 DATA{Input/FixedRectangleMask3.png})
# Test with not passing the fixed mask or moving mask.
itk_add_test(NAME itkMaskedFFTNormalizedCorrelationImageFilterTest6
      COMMAND ITKConvolutionTestDriver
    --compare DATA{Baseline/itkMaskedFFTNormalizedCorrelationImageFilterTest6.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest6.png
    itkMaskedFFTNormalizedCorrelationImageFilterTest DATA{Input/FixedRectangles.png} DATA{Input/MovingRectangles.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedFFTNormalizedCorrelationImageFilterTest6.png 400)

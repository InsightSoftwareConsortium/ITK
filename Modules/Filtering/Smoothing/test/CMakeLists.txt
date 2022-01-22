itk_module_test()
set(ITKSmoothingTests
itkBoxMeanImageFilterTest.cxx
itkBoxSigmaImageFilterTest.cxx
itkDiscreteGaussianImageFilterTest2.cxx
itkFFTDiscreteGaussianImageFilterTest.cxx
itkFFTDiscreteGaussianImageFilterFactoryTest.cxx
itkSmoothingRecursiveGaussianImageFilterTest.cxx
itkSmoothingRecursiveGaussianImageFilterOnVectorImageTest.cxx
itkSmoothingRecursiveGaussianImageFilterOnImageOfVectorTest.cxx
itkSmoothingRecursiveGaussianImageFilterOnImageAdaptorTest.cxx
itkMeanImageFilterTest.cxx
itkDiscreteGaussianImageFilterTest.cxx
itkMedianImageFilterTest.cxx
itkRecursiveGaussianImageFiltersOnTensorsTest.cxx
itkRecursiveGaussianImageFiltersOnVectorImageTest.cxx
itkRecursiveGaussianImageFiltersTest.cxx
itkRecursiveGaussianScaleSpaceTest1.cxx
)

CreateTestDriver(ITKSmoothing  "${ITKSmoothing-Test_LIBRARIES}" "${ITKSmoothingTests}")
itk_add_test(NAME itkBoxMeanImageFilterTest3
      COMMAND ITKSmoothingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkBoxMeanImageFilter3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBoxMeanImageFilter3.png
    itkBoxMeanImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBoxMeanImageFilter3.png 3)
itk_add_test(NAME itkBoxMeanImageFilterTest10
      COMMAND ITKSmoothingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkBoxMeanImageFilter10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBoxMeanImageFilter10.png
    itkBoxMeanImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBoxMeanImageFilter10.png 10)
itk_add_test(NAME itkBoxSigmaImageFilterTest3
      COMMAND ITKSmoothingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkBoxSigmaImageFilter3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBoxSigmaImageFilter3.png
    itkBoxSigmaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBoxSigmaImageFilter3.png 3)
itk_add_test(NAME itkBoxSigmaImageFilterTest10
      COMMAND ITKSmoothingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkBoxSigmaImageFilter10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBoxSigmaImageFilter10.png
    itkBoxSigmaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBoxSigmaImageFilter10.png 10)
itk_add_test(NAME itkDiscreteGaussianImageFilterTest2
      COMMAND ITKSmoothingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/DiscreteGaussianImageFilterTest2_OutputA.mha}
              ${ITK_TEST_OUTPUT_DIR}/DiscreteGaussianImageFilterTest2_OutputA.mha
    itkDiscreteGaussianImageFilterTest2 2 3
    DATA{${ITK_DATA_ROOT}/Input/RGBTestImage.tif}
    ${ITK_TEST_OUTPUT_DIR}/DiscreteGaussianImageFilterTest2_OutputA.mha
    1.870829)
itk_add_test(NAME itkSmoothingRecursiveGaussianImageFilterTestNormalizeScalesOn
      COMMAND ITKSmoothingTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkSmoothingRecursiveGaussianImageFilterTestNormalizeScalesOn.png
                  58ec36acf622fc9c4567f0edd89d2ffa
    itkSmoothingRecursiveGaussianImageFilterTest DATA{${ITK_DATA_ROOT}/Input/sf4.png} ${ITK_TEST_OUTPUT_DIR}/itkSmoothingRecursiveGaussianImageFilterTestNormalizeScalesOn.png 1 2.5)
itk_add_test(NAME itkSmoothingRecursiveGaussianImageFilterOnVectorImageTest
      COMMAND ITKSmoothingTestDriver itkSmoothingRecursiveGaussianImageFilterOnVectorImageTest)
itk_add_test(NAME itkSmoothingRecursiveGaussianImageFilterOnImageOfVectorTest
      COMMAND ITKSmoothingTestDriver itkSmoothingRecursiveGaussianImageFilterOnImageOfVectorTest)
itk_add_test(NAME itkSmoothingRecursiveGaussianImageFilterOnImageAdaptorTest
      COMMAND ITKSmoothingTestDriver itkSmoothingRecursiveGaussianImageFilterOnImageOfVectorTest)
itk_add_test(NAME itkMeanImageFilterTest
      COMMAND ITKSmoothingTestDriver itkMeanImageFilterTest)
itk_add_test(NAME itkDiscreteGaussianImageFilterTest1a
      COMMAND ITKSmoothingTestDriver itkDiscreteGaussianImageFilterTest 1)
itk_add_test(NAME itkDiscreteGaussianImageFilterTest1b
      COMMAND ITKSmoothingTestDriver itkDiscreteGaussianImageFilterTest 0)

# Use equivalent input parameters to compare standard and FFT
# procedures to a common baseline for equivalent output
itk_add_test(NAME itkDiscreteGaussianImageFilterComparisonTest
      COMMAND ITKSmoothingTestDriver
      --compare DATA{Baseline/DiscreteGaussianImageFilterComparisonTestOutput.mha}
                ${ITK_TEST_OUTPUT_DIR}/DiscreteGaussianImageFilterComparisonTestOutput.mha
      itkDiscreteGaussianImageFilterTest2 2 1
        DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/DiscreteGaussianImageFilterComparisonTestOutput.mha
        3.5 0.05 35)
itk_add_test(NAME itkFFTDiscreteGaussianImageFilterComparisonTest
      COMMAND ITKSmoothingTestDriver
      --compare DATA{Baseline/DiscreteGaussianImageFilterComparisonTestOutput.mha}
                ${ITK_TEST_OUTPUT_DIR}/FFTDiscreteGaussianImageFilterComparisonTestOutput.mha
      itkFFTDiscreteGaussianImageFilterTest
        DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/FFTDiscreteGaussianImageFilterComparisonTestOutput.mha
        3.5 0.05 35 2 0)

itk_add_test(NAME itkFFTDiscreteGaussianImageFilterImageSourceTest
      COMMAND ITKSmoothingTestDriver
      --compare DATA{Baseline/FFTDiscreteGaussianImageFilterTestImageSourceOutput.mha}
                ${ITK_TEST_OUTPUT_DIR}/FFTDiscreteGaussianImageFilterTestImageSourceOutput.mha
      itkFFTDiscreteGaussianImageFilterTest
        DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/FFTDiscreteGaussianImageFilterTestImageSourceOutput.mha
        3.5 0.05 35 2 1)
itk_add_test(NAME itkFFTDiscreteGaussianImageFilterKernelDimensionalityTest
      COMMAND ITKSmoothingTestDriver
      --compare DATA{Baseline/FFTDiscreteGaussianImageFilterTestKernelDimensionalityOutput.mha}
                ${ITK_TEST_OUTPUT_DIR}/FFTDiscreteGaussianImageFilterTestKernelDimensionalityOutput.mha
      itkFFTDiscreteGaussianImageFilterTest
        DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/FFTDiscreteGaussianImageFilterTestKernelDimensionalityOutput.mha
        1.2 0.25 11 1 0)

itk_add_test(NAME itkFFTDiscreteGaussianImageFilterFactoryTest
      COMMAND ITKSmoothingTestDriver itkFFTDiscreteGaussianImageFilterFactoryTest)
itk_add_test(NAME itkMedianImageFilterTest
      COMMAND ITKSmoothingTestDriver itkMedianImageFilterTest)
itk_add_test(NAME itkRecursiveGaussianImageFiltersOnTensorsTest
      COMMAND ITKSmoothingTestDriver itkRecursiveGaussianImageFiltersOnTensorsTest)
itk_add_test(NAME itkRecursiveGaussianImageFiltersOnVectorImageTest
      COMMAND ITKSmoothingTestDriver itkRecursiveGaussianImageFiltersOnVectorImageTest)
itk_add_test(NAME itkRecursiveGaussianImageFiltersTest
      COMMAND ITKSmoothingTestDriver itkRecursiveGaussianImageFiltersTest)
itk_add_test(NAME itkRecursiveGaussianScaleSpaceTest1
      COMMAND ITKSmoothingTestDriver
              itkRecursiveGaussianScaleSpaceTest1)

set(ITKSmoothingGTests
      itkMeanImageFilterGTest.cxx
      itkMedianImageFilterGTest.cxx
)
CreateGoogleTestDriver(ITKSmoothing "${ITKSmoothing-Test_LIBRARIES}" "${ITKSmoothingGTests}")

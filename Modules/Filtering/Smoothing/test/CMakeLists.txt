itk_module_test()
set(ITKSmoothingTests
itkBoxMeanImageFilterTest.cxx
itkBoxSigmaImageFilterTest.cxx
itkDiscreteGaussianImageFilterTest2.cxx
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
    itkDiscreteGaussianImageFilterTest2 2 3 DATA{${ITK_DATA_ROOT}/Input/RGBTestImage.tif} 3.5 ${ITK_TEST_OUTPUT_DIR}/DiscreteGaussianImageFilterTest2_OutputA.mha)
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
itk_add_test(NAME itkDiscreteGaussianImageFilterTest
      COMMAND ITKSmoothingTestDriver itkDiscreteGaussianImageFilterTest)
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

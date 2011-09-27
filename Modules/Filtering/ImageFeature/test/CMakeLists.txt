itk_module_test()
set(ITKImageFeatureTests
itkZeroCrossingBasedEdgeDetectionImageFilterTest.cxx
itkLaplacianImageFilterTest.cxx
itkSobelEdgeDetectionImageFilterTest.cxx
itkHessian3DToVesselnessMeasureImageFilterTest.cxx
itkHessianRecursiveGaussianFilterScaleSpaceTest.cxx
itkHessianRecursiveGaussianFilterTest.cxx
itkHoughTransform2DCirclesImageTest.cxx
itkHoughTransform2DLinesImageTest.cxx
itkCannyEdgeDetectionImageFilterTest.cxx
itkBilateralImageFilterTest.cxx
itkBilateralImageFilterTest2.cxx
itkBilateralImageFilterTest3.cxx
itkGradientVectorFlowImageFilterTest.cxx
itkSimpleContourExtractorImageFilterTest.cxx
itkZeroCrossingImageFilterTest.cxx
itkCannyEdgeDetectionImageFilterTest2.cxx
itkDerivativeImageFilterTest.cxx
itkLaplacianRecursiveGaussianImageFilterTest.cxx
)

CreateTestDriver(ITKImageFeature  "${ITKImageFeature-Test_LIBRARIES}" "${ITKImageFeatureTests}")

itk_add_test(NAME itkZeroCrossingBasedEdgeDetectionImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkZeroCrossingBasedEdgeDetectionImageFilterTest)
itk_add_test(NAME itkLaplacianImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkLaplacianImageFilterTest)
itk_add_test(NAME itkSobelEdgeDetectionImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkSobelEdgeDetectionImageFilterTest)
itk_add_test(NAME itkHessian3DToVesselnessMeasureImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkHessian3DToVesselnessMeasureImageFilterTest)
itk_add_test(NAME itkHessianRecursiveGaussianFilterScaleSpaceTest
      COMMAND ITKImageFeatureTestDriver itkHessianRecursiveGaussianFilterScaleSpaceTest)
itk_add_test(NAME itkHessianRecursiveGaussianFilterTest
      COMMAND ITKImageFeatureTestDriver itkHessianRecursiveGaussianFilterTest)
itk_add_test(NAME itkHoughTransform2DCirclesImageTest
      COMMAND ITKImageFeatureTestDriver itkHoughTransform2DCirclesImageTest)
itk_add_test(NAME itkHoughTransform2DLinesImageTest
      COMMAND ITKImageFeatureTestDriver itkHoughTransform2DLinesImageTest)
itk_add_test(NAME itkCannyEdgeDetectionImageFilterTest
      COMMAND ITKImageFeatureTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkCannyEdgeDetectionImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest.png
    itkCannyEdgeDetectionImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest.png)
itk_add_test(NAME itkBilateralImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkBilateralImageFilterTest)
itk_add_test(NAME itkBilateralImageFilterTest2
      COMMAND ITKImageFeatureTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BilateralImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest2.png
    itkBilateralImageFilterTest2 ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest2.png)
itk_add_test(NAME itkBilateralImageFilterTest3
      COMMAND ITKImageFeatureTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BilateralImageFilterTest3.png
              ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest3.png
    itkBilateralImageFilterTest3 ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest3.png)
itk_add_test(NAME itkGradientVectorFlowImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkGradientVectorFlowImageFilterTest)
itk_add_test(NAME itkSimpleContourExtractorImageFilterTest
      COMMAND ITKImageFeatureTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/SimpleContourExtractorImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/SimpleContourExtractorImageFilterTest.png
    itkSimpleContourExtractorImageFilterTest ${ITK_DATA_ROOT}/Baseline/BasicFilters/BinaryThresholdImageFilterTest2.png ${ITK_TEST_OUTPUT_DIR}/SimpleContourExtractorImageFilterTest.png)
itk_add_test(NAME itkZeroCrossingImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkZeroCrossingImageFilterTest)
itk_add_test(NAME itkCannyEdgeDetectionImageFilterTest2
      COMMAND ITKImageFeatureTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest2_A.png
              ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest2_B.png
    itkCannyEdgeDetectionImageFilterTest2 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest2_A.png ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest2_B.png)
itk_add_test(NAME itkDerivativeImageFilterTest1x
      COMMAND ITKImageFeatureTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDerivativeImageFilterTest1x.png
              ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest1x.png
    itkDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest1x.png 1 0)
itk_add_test(NAME itkDerivativeImageFilterTest1y
      COMMAND ITKImageFeatureTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDerivativeImageFilterTest1y.png
              ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest1y.png
    itkDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest1y.png 1 1)
itk_add_test(NAME itkDerivativeImageFilterTest2x
      COMMAND ITKImageFeatureTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDerivativeImageFilterTest2x.png
              ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2x.png
    itkDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2x.png 2 0)
itk_add_test(NAME itkDerivativeImageFilterTest2y
      COMMAND ITKImageFeatureTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDerivativeImageFilterTest2y.png
              ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2y.png
    itkDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2y.png 2 1)
itk_add_test(NAME itkLaplacianRecursiveGaussianImageFilterTest
      COMMAND ITKImageFeatureTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/LaplacianRecursiveGaussianImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png
    itkLaplacianRecursiveGaussianImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png)

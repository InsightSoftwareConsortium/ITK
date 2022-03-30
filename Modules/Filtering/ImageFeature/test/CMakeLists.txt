itk_module_test()
set(ITKImageFeatureTests
itkZeroCrossingBasedEdgeDetectionImageFilterTest.cxx
itkLaplacianImageFilterTest.cxx
itkSobelEdgeDetectionImageFilterTest.cxx
itkHessian3DToVesselnessMeasureImageFilterTest.cxx
itkHessianToObjectnessMeasureImageFilterTest.cxx
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
itkLaplacianSharpeningImageFilterTest.cxx
itkMaskFeaturePointSelectionFilterTest.cxx
itkUnsharpMaskImageFilterTestSimple.cxx
itkUnsharpMaskImageFilterTest.cxx
itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest.cxx
itkDiscreteGaussianDerivativeImageFilterTest.cxx
itkMultiScaleHessianBasedMeasureImageFilterTest.cxx
)

CreateTestDriver(ITKImageFeature  "${ITKImageFeature-Test_LIBRARIES}" "${ITKImageFeatureTests}")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkZeroCrossingBasedEdgeDetectionImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkZeroCrossingBasedEdgeDetectionImageFilterTest 1.0 0.01 0 1)
itk_add_test(NAME itkLaplacianImageFilterTest
      COMMAND ITKImageFeatureTestDriver
      --compare DATA{Baseline/itkLaplacianImageFilterTestBaseline.png}
                ${ITK_TEST_OUTPUT_DIR}/itkLaplacianImageFilterTest.png
                itkLaplacianImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkLaplacianImageFilterTest.png
      0)
itk_add_test(NAME itkLaplacianImageFilterWithSpacingTest
      COMMAND ITKImageFeatureTestDriver
      --compare DATA{Baseline/itkLaplacianImageFilterWithSpacingTestBaseline.png}
                ${ITK_TEST_OUTPUT_DIR}/itkLaplacianImageFilterWithSpacingTest.png
                itkLaplacianImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkLaplacianImageFilterWithSpacingTest.png
      1)
itk_add_test(NAME itkSobelEdgeDetectionImageFilterTest
      COMMAND ITKImageFeatureTestDriver
      --compare DATA{Baseline/itkSobelEdgeDetectionImageFilterTestBaseline.png}
                ${ITK_TEST_OUTPUT_DIR}/itkSobelEdgeDetectionImageFilterTest.png
      itkSobelEdgeDetectionImageFilterTest
      DATA{Input/Swirled.png} ${ITK_TEST_OUTPUT_DIR}/itkSobelEdgeDetectionImageFilterTest.png)
itk_add_test(NAME itkUnsharpMaskImageFilterTestSimple
      COMMAND ITKImageFeatureTestDriver itkUnsharpMaskImageFilterTestSimple)
itk_add_test(NAME itkUnsharpMaskImageFilterTest_DefaultsUChar
      COMMAND ITKImageFeatureTestDriver
  --compare DATA{Baseline/SwirledUSM_Defaults.png}
            ${TEMP}/SwirledUSM_Defaults.png
            --compareIntensityTolerance .001
  itkUnsharpMaskImageFilterTest
  uchar
  DATA{Input/Swirled.png}
  ${TEMP}/SwirledUSM_Defaults.png
  )
itk_add_test(NAME itkUnsharpMaskImageFilterTest_DefaultsFloat
      COMMAND ITKImageFeatureTestDriver
  --compare DATA{Baseline/SwirledUSM_Defaults.nrrd}
            ${TEMP}/SwirledUSM_Defaults.nrrd
            --compareIntensityTolerance .001
  itkUnsharpMaskImageFilterTest
  float
  DATA{Input/Swirled.png}
  ${TEMP}/SwirledUSM_Defaults.nrrd
  )
itk_add_test(NAME itkUnsharpMaskImageFilterTest_3_3_15
      COMMAND ITKImageFeatureTestDriver
  --compare DATA{Baseline/SwirledUSM_3-3-15.nrrd}
            ${TEMP}/SwirledUSM_3-3-15.nrrd.nrrd
            --compareIntensityTolerance .001
  itkUnsharpMaskImageFilterTest
  float
  DATA{Input/Swirled.png}
  ${TEMP}/SwirledUSM_3-3-15.nrrd.nrrd
  3.0 3.0 15)
itk_add_test(NAME itkHessian3DToVesselnessMeasureImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkHessian3DToVesselnessMeasureImageFilterTest 0.5 0.5 2.0)
itk_add_test(NAME itkHessianToObjectnessMeasureImageFilterTest
      COMMAND ITKImageFeatureTestDriver
  --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkHessianToObjectnessMeasureImageFilterTest.mha}
            ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput.mha
            --compareIntensityTolerance .01
  itkHessianToObjectnessMeasureImageFilterTest
  DATA{${ITK_DATA_ROOT}/Input/DSA.png}
  ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput.mha
  1 0)
itk_add_test(NAME itkHessianToObjectnessMeasureImageFilterTest2
      COMMAND ITKImageFeatureTestDriver
  --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkHessianToObjectnessMeasureImageFilterTest2.mha}
            ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput2.mha
            --compareIntensityTolerance .01
  itkHessianToObjectnessMeasureImageFilterTest
  DATA{${ITK_DATA_ROOT}/Input/DSA.png}
  ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput2.mha
  0 0)
itk_add_test(NAME itkHessianRecursiveGaussianFilterScaleSpaceTest
      COMMAND ITKImageFeatureTestDriver itkHessianRecursiveGaussianFilterScaleSpaceTest)

itk_add_test(NAME itkHessianRecursiveGaussianFilterTest
      COMMAND ITKImageFeatureTestDriver --redirectOutput ${TEMP}/itkHessianRecursiveGaussianFilterTest.txt
          itkHessianRecursiveGaussianFilterTest 2.5 0)
set_tests_properties(itkHessianRecursiveGaussianFilterTest PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkHessianRecursiveGaussianFilterTest.txt)

itk_add_test(NAME itkHoughTransform2DCirclesImageTest
      COMMAND ITKImageFeatureTestDriver itkHoughTransform2DCirclesImageTest)
itk_add_test(NAME itkHoughTransform2DLinesImageTest
      COMMAND ITKImageFeatureTestDriver itkHoughTransform2DLinesImageTest)
itk_add_test(NAME itkCannyEdgeDetectionImageFilterTest
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkCannyEdgeDetectionImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest.png
    itkCannyEdgeDetectionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest.png)
itk_add_test(NAME itkBilateralImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkBilateralImageFilterTest)
itk_add_test(NAME itkBilateralImageFilterTest2
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/BilateralImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest2.png
    itkBilateralImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/cake_easy.png} ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest2.png)
itk_add_test(NAME itkBilateralImageFilterTest3
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/BilateralImageFilterTest3.png}
              ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest3.png
    itkBilateralImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/cake_easy.png} ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest3.png)
itk_add_test(NAME itkGradientVectorFlowImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkGradientVectorFlowImageFilterTest)
itk_add_test(NAME itkSimpleContourExtractorImageFilterTest
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/SimpleContourExtractorImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/SimpleContourExtractorImageFilterTest.png
    itkSimpleContourExtractorImageFilterTest DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/BinaryThresholdImageFilterTest2.png} ${ITK_TEST_OUTPUT_DIR}/SimpleContourExtractorImageFilterTest.png)
itk_add_test(NAME itkZeroCrossingImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkZeroCrossingImageFilterTest)
itk_add_test(NAME itkCannyEdgeDetectionImageFilterTest2
      COMMAND ITKImageFeatureTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest2_A.png
              ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest2_B.png
    itkCannyEdgeDetectionImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest2_A.png ${ITK_TEST_OUTPUT_DIR}/itkCannyEdgeDetectionImageFilterTest2_B.png)
itk_add_test(NAME itkDerivativeImageFilterTest1x
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDerivativeImageFilterTest1x.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest1x.png
    itkDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest1x.png 1 0 1)
itk_add_test(NAME itkDerivativeImageFilterTest1y
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDerivativeImageFilterTest1y.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest1y.png
    itkDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest1y.png 1 1 1)
itk_add_test(NAME itkDerivativeImageFilterTest2x
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDerivativeImageFilterTest2x.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2x.png
    itkDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2x.png 2 0 1)
itk_add_test(NAME itkDerivativeImageFilterTest2y
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDerivativeImageFilterTest2y.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2y.png
    itkDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2y.png 2 1 1)
itk_add_test(NAME itkLaplacianRecursiveGaussianImageFilterTest
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/LaplacianRecursiveGaussianImageFilterTest.png,:}
              ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png
    itkLaplacianRecursiveGaussianImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png)
itk_add_test(NAME itkLaplacianSharpeningImageFilterTest
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{Baseline/itkLaplacianSharpeningImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLaplacianSharpeningImageFilterTest.png
    itkLaplacianSharpeningImageFilterTest DATA{${ITK_DATA_ROOT}/Input/sf4.png} ${ITK_TEST_OUTPUT_DIR}/itkLaplacianSharpeningImageFilterTest.png 1)
itk_add_test(NAME itkMaskFeaturePointSelectionFilterTest
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{Baseline/itkMaskFeaturePointSelectionFilterTest.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskFeaturePointSelectionFilterTest.mha
    itkMaskFeaturePointSelectionFilterTest
DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR}/itkMaskFeaturePointSelectionFilterTest.mha 0 2 0 0.01)
itk_add_test(NAME itkMaskFeaturePointSelectionFilterTestTensors
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{Baseline/itkMaskFeaturePointSelectionFilterTestTensors.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskFeaturePointSelectionFilterTestTensors.mha
    itkMaskFeaturePointSelectionFilterTest
DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR}/itkMaskFeaturePointSelectionFilterTestTensors.mha 0 2 1 0.01)
itk_add_test(NAME itkMaskFeaturePointSelectionFilterTestMask
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{Baseline/itkMaskFeaturePointSelectionFilterTestMask.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskFeaturePointSelectionFilterTestMask.mha
    itkMaskFeaturePointSelectionFilterTest
DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR}/itkMaskFeaturePointSelectionFilterTestMask.mha 0 2 1 0.01 DATA{Input/HeadMRVolumeMaskImage.mha})
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest
      COMMAND ITKImageFeatureTestDriver itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest01
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest01.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest01.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest01.png 0 1 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest10
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest10.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest10.png 1 0 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest11
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest11.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest11.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest11.png 1 1 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest20
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest20.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest20.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest20.png 2 0 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest02
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest02.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest02.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest02.png 0 2 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest21
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest21.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest21.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest21.png 2 1 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest12
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest12.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest12.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest12.png 1 2 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest30
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest30.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest30.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest30.png 3 0 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest03
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest03.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest03.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest03.png 0 3 3.0)
itk_add_test(NAME itkMultiScaleHessianBasedMeasureImageFilterTest
      COMMAND ITKImageFeatureTestDriver
          --compare DATA{Baseline/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput.mha
              itkMultiScaleHessianBasedMeasureImageFilterTest DATA{${ITK_DATA_ROOT}/Input/DSA.png} ${ITK_TEST_OUTPUT_DIR}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput.mha ${ITK_TEST_OUTPUT_DIR}/itkMultiScaleHessianBasedMeasureImageFilterTestScalesOutput.mha 5 10 10 1 0 ${ITK_TEST_OUTPUT_DIR}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput2.mha)

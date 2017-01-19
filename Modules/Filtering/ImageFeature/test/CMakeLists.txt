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
itkMaskFeaturePointSelectionFilterTest.cxx
itkUnsharpMaskImageFilterTestSimple.cxx
itkUnsharpMaskImageFilterTest.cxx
)

CreateTestDriver(ITKImageFeature  "${ITKImageFeature-Test_LIBRARIES}" "${ITKImageFeatureTests}")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkZeroCrossingBasedEdgeDetectionImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkZeroCrossingBasedEdgeDetectionImageFilterTest)
itk_add_test(NAME itkLaplacianImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkLaplacianImageFilterTest)
itk_add_test(NAME itkSobelEdgeDetectionImageFilterTest
      COMMAND ITKImageFeatureTestDriver itkSobelEdgeDetectionImageFilterTest)
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
      COMMAND ITKImageFeatureTestDriver itkHessian3DToVesselnessMeasureImageFilterTest)
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
          itkHessianRecursiveGaussianFilterTest)
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
    itkDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest1x.png 1 0)
itk_add_test(NAME itkDerivativeImageFilterTest1y
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDerivativeImageFilterTest1y.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest1y.png
    itkDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest1y.png 1 1)
itk_add_test(NAME itkDerivativeImageFilterTest2x
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDerivativeImageFilterTest2x.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2x.png
    itkDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2x.png 2 0)
itk_add_test(NAME itkDerivativeImageFilterTest2y
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDerivativeImageFilterTest2y.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2y.png
    itkDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDerivativeImageFilterTest2y.png 2 1)
itk_add_test(NAME itkLaplacianRecursiveGaussianImageFilterTest
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/LaplacianRecursiveGaussianImageFilterTest.png,:}
              ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png
    itkLaplacianRecursiveGaussianImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png)
itk_add_test(NAME itkMaskFeaturePointSelectionFilterTest
      COMMAND ITKImageFeatureTestDriver
    --compare DATA{Baseline/itkMaskFeaturePointSelectionFilterTest.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskFeaturePointSelectionFilterTest.mha
    itkMaskFeaturePointSelectionFilterTest
DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR}/itkMaskFeaturePointSelectionFilterTest.mha)

itk_python_add_test(NAME itkGradientVectorFlowImageFilterPythonTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/itkGradientVectorFlowImageFilterPythonTestX.png
                DATA{Baseline/itkGradientVectorFlowImageFilterPythonTestX.png}
      --compare ${ITK_TEST_OUTPUT_DIR}/itkGradientVectorFlowImageFilterPythonTestY.png
                DATA{Baseline/itkGradientVectorFlowImageFilterPythonTestY.png}
    COMMAND itkGradientVectorFlowImageFilterTest.py
      DATA{Input/Contours.png}
      ${ITK_TEST_OUTPUT_DIR}/itkGradientVectorFlowImageFilterPythonTestX.png
      ${ITK_TEST_OUTPUT_DIR}/itkGradientVectorFlowImageFilterPythonTestY.png
      50 50000.0 0.001)

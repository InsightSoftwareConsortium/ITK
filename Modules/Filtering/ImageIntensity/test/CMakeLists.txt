itk_module_test()
set(ITK-ImageIntensityTests
itkCosImageFilterAndAdaptorTest.cxx
itkExpImageFilterAndAdaptorTest.cxx
itkVectorRescaleIntensityImageFilterTest.cxx
itkTernaryMagnitudeSquaredImageFilterTest.cxx
itkMinimumImageFilterTest.cxx
itkLog10ImageFilterAndAdaptorTest.cxx
itkVectorIndexSelectionCastImageFilterTest.cxx
itkInvertIntensityImageFilterTest.cxx
itkSymmetricEigenAnalysisImageFilterTest.cxx
itkEdgePotentialImageFilterTest.cxx
itkComplexToModulusFilterAndAdaptorTest.cxx
itkAddImageAdaptorTest.cxx
itkAndImageFilterTest.cxx
itkAdaptImageFilterTest2.cxx
itkLogImageFilterAndAdaptorTest.cxx
itkNotImageFilterTest.cxx
itkImageIntensityHeaderTest.cxx
itkImageAdaptorNthElementTest.cxx
itkConstrainedValueAdditionImageFilterTest.cxx
itkAtanImageFilterAndAdaptorTest.cxx
itkMaskNegatedImageFilterTest.cxx
itkAddImageFilterTest.cxx
itkMultiplyImageFilterTest.cxx
itkWeightedAddImageFilterTest.cxx
itkRescaleIntensityImageFilterTest.cxx
itkNormalizeImageFilterTest.cxx
itkNaryAddImageFilterTest.cxx
itkShiftScaleImageFilterTest.cxx
itkComplexToPhaseFilterAndAdaptorTest.cxx
itkIntensityWindowingImageFilterTest.cxx
itkTernaryMagnitudeImageFilterTest.cxx
itkAbsImageFilterAndAdaptorTest.cxx
itkJoinSeriesImageFilterTest.cxx
itkMaximumImageFilterTest.cxx
itkBinaryMagnitudeImageFilterTest.cxx
itkMatrixIndexSelectionImageFilterTest.cxx
itkSquareImageFilterTest.cxx
itkRGBToVectorAdaptImageFilterTest.cxx
itkComplexToRealFilterAndAdaptorTest.cxx
itkNaryMaximumImageFilterTest.cxx
itkAtan2ImageFilterTest.cxx
itkSqrtImageFilterAndAdaptorTest.cxx
itkAsinImageFilterAndAdaptorTest.cxx
itkMaskImageFilterTest.cxx
itkHistogramMatchingImageFilterTest.cxx
itkAcosImageFilterAndAdaptorTest.cxx
itkExpNegativeImageFilterAndAdaptorTest.cxx
itkTanImageFilterAndAdaptorTest.cxx
itkSigmoidImageFilterTest.cxx
itkDivideImageFilterTest.cxx
itkDivideImageFilterTest2.cxx
itkVectorExpandImageFilterTest.cxx
itkAdaptImageFilterTest.cxx
itkOrImageFilterTest.cxx
itkRGBToLuminanceImageFilterAndAdaptorTest.cxx
itkJoinImageFilterTest.cxx
itkXorImageFilterTest.cxx
itkSubtractImageFilterTest.cxx
itkComplexToImaginaryFilterAndAdaptorTest.cxx
itkVectorToRGBImageAdaptorTest.cxx
itkSinImageFilterAndAdaptorTest.cxx
itkPolylineMask2DImageFilterTest.cxx
itkPolylineMaskImageFilterTest.cxx
itkModulusImageFilterTest.cxx
)

CreateTestDriver(ITK-ImageIntensity  "${ITK-ImageIntensity-Test_LIBRARIES}" "${ITK-ImageIntensityTests}")

add_test(NAME itkImageIntensityHeaderTest
      COMMAND ITK-ImageIntensityTestDriver itkImageIntensityHeaderTest)
add_test(NAME itkCosImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkCosImageFilterAndAdaptorTest)
add_test(NAME itkVectorRescaleIntensityImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkVectorRescaleIntensityImageFilterTest)
add_test(NAME itkTernaryMagnitudeSquaredImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkTernaryMagnitudeSquaredImageFilterTest)
add_test(NAME itkMinimumImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkMinimumImageFilterTest)
add_test(NAME itkLog10ImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkLog10ImageFilterAndAdaptorTest)
add_test(NAME itkVectorIndexSelectionCastImageFilterTest1
      COMMAND ITK-ImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkVectorIndexSelectionCastImageFilterTest1.mha
              ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest1.mha
    itkVectorIndexSelectionCastImageFilterTest ${ITK_DATA_ROOT}/Input/RGBTestImage.tif ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest1.mha 0)
add_test(NAME itkVectorIndexSelectionCastImageFilterTest2
      COMMAND ITK-ImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkVectorIndexSelectionCastImageFilterTest2.mha
              ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest2.mha
    itkVectorIndexSelectionCastImageFilterTest ${ITK_DATA_ROOT}/Input/RGBTestImage.tif ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest2.mha 1)
add_test(NAME itkVectorIndexSelectionCastImageFilterTest3
      COMMAND ITK-ImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkVectorIndexSelectionCastImageFilterTest3.mha
              ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest3.mha
    itkVectorIndexSelectionCastImageFilterTest ${ITK_DATA_ROOT}/Input/RGBTestImage.tif ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest3.mha 2)
add_test(NAME itkInvertIntensityImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/InvertIntensityImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/InvertIntensityImageFilterTest.png
    itkInvertIntensityImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/InvertIntensityImageFilterTest.png)
add_test(NAME itkSymmetricEigenAnalysisImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkSymmetricEigenAnalysisImageFilterTest)
add_test(NAME itkEdgePotentialImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkEdgePotentialImageFilterTest)
add_test(NAME itkComplexToModulusFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkComplexToModulusFilterAndAdaptorTest)
add_test(NAME itkAddImageAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkAddImageAdaptorTest)
add_test(NAME itkAndImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkAndImageFilterTest)
add_test(NAME itkAdaptImageFilterTest2
      COMMAND ITK-ImageIntensityTestDriver itkAdaptImageFilterTest2)
add_test(NAME itkLogImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkLogImageFilterAndAdaptorTest)
add_test(NAME itkNotImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkNotImageFilterTest)
add_test(NAME itkImageAdaptorNthElementTest
      COMMAND ITK-ImageIntensityTestDriver itkImageAdaptorNthElementTest)
add_test(NAME itkConstrainedValueAdditionImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkConstrainedValueAdditionImageFilterTest)
add_test(NAME itkAtanImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkAtanImageFilterAndAdaptorTest)
add_test(NAME itkMaskNegatedImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkMaskNegatedImageFilterTest)
add_test(NAME itkAddImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkAddImageFilterTest)
add_test(NAME itkMultiplyImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkMultiplyImageFilterTest)
add_test(NAME itkWeightedAddImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkWeightedAddImageFilterTest)
add_test(NAME itkRescaleIntensityImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkRescaleIntensityImageFilterTest)
add_test(NAME itkNormalizeImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkNormalizeImageFilterTest)
add_test(NAME itkNaryAddImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkNaryAddImageFilterTest)
add_test(NAME itkShiftScaleImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkShiftScaleImageFilterTest)
add_test(NAME itkComplexToPhaseFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkComplexToPhaseFilterAndAdaptorTest)
add_test(NAME itkIntensityWindowingImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkIntensityWindowingImageFilterTest)
add_test(NAME itkTernaryMagnitudeImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkTernaryMagnitudeImageFilterTest)
add_test(NAME itkAbsImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkAbsImageFilterAndAdaptorTest)
add_test(NAME itkJoinSeriesImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkJoinSeriesImageFilterTest)
add_test(NAME itkMaximumImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkMaximumImageFilterTest)
add_test(NAME itkBinaryMagnitudeImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkBinaryMagnitudeImageFilterTest)
add_test(NAME itkMatrixIndexSelectionImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MatrixIndexSelectionImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/MatrixIndexSelectionImageFilterTest.png
    itkMatrixIndexSelectionImageFilterTest ${ITK_TEST_OUTPUT_DIR}/MatrixIndexSelectionImageFilterTest.png)
add_test(NAME itkSquareImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkSquareImageFilterTest)
add_test(NAME itkRGBToVectorAdaptImageFilterTest.cxx
      COMMAND ITK-ImageIntensityTestDriver itkRGBToVectorAdaptImageFilterTest)
add_test(NAME itkComplexToRealFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkComplexToRealFilterAndAdaptorTest)
add_test(NAME itkNaryMaximumImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkNaryMaximumImageFilterTest)
add_test(NAME itkAtan2ImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkAtan2ImageFilterTest)
add_test(NAME itkSqrtImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkSqrtImageFilterAndAdaptorTest)
add_test(NAME itkAsinImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkAsinImageFilterAndAdaptorTest)
add_test(NAME itkMaskImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkMaskImageFilterTest)
add_test(NAME itkHistogramMatchingImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkHistogramMatchingImageFilterTest)
add_test(NAME itkAcosImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkAcosImageFilterAndAdaptorTest)
add_test(NAME itkExpNegativeImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkExpNegativeImageFilterAndAdaptorTest)
add_test(NAME itkExpImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkExpImageFilterAndAdaptorTest)
add_test(NAME itkTanImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkTanImageFilterAndAdaptorTest)
add_test(NAME itkSigmoidImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkSigmoidImageFilterTest)
add_test(NAME itkDivideImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkDivideImageFilterTest)
add_test(NAME itkDivideImageFilterTest2
      COMMAND ITK-ImageIntensityTestDriver itkDivideImageFilterTest2)
add_test(NAME itkVectorExpandImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkVectorExpandImageFilterTest)
add_test(NAME itkAdaptImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkAdaptImageFilterTest)
add_test(NAME itkOrImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkOrImageFilterTest)
add_test(NAME itkRGBToLuminanceImageFilterAndAdaptorTest.cxx
      COMMAND ITK-ImageIntensityTestDriver itkRGBToLuminanceImageFilterAndAdaptorTest)
add_test(NAME itkJoinImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkJoinImageFilterTest)
add_test(NAME itkXorImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkXorImageFilterTest)
add_test(NAME itkSubtractImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkSubtractImageFilterTest)
add_test(NAME itkComplexToImaginaryFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkComplexToImaginaryFilterAndAdaptorTest)
add_test(NAME itkVectorToRGBImageAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkVectorToRGBImageAdaptorTest)
add_test(NAME itkSinImageFilterAndAdaptorTest
      COMMAND ITK-ImageIntensityTestDriver itkSinImageFilterAndAdaptorTest)
add_test(NAME itkPolylineMask2DImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkPolylineMask2DImageFilterTest
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkPolylineMask2DImageFilterTest.png)
add_test(NAME itkPolylineMaskImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver itkPolylineMaskImageFilterTest)
add_test(NAME itkModulusImageFilterTest
      COMMAND ITK-ImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ModulusImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png
    itkModulusImageFilterTest ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png)

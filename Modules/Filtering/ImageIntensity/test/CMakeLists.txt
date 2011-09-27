itk_module_test()
set(ITKImageIntensityTests
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
itkImageAdaptorNthElementTest.cxx
itkConstrainedValueAdditionImageFilterTest.cxx
itkAtanImageFilterAndAdaptorTest.cxx
itkMaskNegatedImageFilterTest.cxx
itkAddImageFilterTest.cxx
itkAddImageFilterFrameTest.cxx
itkPowImageFilterTest.cxx
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
itkXorImageFilterTest.cxx
itkSubtractImageFilterTest.cxx
itkComplexToImaginaryFilterAndAdaptorTest.cxx
itkVectorToRGBImageAdaptorTest.cxx
itkSinImageFilterAndAdaptorTest.cxx
itkPolylineMask2DImageFilterTest.cxx
itkPolylineMaskImageFilterTest.cxx
itkPromoteDimensionImageTest.cxx
itkModulusImageFilterTest.cxx
itkVectorMagnitudeImageFilterTest.cxx
itkNormalizeToConstantImageFilterTest.cxx
)

CreateTestDriver(ITKImageIntensity  "${ITKImageIntensity-Test_LIBRARIES}" "${ITKImageIntensityTests}")

itk_add_test(NAME itkCosImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkCosImageFilterAndAdaptorTest)
itk_add_test(NAME itkVectorRescaleIntensityImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkVectorRescaleIntensityImageFilterTest)
itk_add_test(NAME itkTernaryMagnitudeSquaredImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkTernaryMagnitudeSquaredImageFilterTest)
itk_add_test(NAME itkMinimumImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkMinimumImageFilterTest)
itk_add_test(NAME itkLog10ImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkLog10ImageFilterAndAdaptorTest)
itk_add_test(NAME itkVectorIndexSelectionCastImageFilterTest1
      COMMAND ITKImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkVectorIndexSelectionCastImageFilterTest1.mha
              ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest1.mha
    itkVectorIndexSelectionCastImageFilterTest ${ITK_DATA_ROOT}/Input/RGBTestImage.tif ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest1.mha 0)
itk_add_test(NAME itkVectorIndexSelectionCastImageFilterTest2
      COMMAND ITKImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkVectorIndexSelectionCastImageFilterTest2.mha
              ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest2.mha
    itkVectorIndexSelectionCastImageFilterTest ${ITK_DATA_ROOT}/Input/RGBTestImage.tif ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest2.mha 1)
itk_add_test(NAME itkVectorIndexSelectionCastImageFilterTest3
      COMMAND ITKImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkVectorIndexSelectionCastImageFilterTest3.mha
              ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest3.mha
    itkVectorIndexSelectionCastImageFilterTest ${ITK_DATA_ROOT}/Input/RGBTestImage.tif ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest3.mha 2)
itk_add_test(NAME itkInvertIntensityImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/InvertIntensityImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/InvertIntensityImageFilterTest.png
    itkInvertIntensityImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/InvertIntensityImageFilterTest.png)
itk_add_test(NAME itkSymmetricEigenAnalysisImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkSymmetricEigenAnalysisImageFilterTest)
itk_add_test(NAME itkEdgePotentialImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkEdgePotentialImageFilterTest)
itk_add_test(NAME itkComplexToModulusFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkComplexToModulusFilterAndAdaptorTest)
itk_add_test(NAME itkAddImageAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkAddImageAdaptorTest)
itk_add_test(NAME itkAndImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkAndImageFilterTest)
itk_add_test(NAME itkAdaptImageFilterTest2
      COMMAND ITKImageIntensityTestDriver itkAdaptImageFilterTest2)
itk_add_test(NAME itkLogImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkLogImageFilterAndAdaptorTest)
itk_add_test(NAME itkNotImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkNotImageFilterTest)
itk_add_test(NAME itkImageAdaptorNthElementTest
      COMMAND ITKImageIntensityTestDriver itkImageAdaptorNthElementTest)
itk_add_test(NAME itkConstrainedValueAdditionImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkConstrainedValueAdditionImageFilterTest)
itk_add_test(NAME itkAtanImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkAtanImageFilterAndAdaptorTest)
itk_add_test(NAME itkMaskNegatedImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkMaskNegatedImageFilterTest)
itk_add_test(NAME itkAddImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkAddImageFilterTest)
itk_add_test(NAME itkAddImageFilterFrameTest
      COMMAND ITKImageIntensityTestDriver itkAddImageFilterFrameTest)
itk_add_test(NAME itkPowImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkPowImageFilterTest)
itk_add_test(NAME itkMultiplyImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkMultiplyImageFilterTest)
itk_add_test(NAME itkWeightedAddImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkWeightedAddImageFilterTest)
itk_add_test(NAME itkRescaleIntensityImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkRescaleIntensityImageFilterTest)
itk_add_test(NAME itkNormalizeImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkNormalizeImageFilterTest)
itk_add_test(NAME itkNaryAddImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkNaryAddImageFilterTest)
itk_add_test(NAME itkShiftScaleImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkShiftScaleImageFilterTest)
itk_add_test(NAME itkComplexToPhaseFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkComplexToPhaseFilterAndAdaptorTest)
itk_add_test(NAME itkIntensityWindowingImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkIntensityWindowingImageFilterTest)
itk_add_test(NAME itkTernaryMagnitudeImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkTernaryMagnitudeImageFilterTest)
itk_add_test(NAME itkAbsImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkAbsImageFilterAndAdaptorTest)
itk_add_test(NAME itkMaximumImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkMaximumImageFilterTest)
itk_add_test(NAME itkBinaryMagnitudeImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkBinaryMagnitudeImageFilterTest)
itk_add_test(NAME itkMatrixIndexSelectionImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MatrixIndexSelectionImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/MatrixIndexSelectionImageFilterTest.png
    itkMatrixIndexSelectionImageFilterTest ${ITK_TEST_OUTPUT_DIR}/MatrixIndexSelectionImageFilterTest.png)
itk_add_test(NAME itkSquareImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkSquareImageFilterTest)
itk_add_test(NAME itkRGBToVectorAdaptImageFilterTest.cxx
      COMMAND ITKImageIntensityTestDriver itkRGBToVectorAdaptImageFilterTest)
itk_add_test(NAME itkComplexToRealFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkComplexToRealFilterAndAdaptorTest)
itk_add_test(NAME itkNaryMaximumImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkNaryMaximumImageFilterTest)
itk_add_test(NAME itkAtan2ImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkAtan2ImageFilterTest)
itk_add_test(NAME itkSqrtImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkSqrtImageFilterAndAdaptorTest)
itk_add_test(NAME itkAsinImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkAsinImageFilterAndAdaptorTest)
itk_add_test(NAME itkMaskImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkMaskImageFilterTest)
itk_add_test(NAME itkHistogramMatchingImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkHistogramMatchingImageFilterTest)
itk_add_test(NAME itkAcosImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkAcosImageFilterAndAdaptorTest)
itk_add_test(NAME itkExpNegativeImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkExpNegativeImageFilterAndAdaptorTest)
itk_add_test(NAME itkExpImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkExpImageFilterAndAdaptorTest)
itk_add_test(NAME itkTanImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkTanImageFilterAndAdaptorTest)
itk_add_test(NAME itkSigmoidImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkSigmoidImageFilterTest)
itk_add_test(NAME itkDivideImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkDivideImageFilterTest)
itk_add_test(NAME itkDivideImageFilterTest2
      COMMAND ITKImageIntensityTestDriver itkDivideImageFilterTest2)
itk_add_test(NAME itkVectorExpandImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkVectorExpandImageFilterTest)
itk_add_test(NAME itkAdaptImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkAdaptImageFilterTest)
itk_add_test(NAME itkOrImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkOrImageFilterTest)
itk_add_test(NAME itkRGBToLuminanceImageFilterAndAdaptorTest.cxx
      COMMAND ITKImageIntensityTestDriver itkRGBToLuminanceImageFilterAndAdaptorTest)
itk_add_test(NAME itkXorImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkXorImageFilterTest)
itk_add_test(NAME itkSubtractImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkSubtractImageFilterTest)
itk_add_test(NAME itkComplexToImaginaryFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkComplexToImaginaryFilterAndAdaptorTest)
itk_add_test(NAME itkVectorToRGBImageAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkVectorToRGBImageAdaptorTest)
itk_add_test(NAME itkSinImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkSinImageFilterAndAdaptorTest)
itk_add_test(NAME itkPolylineMask2DImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkPolylineMask2DImageFilterTest
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkPolylineMask2DImageFilterTest.png)
itk_add_test(NAME itkPolylineMaskImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkPolylineMaskImageFilterTest)
itk_add_test(NAME itkPromoteDimensionImageTest
      COMMAND ITKImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PromoteDimensionImageTest.mhd
              ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd
    itkPromoteDimensionImageTest ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd)
itk_add_test(NAME itkModulusImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ModulusImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png
    itkModulusImageFilterTest ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png)
itk_add_test(NAME itkVectorMagnitudeImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkVectorMagnitudeImageFilterTest)
itk_add_test(NAME itkNormalizeToConstantImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkNormalizeToConstantImageFilterTest)

itk_module_test()
set(ITKImageIntensityTests
itkCosImageFilterAndAdaptorTest.cxx
itkExpImageFilterAndAdaptorTest.cxx
itkVectorRescaleIntensityImageFilterTest.cxx
itkTernaryMagnitudeSquaredImageFilterTest.cxx
itkTernaryOperatorImageFilterTest.cxx
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
itkAddImageFilterTest2.cxx
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
itkDivideOrZeroOutImageFilterTest.cxx
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
itkEqualTest.cxx
itkNotEqualTest.cxx
itkGreaterEqualTest.cxx
itkGreaterTest.cxx
itkLessEqualTest.cxx
itkLessTest.cxx
itkClampImageFilterTest.cxx
itkNthElementPixelAccessorTest2.cxx
itkMagnitudeAndPhaseToComplexImageFilterTest.cxx
)

# Disable optimization on the tests below to avoid possible
# optimization bugs in some compilers. This option will appear on the
# command line after any other optimization flag (including those from
# CMAKE_CXX_FLAGS) which means that on gcc, MSVC, and the Intel
# compiler, it will override any other optimization flag.
if (DEFINED ITK_CXX_DISABLE_OPTIMIZATION_FLAG)
  set_source_files_properties(
    itkClampImageFilterTest.cxx
    PROPERTIES COMPILE_FLAGS ${ITK_CXX_DISABLE_OPTIMIZATION_FLAG}
  )
endif()

CreateTestDriver(ITKImageIntensity  "${ITKImageIntensity-Test_LIBRARIES}" "${ITKImageIntensityTests}")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkCosImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkCosImageFilterAndAdaptorTest)
itk_add_test(NAME itkVectorRescaleIntensityImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkVectorRescaleIntensityImageFilterTest)
itk_add_test(NAME itkTernaryMagnitudeSquaredImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkTernaryMagnitudeSquaredImageFilterTest)
itk_add_test(NAME itkTernaryOperatorImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkTernaryOperatorImageFilterTest)
itk_add_test(NAME itkMinimumImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkMinimumImageFilterTest)
itk_add_test(NAME itkLog10ImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkLog10ImageFilterAndAdaptorTest)
itk_add_test(NAME itkVectorIndexSelectionCastImageFilterTest1
      COMMAND ITKImageIntensityTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkVectorIndexSelectionCastImageFilterTest1.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest1.mha
    itkVectorIndexSelectionCastImageFilterTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImage.tif} ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest1.mha 0)
itk_add_test(NAME itkVectorIndexSelectionCastImageFilterTest2
      COMMAND ITKImageIntensityTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkVectorIndexSelectionCastImageFilterTest2.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest2.mha
    itkVectorIndexSelectionCastImageFilterTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImage.tif} ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest2.mha 1)
itk_add_test(NAME itkVectorIndexSelectionCastImageFilterTest3
      COMMAND ITKImageIntensityTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkVectorIndexSelectionCastImageFilterTest3.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest3.mha
    itkVectorIndexSelectionCastImageFilterTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImage.tif} ${ITK_TEST_OUTPUT_DIR}/itkVectorIndexSelectionCastImageFilterTest3.mha 2)
itk_add_test(NAME itkInvertIntensityImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/InvertIntensityImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/InvertIntensityImageFilterTest.png
    itkInvertIntensityImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/InvertIntensityImageFilterTest.png)
itk_add_test(NAME itkSymmetricEigenAnalysisImageFilterTestOrderByValue
      COMMAND ITKImageIntensityTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkSymmetricEigenAnalysisImageFilterTestOrderByValue.png
              941cdbc107bea093a564a83ed5993d6d
    itkSymmetricEigenAnalysisImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkSymmetricEigenAnalysisImageFilterTestOrderByValue.png 1)
itk_add_test(NAME itkSymmetricEigenAnalysisImageFilterTestOrderByMagnitude
      COMMAND ITKImageIntensityTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkSymmetricEigenAnalysisImageFilterTestOrderByMagnitude.png
              dbff47eae29f458759de12b3dee95176
    itkSymmetricEigenAnalysisImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkSymmetricEigenAnalysisImageFilterTestOrderByMagnitude.png 2)
itk_add_test(NAME itkSymmetricEigenAnalysisImageFilterTestDoNotOrder
      COMMAND ITKImageIntensityTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkSymmetricEigenAnalysisImageFilterTestDoNotOrder.png
              dbff47eae29f458759de12b3dee95176
    itkSymmetricEigenAnalysisImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkSymmetricEigenAnalysisImageFilterTestDoNotOrder.png 3)
itk_add_test(NAME itkEdgePotentialImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkEdgePotentialImageFilterTest)
itk_add_test(NAME itkComplexToModulusFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkComplexToModulusFilterAndAdaptorTest)
itk_add_test(NAME itkAddImageAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkAddImageAdaptorTest)
itk_add_test(NAME itkAndImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkAndImageFilterTest.png
              93d46ee3d5e0c157e2b2d27977ae4d0f
    itkAndImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkAndImageFilterTest.png)
itk_add_test(NAME itkAdaptImageFilterTest2
      COMMAND ITKImageIntensityTestDriver itkAdaptImageFilterTest2)
itk_add_test(NAME itkLogImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkLogImageFilterAndAdaptorTest)
itk_add_test(NAME itkNotImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkNotImageFilterTest)
itk_add_test(NAME itkImageAdaptorNthElementTest
      COMMAND ITKImageIntensityTestDriver itkImageAdaptorNthElementTest)
itk_add_test(NAME itkConstrainedValueAdditionImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkConstrainedValueAdditionImageFilterTest.png
              6ef609d089e93e2eed8f089f75bc67fb
    itkConstrainedValueAdditionImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkConstrainedValueAdditionImageFilterTest.png)
itk_add_test(NAME itkAtanImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkAtanImageFilterAndAdaptorTest)
itk_add_test(NAME itkMaskNegatedImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkMaskNegatedImageFilterTest)
itk_add_test(NAME itkAddImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkAddImageFilterTest)
itk_add_test(NAME itkAddImageFilterTest2
      COMMAND ITKImageIntensityTestDriver itkAddImageFilterTest2
      DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${TEMP}/itkAddImageFilterTest2.mha)
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
      COMMAND ITKImageIntensityTestDriver --redirectOutput ${TEMP}/itkNormalizeImageFilterTest.txt
      itkNormalizeImageFilterTest)
set_tests_properties(itkNormalizeImageFilterTest PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkNormalizeImageFilterTest.txt)

itk_add_test(NAME itkNaryAddImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkNaryAddImageFilterTest)
itk_add_test(NAME itkShiftScaleImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkShiftScaleImageFilterTest)
itk_add_test(NAME itkComplexToPhaseFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkComplexToPhaseFilterAndAdaptorTest)
itk_add_test(NAME itkIntensityWindowingImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkIntensityWindowingImageFilterTest)
itk_add_test(NAME itkTernaryMagnitudeImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkTernaryMagnitudeImageFilterTest.png
              7f199e1363c4b754eb03cf4d0ba8d343
    itkTernaryMagnitudeImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkTernaryMagnitudeImageFilterTest.png)
itk_add_test(NAME itkAbsImageFilterAndAdaptorTest
      COMMAND ITKImageIntensityTestDriver itkAbsImageFilterAndAdaptorTest)
itk_add_test(NAME itkMaximumImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkMaximumImageFilterTest)
itk_add_test(NAME itkBinaryMagnitudeImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkBinaryMagnitudeImageFilterTest)
itk_add_test(NAME itkMatrixIndexSelectionImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/MatrixIndexSelectionImageFilterTest.png}
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
itk_add_test(NAME itkDivideOrZeroOutImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkDivideOrZeroOutImageFilterTest)
itk_add_test(NAME itkVectorExpandImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkVectorExpandImageFilterTest)
itk_add_test(NAME itkAdaptImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkAdaptImageFilterTest)
itk_add_test(NAME itkOrImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkOrImageFilterTest.png
              817d6d96c1c07cb12bfc200134aa57ba
    itkOrImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkOrImageFilterTest.png)
itk_add_test(NAME itkRGBToLuminanceImageFilterAndAdaptorTest.cxx
      COMMAND ITKImageIntensityTestDriver itkRGBToLuminanceImageFilterAndAdaptorTest)
itk_add_test(NAME itkXorImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkXorImageFilterTest.png
              3b5b9852567ef7618aac7f5f2d74ef74
    itkXorImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkXorImageFilterTest.png)
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
              DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkPolylineMask2DImageFilterTest.png)
itk_add_test(NAME itkPolylineMaskImageFilterTest
      COMMAND ITKImageIntensityTestDriver
      --compare DATA{Baseline/PolylineMaskImageFilterTest.mha}
                ${ITK_TEST_OUTPUT_DIR}/PolylineMaskImageFilterTest.mha
      itkPolylineMaskImageFilterTest ${ITK_TEST_OUTPUT_DIR}/PolylineMaskImageFilterTest.mha)
itk_add_test(NAME itkPromoteDimensionImageTest
      COMMAND ITKImageIntensityTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/PromoteDimensionImageTest.mhd,PromoteDimensionImageTest.raw}
              ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd
    itkPromoteDimensionImageTest DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd)
itk_add_test(NAME itkModulusImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ModulusImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png
    itkModulusImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Spots.png} ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png)
itk_add_test(NAME itkVectorMagnitudeImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkVectorMagnitudeImageFilterTest)
itk_add_test(NAME itkNormalizeToConstantImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkNormalizeToConstantImageFilterTest)
itk_add_test(NAME itkEqualTest
      COMMAND ITKImageIntensityTestDriver itkEqualTest)
itk_add_test(NAME itkNotEqualTest
      COMMAND ITKImageIntensityTestDriver itkNotEqualTest)
itk_add_test(NAME itkGreaterEqualTest
      COMMAND ITKImageIntensityTestDriver itkGreaterEqualTest)
itk_add_test(NAME itkGreaterTest
      COMMAND ITKImageIntensityTestDriver itkGreaterTest)
itk_add_test(NAME itkLessEqualTest
      COMMAND ITKImageIntensityTestDriver itkLessEqualTest)
itk_add_test(NAME itkLessTest
      COMMAND ITKImageIntensityTestDriver itkLessTest)
itk_add_test(NAME itkClampImageFilterTest
      COMMAND ITKImageIntensityTestDriver itkClampImageFilterTest)
itk_add_test(NAME itkNthElementPixelAccessorTest2
      COMMAND ITKImageIntensityTestDriver itkNthElementPixelAccessorTest2)
itk_add_test(NAME itkMagnitudeAndPhaseToComplexImageFilterTest
      COMMAND ITKImageIntensityTestDriver
    --compare
      DATA{Baseline/itkMagnitudeAndPhaseToComplexImageFilterTestBaseline.mha}
      ${ITK_TEST_OUTPUT_DIR}/itkMagnitudeAndPhaseToComplexImageFilterTest.mha
    itkMagnitudeAndPhaseToComplexImageFilterTest
      DATA{Input/itkBrainSliceComplexMagnitude.mha}
      DATA{Input/itkBrainSliceComplexPhase.mha}
      ${ITK_TEST_OUTPUT_DIR}/itkMagnitudeAndPhaseToComplexImageFilterTest.mha )


set(ITKImageIntensityGTests
  itkBitwiseOpsFunctorsTest.cxx
  itkArithmeticOpsFunctorsTest.cxx
)

if(MSVC)
  # disable false warning about floating division by zero
  # Floating division by zero is defined unlike integer division
  set_source_files_properties(
    itkArithmeticOpsFunctorsTest.cxx
    PROPERTIES COMPILE_FLAGS  -wd4723)
endif()

CreateGoogleTestDriver(ITKImageIntensity "${ITKImageIntensity-Test_LIBRARIES}" "${ITKImageIntensityGTests}")

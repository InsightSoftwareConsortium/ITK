itk_module_test()
set(Libraries ${IsotropicWavelets-Test_LIBRARIES})

set(
  IsotropicWaveletsTests
  itkFrequencyImageRegionIteratorWithIndexTest.cxx
  itkIsotropicWaveletFrequencyFunctionTest.cxx
  itkWaveletFrequencyFilterBankGeneratorTest.cxx
  itkRieszFrequencyFilterBankGeneratorTest.cxx
  itkFrequencyExpandTest.cxx
  itkFrequencyShrinkTest.cxx
  itkFrequencyShrinkMultiLevelTest.cxx
  itkFrequencyExpandAndShrinkTest.cxx
  itkExpandWithZerosImageFilterTest.cxx
  itkDecimateImageFilterTest.cxx
  itkInd2SubTest.cxx
  itkWaveletFrequencyForwardTest.cxx
  itkWaveletFrequencyInverseTest.cxx
  itkMonogenicSignalFrequencyImageFilterTest.cxx
  itkMonogenicPhaseAnalysisSoftThresholdImageFilterTest.cxx
  itkMonogenicPhaseAnalysisEigenValuesImageFilterTest.cxx
  itkVectorInverseFFTImageFilterTest.cxx
  itkRieszWaveletPhaseAnalysisTest.cxx
  itkFFTPadPositiveIndexImageFilterTest.cxx
  itkZeroDCImageFilterTest.cxx
)

if(ITKVtkGlue_ENABLED)
  find_package(VTK REQUIRED)
  include_directories(SYSTEM ${VTK_INCLUDE_DIRS})
  list(APPEND Libraries ${VTK_LIBRARIES})
endif()

#ITK_VISUALIZE_TESTS is passed to source code.
#change it to ON to visualize images, for dev purposes.
option(
  ITK_VISUALIZE_TESTS
  "Visualize images in tests for development purposes. Require Module_ITKVtkGlue."
  OFF
)
mark_as_advanced(ITK_VISUALIZE_TESTS)
if(ITK_VISUALIZE_TESTS)
  list(APPEND IsotropicWaveletsTests runViewImage.cxx)
endif()

createtestdriver(IsotropicWavelets "${Libraries}" "${IsotropicWaveletsTests}")

# use #ifdef ITK_VISUALIZE_TESTS, instead of checking value with #ifdef ITK_VISUALIZE_TESTS
if(ITK_VISUALIZE_TESTS)
  target_compile_definitions(
    IsotropicWaveletsTestDriver
    PRIVATE
      ITK_VISUALIZE_TESTS
  )
endif()
# NOTE: If ITK_VISUALIZE_TESTS = ON, require Module_ITKVtkGlue = ON
if(ITK_VISUALIZE_TESTS AND NOT ITKVtkGlue_ENABLED)
  message(
    FATAL_ERROR
    "ITK_VISUALIZE_TESTS is set, but Module_ITKVtkGlue is not enabled. Set Module_ITKVtkGlue = ON or set ITK_VISUALIZE_TESTS = OFF to avoid this error."
  )
endif()

# Wavelet Generator
itk_add_test(
  NAME itkWaveletFrequencyFilterBankGeneratorTest1
  COMMAND
    IsotropicWaveletsTestDriver
    itkWaveletFrequencyFilterBankGeneratorTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyFilterBankGeneratorTest1.tiff
    1
    "Held"
)
itk_add_test(
  NAME itkWaveletFrequencyFilterBankGeneratorTest2
  COMMAND
    IsotropicWaveletsTestDriver
    itkWaveletFrequencyFilterBankGeneratorTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyFilterBankGeneratorTest2.tiff
    2
    "Held"
)
# 2D:
itk_add_test(
  NAME itkWaveletFrequencyFilterBankGeneratorTest2D1
  COMMAND
    IsotropicWaveletsTestDriver
    itkWaveletFrequencyFilterBankGeneratorTest
    DATA{Input/checkershadow_gray_540x420.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyFilterBankGeneratorTest2D1.tiff
    1
    "Held"
    2
)
itk_add_test(
  NAME itkWaveletFrequencyFilterBankGeneratorTest2D2
  COMMAND
    IsotropicWaveletsTestDriver
    itkWaveletFrequencyFilterBankGeneratorTest
    DATA{Input/checkershadow_gray_540x420.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyFilterBankGeneratorTest2D2.tiff
    2
    "Held"
    2
)
## Riesz Related
# RieszGenerator
itk_add_test(
  NAME itkRieszFrequencyFilterBankGeneratorTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszFrequencyFilterBankGeneratorTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkRieszFrequencyFilterBankGeneratorTest.tiff
)
# Monogenic Analysis
itk_add_test(
  NAME itkMonogenicSignalFrequencyImageFilterTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkMonogenicSignalFrequencyImageFilterTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkMonogenicSignalFrequencyImageFilterTest.tiff
)

itk_add_test(
  NAME itkMonogenicPhaseAnalysisSoftThresholdImageFilterTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkMonogenicPhaseAnalysisSoftThresholdImageFilterTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkMonogenicPhaseAnalysisSoftThresholdImageFilterTest.tiff
)
#EigenValues
itk_add_test(
  NAME itkMonogenicPhaseAnalysisEigenValuesImageFilterTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkMonogenicPhaseAnalysisEigenValuesImageFilterTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkMonogenicPhaseAnalysisEigenValuesImageFilterTest.tiff
)

itk_add_test(
  NAME itkMonogenicPhaseAnalysisEigenValuesImageFilterTest2D
  COMMAND
    IsotropicWaveletsTestDriver
    itkMonogenicPhaseAnalysisEigenValuesImageFilterTest
    DATA{Input/checkershadow_gray_540x420.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkMonogenicPhaseAnalysisEigenValuesImageFilterTest2D.tiff
    2
)
# VectorInverseFFT
itk_add_test(
  NAME itkVectorInverseFFTImageFilterTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkVectorInverseFFTImageFilterTest
    DATA{Input/collagen_32x32x16.tiff}
)
#Wavelet Forward
itk_add_test(
  NAME itkWaveletFrequencyForwardTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkWaveletFrequencyForwardTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyForwardTest.tiff
    1
    1
    "Vow"
)

itk_add_test(
  NAME itkWaveletFrequencyForwardTest2D
  COMMAND
    IsotropicWaveletsTestDriver
    itkWaveletFrequencyForwardTest
    DATA{Input/checkershadow_gray_540x420.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyForwardTest2D.tiff
    1
    1
    "Held"
    2
)

#Wavelet Inverse
itk_add_test(
  NAME itkWaveletFrequencyInverseTest
  COMMAND
    IsotropicWaveletsTestDriver
    --compare
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest.tiff
    itkWaveletFrequencyInverseTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest.tiff
    1
    1
    "Held"
)

itk_add_test(
  NAME itkWaveletFrequencyInverseTestMultiLevelMultiBand
  COMMAND
    IsotropicWaveletsTestDriver
    --compare
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTestMultiLevelMultiBand.tiff
    itkWaveletFrequencyInverseTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTestMultiLevelMultiBand.tiff
    2
    5
    "Held"
)
#2D
itk_add_test(
  NAME itkWaveletFrequencyInverseTest2D
  COMMAND
    IsotropicWaveletsTestDriver
    --compare
    DATA{Input/checkershadow_gray_540x420.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest2D.tiff
    itkWaveletFrequencyInverseTest
    DATA{Input/checkershadow_gray_540x420.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest2D.tiff
    1
    1
    "Held"
    2
)

itk_add_test(
  NAME itkWaveletFrequencyInverseTest2DMultiLevelMultiBand
  COMMAND
    IsotropicWaveletsTestDriver
    --compare
    DATA{Input/checkershadow_Lch_512x512.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest2DMultiLevelMultiBand.tiff
    itkWaveletFrequencyInverseTest
    DATA{Input/checkershadow_Lch_512x512.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest2DMultiLevelMultiBand.tiff
    5
    5
    "Held"
    2
)
# RieszWavelet Phase Analysis
set(DefaultWavelet Held)
itk_add_test(
  NAME itkRieszWaveletPhaseAnalysisTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszWaveletPhaseAnalysisTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkRieszWaveletPhaseAnalysisTest.tiff
    1
    1
    ${DefaultWavelet}
)
itk_add_test(
  NAME itkRieszWaveletPhaseAnalysisTestMultiLevelMultiBand
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszWaveletPhaseAnalysisTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkRieszWaveletPhaseAnalysisTestMultiLevelMultiBand.tiff
    2
    5
    ${DefaultWavelet}
)
###
itk_add_test(
  NAME itkRieszWaveletPhaseAnalysisTest2D
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszWaveletPhaseAnalysisTest
    DATA{Input/checkershadow_gray_540x420.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkRieszWaveletPhaseAnalysisTest2D.tiff
    1
    1
    ${DefaultWavelet}
    2
)
itk_add_test(
  NAME itkRieszWaveletPhaseAnalysisTest2DMultiLevelMultiBand
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszWaveletPhaseAnalysisTest
    DATA{Input/checkershadow_Lch_512x512.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkRieszWaveletPhaseAnalysisTest2DMultiLevelMultiBand.tiff
    5
    5
    ${DefaultWavelet}
    2
)
#IsotropicWaveletFrequencyFunctionTest
itk_add_test(
  NAME itkIsotropicWaveletFrequencyFunctionTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkIsotropicWaveletFrequencyFunctionTest
    ${ITK_TEST_OUTPUT_DIR}/profileMotherWavelet
    "unused"
    5
    "Held"
    3
)
# FrequencyIterator
itk_add_test(
  NAME itkFrequencyImageRegionIteratorWithIndexTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkFrequencyImageRegionIteratorWithIndexTest
)
##Ind2Sub
itk_add_test(
  NAME itkInd2SubTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkInd2SubTest
)
## ExpandWithZeros (in spatial domain)
itk_add_test(
  NAME itkExpandWithZerosImageFilterTest2D
  COMMAND
    IsotropicWaveletsTestDriver
    itkExpandWithZerosImageFilterTest
    2
)
itk_add_test(
  NAME itkExpandWithZerosImageFilterTest3D
  COMMAND
    IsotropicWaveletsTestDriver
    itkExpandWithZerosImageFilterTest
    3
)
## Decimate (in spatial domain)
itk_add_test(
  NAME itkDecimateImageFilterTest2D
  COMMAND
    IsotropicWaveletsTestDriver
    itkDecimateImageFilterTest
    2
)
itk_add_test(
  NAME itkDecimateImageFilterTest3D
  COMMAND
    IsotropicWaveletsTestDriver
    itkDecimateImageFilterTest
    3
)
### FrequencyExpandAndShrink
itk_add_test(
  NAME itkFrequencyExpandAndShrinkTest2D
  COMMAND
    IsotropicWaveletsTestDriver
    itkFrequencyExpandAndShrinkTest
    DATA{Input/checkershadow_Lch_512x512.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandAndShrinkTest2D.tiff
    2
)
itk_add_test(
  NAME itkFrequencyExpandAndShrinkTest3D
  COMMAND
    IsotropicWaveletsTestDriver
    itkFrequencyExpandAndShrinkTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandAndShrinkTest3D.tiff
    3
)
## Even input
#Expand
itk_add_test(
  NAME itkFrequencyExpandEvenTest
  COMMAND
    IsotropicWaveletsTestDriver
    # --compare
    #   DATA{Baseline/itkFrequencyExpandEvenTest.tiff}
    #   ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandEvenTest.tiff
    itkFrequencyExpandTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandEvenTest.tiff
)
##Shrink
itk_add_test(
  NAME itkFrequencyShrinkEvenTest
  COMMAND
    IsotropicWaveletsTestDriver
    # --compare
    #   DATA{Baseline/itkFrequencyShrinkEvenTest.tiff}
    #   ${ITK_TEST_OUTPUT_DIR}/itkFrequencyShrinkEvenTest.tiff
    itkFrequencyShrinkTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyShrinkEvenTest.tiff
)
itk_add_test(
  NAME itkFrequencyShrinkEvenTest2D
  COMMAND
    IsotropicWaveletsTestDriver
    itkFrequencyShrinkTest
    DATA{Input/checkershadow_Lch_512x512.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyShrinkEvenTest2D.tiff
    2
)
##FFTPad
itk_add_test(
  NAME itkFFTPadPositiveIndexImageFilterTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkFFTPadPositiveIndexImageFilterTest
    DATA{Input/checkershadow_gray_540x420.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFFTPadPositiveIndexImageFilterTest.tiff
)
## ZeroDC
itk_add_test(
  NAME itkZeroDCImageFilterTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkZeroDCImageFilterTest
    DATA{Input/checkershadow_Lch_512x512.tiff}
    2
)

## Odd input
# Require ITK_USE_FFTWF
# TODO bug in VNL FFT Forward? error with input [21,21,9] about wrong size not being multiple of 2,3,5
# if(ITK_USE_FFTWF)
#Expand
itk_add_test(
  NAME itkFrequencyExpandOddTest
  COMMAND
    IsotropicWaveletsTestDriver
    --compare
    DATA{Baseline/itkFrequencyExpandOddTest.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandOddTest.tiff
    itkFrequencyExpandTest
    DATA{Input/collagen_21x21x9.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandOddTest.tiff
)
#Shrink
itk_add_test(
  NAME itkFrequencyShrinkOddTest
  COMMAND
    IsotropicWaveletsTestDriver
    --compare
    DATA{Baseline/itkFrequencyShrinkOddTest.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyShrinkOddTest.tiff
    itkFrequencyShrinkTest
    DATA{Input/collagen_21x21x9.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyShrinkOddTest.tiff
)
list(
  APPEND TEST_LIST
  itkFrequencyExpandOddTest
  itkFrequencyShrinkOddTest
)
# endif()

itk_python_expression_add_test(
  NAME itkFFTPadPositiveIndexImageFilterPythonTest
  EXPRESSION "instance = itk.FFTPadPositiveIndexImageFilter.New()"
)
itk_python_expression_add_test(
  NAME itkFrequencyExpandImageFilterPythonTest
  EXPRESSION "instance = itk.FrequencyExpandImageFilter.New()"
)
itk_python_expression_add_test(
  NAME itkFrequencyFunctionPythonTest
  EXPRESSION "instance = itk.FrequencyFunction.New()"
)
itk_python_expression_add_test(
  NAME itkFrequencyShrinkImageFilterPythonTest
  EXPRESSION "instance = itk.FrequencyShrinkImageFilter.New()"
)
itk_python_expression_add_test(
  NAME itkIsotropicFrequencyFunctionPythonTest
  EXPRESSION "instance = itk.IsotropicFrequencyFunction.New()"
)
itk_python_expression_add_test(
  NAME itkIsotropicWaveletFrequencyFunctionPythonTest
  EXPRESSION "instance = itk.IsotropicWaveletFrequencyFunction.New()"
)
itk_python_expression_add_test(
  NAME itkHeldIsotropicWaveletPythonTest
  EXPRESSION "instance = itk.HeldIsotropicWavelet.New()"
)

itk_module_test()
set(Libraries ${IsotropicWavelets-Test_LIBRARIES})

set(
  IsotropicWaveletsTests
  itkFrequencyExpandTest.cxx
  itkFrequencyShrinkTest.cxx
  itkInd2SubTest.cxx
  itkFrequencyImageRegionIteratorWithIndexTest.cxx
  itkWaveletFrequencyForwardTest.cxx
  itkWaveletFrequencyInverseTest.cxx
  itkWaveletFrequencyFilterBankGeneratorTest.cxx
  itkRieszFrequencyFilterBankGeneratorTest.cxx
  itkMonogenicSignalFrequencyImageFilterTest.cxx
)

if(ITKVtkGlue_ENABLED)
  find_package(VTK REQUIRED)
  include_directories(SYSTEM ${VTK_INCLUDE_DIRS})
  list(APPEND Libraries ${VTK_LIBRARIES})
endif()

createtestdriver(IsotropicWavelets "${Libraries}" "${IsotropicWaveletsTests}")

#ITK_VISUALIZE_TESTS is passed to source code.
#change it to 1 to visualize images, for dev purposes.
set(ITK_VISUALIZE_TESTS 0)
target_compile_definitions(
  IsotropicWaveletsTestDriver
  PRIVATE
    ITK_VISUALIZE_TESTS=${ITK_VISUALIZE_TESTS}
)
# NOTE: If ITK_VISUALIZE_TESTS != 0, require Module_ITKVtkGlue = ON
if(ITK_VISUALIZE_TESTS AND NOT ITKVtkGlue_ENABLED)
  message(
    FATAL_ERROR
    "ITK_VISUALIZE_TESTS is set, but Module_ITKVtkGlue is not enabled. Set Module_ITKVtkGlue = ON or set ITK_VISUALIZE_TESTS to 0 to avoid this error."
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
)
itk_add_test(
  NAME itkWaveletFrequencyFilterBankGeneratorTest2
  COMMAND
    IsotropicWaveletsTestDriver
    itkWaveletFrequencyFilterBankGeneratorTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyFilterBankGeneratorTest2.tiff
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
#Wavelet Forward / Inverse
itk_add_test(
  NAME itkWaveletFrequencyForwardTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkWaveletFrequencyForwardTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyForwardTest.tiff
    1
    1
)

itk_add_test(
  NAME itkWaveletFrequencyInverseTest
  COMMAND
    IsotropicWaveletsTestDriver
    --compare
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest.tiff
    #TODO, justify following tolerance, beyond the unsigned to float conversion.
    # There is a lost of information because FrequencyShrinker, which does not interpolate. But not enough.
    --compareIntensityTolerance
    50
    itkWaveletFrequencyInverseTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest.tiff
    1
    1
)

###
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
### FrequencyExpand and FrequencyShrink
## Even input
#Expand
itk_add_test(
  NAME itkFrequencyExpandEvenTest
  COMMAND
    IsotropicWaveletsTestDriver
    --compare
    DATA{Baseline/itkFrequencyExpandEvenTest.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandEvenTest.tiff
    itkFrequencyExpandTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandEvenTest.tiff
)
##Shrink
itk_add_test(
  NAME itkFrequencyShrinkEvenTest
  COMMAND
    IsotropicWaveletsTestDriver
    --compare
    DATA{Baseline/itkFrequencyShrinkEvenTest.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyShrinkEvenTest.tiff
    itkFrequencyShrinkTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyShrinkEvenTest.tiff
)

## Odd input
# Require ITK_USE_FFTWF
# TODO bug in VNL FFT Forward? error with input [21,21,9] about wrong size not being multiple of 2,3,5
if(ITK_USE_FFTWF)
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
endif()

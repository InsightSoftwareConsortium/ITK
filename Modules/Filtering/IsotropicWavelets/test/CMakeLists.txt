itk_module_test()
set(Libraries ${IsotropicWavelets-Test_LIBRARIES})

set(
  IsotropicWaveletsTests
  #### Frequency Manipulation ###
  # Resize
  itkFrequencyExpandTest.cxx
  itkFrequencyShrinkTest.cxx
  itkFrequencyExpandAndShrinkTest.cxx
  # Frequency resize Helpers
  itkInd2SubTest.cxx
  ###########################
  # Spatial domain resize without interpolation
  itkExpandWithZerosImageFilterTest.cxx
  itkShrinkDecimateImageFilterTest.cxx
  # Syntactic sugar utilities
  itkVectorInverseFFTImageFilterTest.cxx
  itkZeroDCImageFilterTest.cxx
  # Output data for each wavelet to visualize with python.
  itkIsotropicWaveletFrequencyFunctionTest.cxx
  itkHeldIsotropicWaveletTest.cxx
  itkSimoncelliIsotropicWaveletTest.cxx
  itkVowIsotropicWaveletTest.cxx
  itkShannonIsotropicWaveletTest.cxx
  #Isotropic Wavelet
  itkWaveletFrequencyFilterBankGeneratorTest.cxx
  itkWaveletFrequencyFilterBankGeneratorDownsampleTest.cxx
  itkWaveletFrequencyForwardTest.cxx
  itkWaveletFrequencyInverseTest.cxx
  itkWaveletFrequencyForwardUndecimatedTest.cxx
  itkWaveletFrequencyInverseUndecimatedTest.cxx
  itkWaveletUtilitiesTest.cxx
  # Phase Analysis
  itkPhaseAnalysisSoftThresholdImageFilterTest.cxx
  # Riesz / Monogenic
  itkRieszFrequencyFunctionTest.cxx
  itkRieszFrequencyFilterBankGeneratorTest.cxx
  itkMonogenicSignalFrequencyImageFilterTest.cxx
  # StructureTensor
  itkStructureTensorImageFilterTest.cxx
  # TODO Wavelet + Riesz + PhaseAnalysis. This is not an unit test. Convert to example or application.
  itkRieszWaveletPhaseAnalysisTest.cxx
  itkStructureTensorWithGeneralizedRieszTest.cxx
  # Steerable Riesz Matrix
  itkRieszRotationMatrixTest.cxx
  # Composite Filter
  itkWaveletCoeffsPhaseAnalyzisImageFilterTest.cxx
  itkWaveletCoeffsSpatialDomainImageFilterTest.cxx
)

if(ITKVtkGlue_ENABLED)
  find_package(VTK REQUIRED)
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

createtestdriver(IsotropicWavelets "${Libraries}" "${IsotropicWaveletsTests}")

if(ITKVtkGlue_ENABLED)
  target_include_directories(
    IsotropicWaveletsTestDriver
    SYSTEM
    PRIVATE
      ${VTK_INCLUDE_DIRS}
  )
endif()

# Coverage flags.
# set(COVERAGE_FLAGS "-coverage -fprofile-arcs -ftest-coverage")
# set_target_properties(IsotropicWaveletsTestDriver
#   PROPERTIES COMPILE_FLAGS "${CMAKE_CXX_FLAGS} ${COVERAGE_FLAGS}"
#              LINK_FLAGS "-fprofile-arcs"
#   )

# use #ifdef ITK_VISUALIZE_TESTS (existence check), not #if ITK_VISUALIZE_TESTS
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
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyFilterBankGeneratorTest1.tiff
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
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyFilterBankGeneratorTest2.tiff
)
# 2D:
# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
# itk_add_test(NAME itkWaveletFrequencyFilterBankGeneratorTest2D1
#   COMMAND IsotropicWaveletsTestDriver
#     itkWaveletFrequencyFilterBankGeneratorTest DATA{Input/checkershadow_Lch_512x512.tiff}
#     ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyFilterBankGeneratorTest2D1.tiff
#     1 "Held" 2)
# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
#   itk_add_test(NAME itkWaveletFrequencyFilterBankGeneratorTest2D2
#   COMMAND IsotropicWaveletsTestDriver
#   itkWaveletFrequencyFilterBankGeneratorTest DATA{Input/checkershadow_Lch_512x512.tiff}
#   ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyFilterBankGeneratorTest2D2.tiff
#   2 "Held" 2)
# Generator + Downsample
# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
# itk_add_test(NAME itkWaveletFrequencyFilterBankGeneratorDownsampleTest2D
#   COMMAND IsotropicWaveletsTestDriver
#   itkWaveletFrequencyFilterBankGeneratorDownsampleTest DATA{Input/checkershadow_Lch_512x512.tiff}
#   ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyFilterBankGeneratorDownsampleTest2D.tiff
#     2 "Held" 2)
## Riesz Related
# RieszFunction
itk_add_test(
  NAME itkRieszFrequencyFunctionTest2D
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszFrequencyFunctionTest
    2
)
itk_add_test(
  NAME itkRieszFrequencyFunctionTest3D
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszFrequencyFunctionTest
    3
)
# RieszGenerator
itk_add_test(
  NAME itkRieszFrequencyFilterBankGeneratorTest1
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszFrequencyFilterBankGeneratorTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkRieszFrequencyFilterBankGeneratorTest1.tiff
    1
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkRieszFrequencyFilterBankGeneratorTest1.tiff
)
itk_add_test(
  NAME itkRieszFrequencyFilterBankGeneratorTest2
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszFrequencyFilterBankGeneratorTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkRieszFrequencyFilterBankGeneratorTest2.tiff
    2
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkRieszFrequencyFilterBankGeneratorTest2.tiff
)
# Riesz Steerable framework
itk_add_test(
  NAME itkRieszRotationMatrixTest2D
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszRotationMatrixTest
    2
)
itk_add_test(
  NAME itkRieszRotationMatrixTest3D
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszRotationMatrixTest
    3
)
# Monogenic Analysis
itk_add_test(
  NAME itkMonogenicSignalFrequencyImageFilterTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkMonogenicSignalFrequencyImageFilterTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkMonogenicSignalFrequencyImageFilterTest.tiff
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkMonogenicSignalFrequencyImageFilterTest.tiff
)

itk_add_test(
  NAME itkPhaseAnalysisSoftThresholdImageFilterTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkPhaseAnalysisSoftThresholdImageFilterTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkPhaseAnalysisSoftThresholdImageFilterTest.tiff
    1
    2.0
    10044.513
    5020.3013
    20085.115
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkPhaseAnalysisSoftThresholdImageFilterTest.tiff
)
# StructureTensor
itk_add_test(
  NAME itkStructureTensorImageFilterTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkStructureTensorImageFilterTest
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
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyForwardTest.tiff
)

# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
# itk_add_test(NAME itkWaveletFrequencyForwardTest2D
#   COMMAND IsotropicWaveletsTestDriver
#   itkWaveletFrequencyForwardTest DATA{Input/checkershadow_Lch_512x512.tiff}
#   ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyForwardTest2D.tiff
#   1 1 "Held" 2)
# Wavelet Forward Undecimated
itk_add_test(
  NAME itkWaveletFrequencyForwardUndecimatedTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkWaveletFrequencyForwardUndecimatedTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyForwardUndecimatedTest.tiff
    2
    1
    "Simoncelli"
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyForwardUndecimatedTest.tiff
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
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest.tiff
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
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTestMultiLevelMultiBand.tiff
)
# Wavelet Inverse Undecimated
itk_add_test(
  NAME itkWaveletFrequencyInverseUndecimatedTest
  COMMAND
    IsotropicWaveletsTestDriver
    --compare
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseUndecimatedTest.tiff
    itkWaveletFrequencyInverseUndecimatedTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseUndecimatedTest.tiff
    2
    1
    "Held"
    "noFilterBankPyramid"
    3
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseUndecimatedTest.tiff
)
#2D
# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
# itk_add_test(NAME itkWaveletFrequencyInverseTest2D
#   COMMAND IsotropicWaveletsTestDriver
#     --compare DATA{Input/checkershadow_Lch_512x512.tiff}
#     ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest2D.tiff
#   itkWaveletFrequencyInverseTest
#   DATA{Input/checkershadow_Lch_512x512.tiff}
#   ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest2D.tiff
#   1 1
#   "Held"
#   2)

# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
# itk_add_test(NAME itkWaveletFrequencyInverseTest2DMultiLevelMultiBand
#   COMMAND IsotropicWaveletsTestDriver
#     --compare DATA{Input/checkershadow_Lch_512x512.tiff}
#     ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest2DMultiLevelMultiBand.tiff
#   itkWaveletFrequencyInverseTest
#   DATA{Input/checkershadow_Lch_512x512.tiff}
#   ${ITK_TEST_OUTPUT_DIR}/itkWaveletFrequencyInverseTest2DMultiLevelMultiBand.tiff
#   5 5
#   "Held"
#   2)
# WaveletUtilities
itk_add_test(
  NAME itkWaveletUtilitiesTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkWaveletUtilitiesTest
)
#### TODO move StructureTensorGeneralizedRiesz and RieszWavelet to examples/applications.
set(DefaultWavelet Held)
# StructureTensor and Generalized Riesz.
itk_add_test(
  NAME itkStructureTensorWithGeneralizedRieszTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkStructureTensorWithGeneralizedRieszTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkStructureTensorWithGeneralizedRieszTest.tiff
    1
    1
    ${DefaultWavelet}
    1
    Apply
    3
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkStructureTensorWithGeneralizedRieszTest.tiff
)
# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
# itk_add_test(NAME itkStructureTensorWithGeneralizedRieszTest2D
#   COMMAND IsotropicWaveletsTestDriver
#   itkStructureTensorWithGeneralizedRieszTest DATA{Input/checkershadow_Lch_512x512.tiff}
#   ${ITK_TEST_OUTPUT_DIR}/itkStructureTensorWithGeneralizedRieszTest2D.tiff
#   1 1 ${DefaultWavelet} 2 Apply 2 )
# RieszWavelet Phase Analysis
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
    3
    Apply
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkRieszWaveletPhaseAnalysisTest.tiff
)
itk_add_test(
  NAME itkRieszWaveletPhaseAnalysisTestMultiLevelMultiBand
  COMMAND
    IsotropicWaveletsTestDriver
    itkRieszWaveletPhaseAnalysisTest
    DATA{Input/collagen_64x64x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkRieszWaveletPhaseAnalysisTestMultiLevelMultiBand.tiff
    1
    4
    ${DefaultWavelet}
    3
    Apply
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkRieszWaveletPhaseAnalysisTestMultiLevelMultiBand.tiff
)
###
# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
# itk_add_test(NAME itkRieszWaveletPhaseAnalysisTest2D
#   COMMAND IsotropicWaveletsTestDriver
#   itkRieszWaveletPhaseAnalysisTest DATA{Input/checkershadow_Lch_512x512.tiff}
#   ${ITK_TEST_OUTPUT_DIR}/itkRieszWaveletPhaseAnalysisTest2D.tiff
#   1 1 ${DefaultWavelet} 2 Apply )
# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
# itk_add_test(NAME itkRieszWaveletPhaseAnalysisTest2DMultiLevelMultiBand
#   COMMAND IsotropicWaveletsTestDriver
#   itkRieszWaveletPhaseAnalysisTest DATA{Input/checkershadow_Lch_512x512.tiff}
#   ${ITK_TEST_OUTPUT_DIR}/itkRieszWaveletPhaseAnalysisTest2DMultiLevelMultiBand.tiff
#   9 2 ${DefaultWavelet} 2 Apply )
#IsotropicWaveletFrequencyFunctionTest
itk_add_test(
  NAME itkIsotropicWaveletFrequencyFunctionHeldTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkIsotropicWaveletFrequencyFunctionTest
    ${ITK_TEST_OUTPUT_DIR}/profileMotherWaveletHeld
    "unused"
    1
    "Held"
    2
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/profileMotherWaveletHeld_*
)
itk_add_test(
  NAME itkIsotropicWaveletFrequencyFunctionShannonTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkIsotropicWaveletFrequencyFunctionTest
    ${ITK_TEST_OUTPUT_DIR}/profileMotherWaveletShannon
    "unused"
    1
    "Shannon"
    2
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/profileMotherWaveletShannon_*
)
itk_add_test(
  NAME itkIsotropicWaveletFrequencyFunctionSimoncelliTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkIsotropicWaveletFrequencyFunctionTest
    ${ITK_TEST_OUTPUT_DIR}/profileMotherWaveletSimoncelli
    "unused"
    1
    "Simoncelli"
    2
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/profileMotherWaveletSimoncelli_*
)
itk_add_test(
  NAME itkIsotropicWaveletFrequencyFunctionVowTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkIsotropicWaveletFrequencyFunctionTest
    ${ITK_TEST_OUTPUT_DIR}/profileMotherWaveletVow
    "unused"
    1
    "Vow"
    2
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/profileMotherWaveletVow_*
)

itk_add_test(
  NAME itkHeldIsotropicWaveletTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkHeldIsotropicWaveletTest
)
itk_add_test(
  NAME itkSimoncelliIsotropicWaveletTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkSimoncelliIsotropicWaveletTest
)
itk_add_test(
  NAME itkShannonIsotropicWaveletTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkShannonIsotropicWaveletTest
)
itk_add_test(
  NAME itkVowIsotropicWaveletTest
  COMMAND
    IsotropicWaveletsTestDriver
    itkVowIsotropicWaveletTest
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
    3
)
itk_add_test(
  NAME itkExpandWithZerosImageFilterTest3D
  COMMAND
    IsotropicWaveletsTestDriver
    itkExpandWithZerosImageFilterTest
    3
    3
)
## ShrinkDecimate (in spatial domain)
itk_add_test(
  NAME itkShrinkDecimateImageFilterTest2D
  COMMAND
    IsotropicWaveletsTestDriver
    itkShrinkDecimateImageFilterTest
    2
)
itk_add_test(
  NAME itkShrinkDecimateImageFilterTest3D
  COMMAND
    IsotropicWaveletsTestDriver
    itkShrinkDecimateImageFilterTest
    3
)
### FrequencyExpandAndShrink
# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
# itk_add_test(NAME itkFrequencyExpandAndShrinkTest2D
#   COMMAND IsotropicWaveletsTestDriver itkFrequencyExpandAndShrinkTest
#       DATA{Input/checkershadow_Lch_512x512.tiff}
#       ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandAndShrinkTest2D.tiff
#       2)
itk_add_test(
  NAME itkFrequencyExpandAndShrinkTest3D
  COMMAND
    IsotropicWaveletsTestDriver
    itkFrequencyExpandAndShrinkTest
    DATA{Input/collagen_32x32x16.tiff}
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandAndShrinkTest3D.tiff
    3
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandAndShrinkTest3D.tiff
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
  ITK_REMOVE_TEMPORARY_TEST_FILES
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
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkFrequencyShrinkEvenTest.tiff
)
# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
#   itk_add_test(NAME itkFrequencyShrinkEvenTest2D
#   COMMAND IsotropicWaveletsTestDriver
#   itkFrequencyShrinkTest
#     DATA{Input/checkershadow_Lch_512x512.tiff}
#     ${ITK_TEST_OUTPUT_DIR}/itkFrequencyShrinkEvenTest2D.tiff
#     2
#     )
## ZeroDC
# DISABLED: checkershadow_Lch_512x512.tiff is orphaned (404 from gh-pages,
# Girder, and itk.org). TODO: re-enable when a replacement baseline is
# added to ITKTestingData.
# itk_add_test(NAME itkZeroDCImageFilterTest
#   COMMAND IsotropicWaveletsTestDriver
#   itkZeroDCImageFilterTest
#   DATA{Input/checkershadow_Lch_512x512.tiff}
#     2
#     )
## WaveletCoeffsPhaseAnalyzis
itk_add_test(
  NAME itkWaveletCoeffsPhaseAnalyzisImageFilterTest
  COMMAND
    IsotropicWaveletsTestDriver
    --compare
    DATA{Baseline/vol11-16_16_16_output_expected.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/vol11-16_16_16_L2_B2_S2.nrrd
    itkWaveletCoeffsPhaseAnalyzisImageFilterTest
    DATA{Input/vol11-16_16_16.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/vol11-16_16_16.nrrd
    2
    2
    Simoncelli
    3
    NoApply
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/vol11-16_16_16_L2_B2_S2.nrrd
    ${ITK_TEST_OUTPUT_DIR}/vol11-16_16_16.nrrd
)
## WaveletCoeffsSpatialDomain
itk_add_test(
  NAME itkWaveletCoeffsSpatialDomainImageFilterTest
  COMMAND
    IsotropicWaveletsTestDriver
    # --compare
    # DATA{Baseline/vol11-16_16_16_output_expected.nrrd}
    # ${ITK_TEST_OUTPUT_DIR}/vol11-16_16_16_L2_B2_S2_SpatialDomain.nrrd
    itkWaveletCoeffsSpatialDomainImageFilterTest
    DATA{Input/vol11-16_16_16.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/vol11-16_16_16_SpatialDomain.nrrd
    2
    2
    Simoncelli
    3
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/vol11-16_16_16_SpatialDomain.nrrd
)

## Odd input
# Require ITK_USE_FFTWF
# Odd-sized input [21,21,9] (prime factor 7) is unsupported by the Vnl FFT,
# so these require an FFTW backend; the test selects its pixel type to match
# the configured FFTW precision.
if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  # No baseline is published for these odd-sized cases (as with the Even
  # tests above), so run without --compare: the test exercises the odd-size
  # FFTW path and fails on any exception.
  #Expand
  itk_add_test(
    NAME itkFrequencyExpandOddTest
    COMMAND
      IsotropicWaveletsTestDriver
      itkFrequencyExpandTest
      DATA{Input/collagen_21x21x9.tiff}
      ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandOddTest.tiff
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/itkFrequencyExpandOddTest.tiff
  )
  #Shrink
  itk_add_test(
    NAME itkFrequencyShrinkOddTest
    COMMAND
      IsotropicWaveletsTestDriver
      itkFrequencyShrinkTest
      DATA{Input/collagen_21x21x9.tiff}
      ${ITK_TEST_OUTPUT_DIR}/itkFrequencyShrinkOddTest.tiff
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/itkFrequencyShrinkOddTest.tiff
  )
  list(
    APPEND
    TEST_LIST
    itkFrequencyExpandOddTest
    itkFrequencyShrinkOddTest
  )
endif()

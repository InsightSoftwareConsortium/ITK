itk_module_test()
set(ITKThresholdingTests
itkBinaryThresholdImageFilterTest2.cxx
itkBinaryThresholdImageFilterTest.cxx
itkBinaryThresholdProjectionImageFilterTest.cxx
itkBinaryThresholdSpatialFunctionTest.cxx
itkHuangThresholdImageFilterTest.cxx
itkIntermodesThresholdImageFilterTest.cxx
itkIsoDataThresholdImageFilterTest.cxx
itkKittlerIllingworthThresholdImageFilterTest.cxx
itkLiThresholdImageFilterTest.cxx
itkMaximumEntropyThresholdImageFilterTest.cxx
itkMomentsThresholdImageFilterTest.cxx
itkOtsuMultipleThresholdsCalculatorTest.cxx
itkOtsuMultipleThresholdsCalculatorTest2.cxx
itkOtsuMultipleThresholdsImageFilterTest.cxx
#itkOtsuThresholdCalculatorVersusOtsuMultipleThresholdsCalculatorTest.cxx
itkOtsuThresholdCalculatorTest.cxx
itkOtsuThresholdImageFilterTest.cxx
itkRenyiEntropyThresholdImageFilterTest.cxx
itkShanbhagThresholdImageFilterTest.cxx
itkThresholdImageFilterTest.cxx
itkThresholdLabelerImageFilterTest.cxx
itkTriangleThresholdImageFilterTest.cxx
itkYenThresholdImageFilterTest.cxx
itkHuangMaskedThresholdImageFilterTest.cxx
itkIntermodesMaskedThresholdImageFilterTest.cxx
itkIsoDataMaskedThresholdImageFilterTest.cxx
itkKittlerIllingworthMaskedThresholdImageFilterTest.cxx
itkLiMaskedThresholdImageFilterTest.cxx
itkMaximumEntropyMaskedThresholdImageFilterTest.cxx
itkMomentsMaskedThresholdImageFilterTest.cxx
itkOtsuMaskedThresholdImageFilterTest.cxx
itkRenyiEntropyMaskedThresholdImageFilterTest.cxx
itkShanbhagMaskedThresholdImageFilterTest.cxx
itkTriangleMaskedThresholdImageFilterTest.cxx
itkYenMaskedThresholdImageFilterTest.cxx
itkKappaSigmaThresholdImageCalculatorTest.cxx
itkKappaSigmaThresholdImageFilterTest.cxx
)

CreateTestDriver(ITKThresholding  "${ITKThresholding-Test_LIBRARIES}" "${ITKThresholdingTests}")

itk_add_test(NAME itkOtsuThresholdCalculatorTest
      COMMAND ITKThresholdingTestDriver itkOtsuThresholdCalculatorTest)

itk_add_test(NAME itkOtsuMultipleThresholdsImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuMultipleThresholdsImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest.png
    itkOtsuMultipleThresholdsImageFilterTest DATA{${ITK_DATA_ROOT}/Input/peppers.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest.png 64 3 2 0 1)

itk_add_test(NAME itkThresholdLabelerImageFilterTest
      COMMAND ITKThresholdingTestDriver itkThresholdLabelerImageFilterTest)
itk_add_test(NAME itkBinaryThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver itkBinaryThresholdImageFilterTest)
itk_add_test(NAME itkThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver itkThresholdImageFilterTest)
itk_add_test(NAME itkOtsuMultipleThresholdsCalculatorTest1
      COMMAND ITKThresholdingTestDriver itkOtsuMultipleThresholdsCalculatorTest 0)
itk_add_test(NAME itkOtsuMultipleThresholdsCalculatorTest2
      COMMAND ITKThresholdingTestDriver itkOtsuMultipleThresholdsCalculatorTest 1)
itk_add_test(NAME itkBinaryThresholdImageFilterTest2
      COMMAND ITKThresholdingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/BinaryThresholdImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png
              itkBinaryThresholdImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm} ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png)
itk_add_test(NAME itkBinaryThresholdProjectionImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryThresholdProjection.png}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryThresholdProjection.png
    itkBinaryThresholdProjectionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryThresholdProjection.png 100 255 0)
itk_add_test(NAME itkBinaryThresholdSpatialFunctionTest
      COMMAND ITKThresholdingTestDriver itkBinaryThresholdSpatialFunctionTest)

itk_add_test(NAME itkHuangThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkHuangThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkHuangThresholdImageFilterTest.png
    itkHuangThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkHuangThresholdImageFilterTest.png 256 1 19)
itk_add_test(NAME itkHuangThresholdImageFilterTestNoAutoMinMax
      COMMAND ITKThresholdingTestDriver
    --compare-MD5 DATA{Baseline/itkHuangThresholdImageFilterTestNoAutoMinMax.png}
              fdcefb74090f63114d811e92f0f5836b
    itkHuangThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkHuangThresholdImageFilterTestNoAutoMinMax.png 32 0 -31744)

itk_add_test(NAME itkHuangMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
       --compare DATA{Baseline/itkHuangMaskedThresholdImageFilterTest.png}
                ${ITK_TEST_OUTPUT_DIR}/itkHuangMaskedThresholdImageFilterTest.png
    itkHuangMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkHuangMaskedThresholdImageFilterTest.png 1 255 239)

itk_add_test(NAME itkIntermodesThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIntermodesThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTest.png
    itkIntermodesThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTest.png 256 1 1000 1 76)
itk_add_test(NAME itkIntermodesThresholdImageFilterTestNoInterMode
      COMMAND ITKThresholdingTestDriver
    --compare-MD5 DATA{Baseline/itkIntermodesThresholdImageFilterTestNoInterMode.png}
              eed45f177050894e50ec1be5bc6bd4fa
    itkIntermodesThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTestNoInterMode.png 32 1 2000 0 75)
#itk_add_test(NAME itkIntermodesThresholdImageFilterTestNoAutoMinMax
#      COMMAND ITKThresholdingTestDriver
#    --compare-MD5 DATA{Baseline/itkIntermodesThresholdImageFilterTestNoAutoMinMax.png}
#              eed45f177050894e50ec1be5bc6bd4fa
#    itkIntermodesThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTestNoAutoMinMax.png 32 0 400000 0 19)

itk_add_test(NAME itkIntermodesMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIntermodesMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIntermodesMaskedThresholdImageFilterTest.png
    itkIntermodesMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkIntermodesMaskedThresholdImageFilterTest.png 1 255 1000 1 97)

itk_add_test(NAME itkIsoDataThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIsoDataThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIsoDataThresholdImageFilterTest.png
    itkIsoDataThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkIsoDataThresholdImageFilterTest.png 256 1 85)
itk_add_test(NAME itkIsoDataThresholdImageFilterTestNoAutoMinMax
      COMMAND ITKThresholdingTestDriver
    --compare-MD5 DATA{Baseline/itkIsoDataThresholdImageFilterTestNoAutoMinMax.png}
              9f844fd120ff49a7812e329a1b0216d8
    itkIsoDataThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkIsoDataThresholdImageFilterTestNoAutoMinMax.png 32 0 1)

itk_add_test(NAME itkIsoDataMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIsoDataMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIsoDataMaskedThresholdImageFilterTest.png
    itkIsoDataMaskedThresholdImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkIsoDataMaskedThresholdImageFilterTest.png 1 255 114)

itk_add_test(NAME itkKittlerIllingworthThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkKittlerIllingworthThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthThresholdImageFilterTest.png
    itkKittlerIllingworthThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthThresholdImageFilterTest.png 256 1 32)
itk_add_test(NAME itkKittlerIllingworthThresholdImageFilterTestNoAutoMinMax
      COMMAND ITKThresholdingTestDriver
    --compare-MD5 DATA{Baseline/itkKittlerIllingworthThresholdImageFilterTestNoAutoMinMax.png}
              ecb99e6ffea7be1e5419350f725da86b
    itkKittlerIllingworthThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthThresholdImageFilterTestNoAutoMinMax.png 32 0 1023)

itk_add_test(NAME itkKittlerIllingworthMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkKittlerIllingworthMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthMaskedThresholdImageFilterTest.png
    itkKittlerIllingworthMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthMaskedThresholdImageFilterTest.png 1 255 254)

itk_add_test(NAME itkLiThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkLiThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLiThresholdImageFilterTest.png
    itkLiThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLiThresholdImageFilterTest.png 256 1 54)
itk_add_test(NAME itkLiThresholdImageFilterTestNoAutoMinMax
      COMMAND ITKThresholdingTestDriver
    --compare-MD5 DATA{Baseline/itkLiThresholdImageFilterTestNoAutoMinMax.png}
              ecb99e6ffea7be1e5419350f725da86b
    itkLiThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLiThresholdImageFilterTestNoAutoMinMax.png 32 0 1023)

itk_add_test(NAME itkLiMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
     --compare DATA{Baseline/itkLiMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLiMaskedThresholdImageFilterTest.png
    itkLiMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkLiMaskedThresholdImageFilterTest.png 1 255 177)

itk_add_test(NAME itkMaximumEntropyThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMaximumEntropyThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTest.png
    itkMaximumEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTest.png 256 1 140)
itk_add_test(NAME itkMaximumEntropyThresholdImageFilterTestNoAutoMinMax
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMaximumEntropyThresholdImageFilterTestNoAutoMinMax.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTestNoAutoMinMax.png
    itkMaximumEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTestNoAutoMinMax.png 32 0 -31744)

itk_add_test(NAME itkMaximumEntropyMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
     --compare DATA{Baseline/itkMaximumEntropyMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyMaskedThresholdImageFilterTest.png
    itkMaximumEntropyMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyMaskedThresholdImageFilterTest.png 1 255 118)

itk_add_test(NAME itkMomentsThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMomentsThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTest.png
    itkMomentsThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTest.png 256 1 133)
itk_add_test(NAME itkMomentsThresholdImageFilterTestNoAutoMinMax
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMomentsThresholdImageFilterTestNoAutoMinMax.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTestNoAutoMinMax.png
    itkMomentsThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTestNoAutoMinMax.png 32 0 -31744)

itk_add_test(NAME itkMomentsMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
     --compare DATA{Baseline/itkMomentsMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMomentsMaskedThresholdImageFilterTest.png
    itkMomentsMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkMomentsMaskedThresholdImageFilterTest.png 1 255 139)

itk_add_test(NAME itkOtsuThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTest.png
    itkOtsuThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTest.png 256 1 84 0)
itk_add_test(NAME itkOtsuThresholdImageFilterTestFlipInsideOut
      COMMAND ITKThresholdingTestDriver
    --compare-MD5 DATA{Baseline/itkOtsuThresholdImageFilterTestFlipInsideOut.png}
              70f6b53f769782d0176c28b206d401d3
    itkOtsuThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTestFlipInsideOut.png 256 1 84 1)
itk_add_test(NAME itkOtsuThresholdImageFilterTestNoAutoMinMax
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuThresholdImageFilterTestNoAutoMinMax.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTestNoAutoMinMax.png
    itkOtsuThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTestNoAutoMinMax.png 32 0 -30720 0)

# With one threshold, the OtsuMultipleThresholdsImageFilter should give the same result as the OtsuThresholdImageFilter.
# Therefore, in this test, we compare the output to the output of the OtsuThresholdImageFilter.
# We set the number of histogram bins to 256 because that is the default for the OtsuThresholdImageFilter.
itk_add_test(NAME itkOtsuMultipleThresholdsImageFilterTest2
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuMultipleThresholdsImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest2.png
    itkOtsuMultipleThresholdsImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest2.png 256 1 0)

# Test the algorithm on an image with a small dark region.
itk_add_test(NAME itkOtsuMultipleThresholdsImageFilterTest3
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuMultipleThresholdsImageFilterTest3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest3.png
    itkOtsuMultipleThresholdsImageFilterTest DATA{${ITK_DATA_ROOT}/Input/DotOnVaryingBackground.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest3.png 64 1 2 0 1)

# Test the valley emphasis algorithm on an image with a small dark region.
itk_add_test(NAME itkOtsuMultipleThresholdsImageFilterTest4
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuMultipleThresholdsImageFilterTest4.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest4.png
    itkOtsuMultipleThresholdsImageFilterTest DATA{${ITK_DATA_ROOT}/Input/DotOnVaryingBackground.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest4.png 64 1 2 1)

itk_add_test(NAME itkOtsuMultipleThresholdsImageFilterTest5
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuMultipleThresholdsImageFilterTest5.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest5.png
    itkOtsuMultipleThresholdsImageFilterTest DATA{Input/image_3modes.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest5.png 128 2 0)

itk_add_test(NAME itkOtsuMultipleThresholdsImageFilterTest6
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuMultipleThresholdsImageFilterTest5.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest6.png
    itkOtsuMultipleThresholdsImageFilterTest DATA{Input/image_3modes.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest6.png 128 2 0 0 1)
set_tests_properties(itkOtsuMultipleThresholdsImageFilterTest6 PROPERTIES WILL_FAIL TRUE)

#itk_add_test(NAME itkOtsuThresholdCalculatorVersusOtsuMultipleThresholdsCalculatorTest
#      COMMAND ITKThresholdingTestDriver
#      itkOtsuThresholdCalculatorVersusOtsuMultipleThresholdsCalculatorTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png})

itk_add_test(NAME itkOtsuMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuMaskedThresholdImageFilterTest.png
    itkOtsuMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMaskedThresholdImageFilterTest.png 1 255 184)

itk_add_test(NAME itkRenyiEntropyThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkRenyiEntropyThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyThresholdImageFilterTest.png
    itkRenyiEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyThresholdImageFilterTest.png 256 1 51)
itk_add_test(NAME itkRenyiEntropyThresholdImageFilterTestNoAutoMinMax
      COMMAND ITKThresholdingTestDriver
    --compare-MD5 DATA{Baseline/itkRenyiEntropyThresholdImageFilterTestNoAutoMinMax.png}
              d5caadf6ba3cc5177eaa04e831fd0738
    itkRenyiEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyThresholdImageFilterTestNoAutoMinMax.png 32 0 -31744)


itk_add_test(NAME itkRenyiEntropyMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkRenyiEntropyMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyMaskedThresholdImageFilterTest.png
    itkRenyiEntropyMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyMaskedThresholdImageFilterTest.png 1 255 118)

itk_add_test(NAME itkShanbhagThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkShanbhagThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTest.png
    itkShanbhagThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTest.png 256 1 133)
itk_add_test(NAME itkShanbhagThresholdImageFilterTestNoAutoMinMax
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkShanbhagThresholdImageFilterTestNoAutoMinMax.png}
              ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTestNoAutoMinMax.png
    itkShanbhagThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTestNoAutoMinMax.png 32 0 -31744)

itk_add_test(NAME itkShanbhagMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
     --compare DATA{Baseline/itkShanbhagMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkShanbhagMaskedThresholdImageFilterTest.png
    itkShanbhagMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkShanbhagMaskedThresholdImageFilterTest.png 1 255 55)


itk_add_test(NAME itkTriangleThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkTriangleThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleThresholdImageFilterTest.png
    itkTriangleThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkTriangleThresholdImageFilterTest.png 256 1 2)
itk_add_test(NAME itkTriangleThresholdImageFilterTestNoAutoMinMax
      COMMAND ITKThresholdingTestDriver
    --compare-MD5 DATA{Baseline/itkTriangleThresholdImageFilterTestNoAutoMinMax.png}
              ecb99e6ffea7be1e5419350f725da86b
    itkTriangleThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkTriangleThresholdImageFilterTestNoAutoMinMax.png 32 0 3071)

itk_add_test(NAME itkTriangleMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
     --compare DATA{Baseline/itkTriangleMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleMaskedThresholdImageFilterTest.png
    itkTriangleMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkTriangleMaskedThresholdImageFilterTest.png 1 255 254)

itk_add_test(NAME itkYenThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkYenThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkYenThresholdImageFilterTest.png
    itkYenThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkYenThresholdImageFilterTest.png 256 1 26)
itk_add_test(NAME itkYenThresholdImageFilterTestNoAutoMinMax
      COMMAND ITKThresholdingTestDriver
    --compare-MD5 DATA{Baseline/itkYenThresholdImageFilterTestNoAutoMinMax.png}
              fcd6bcb56c1689fcef28b57c22475bad
    itkYenThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkYenThresholdImageFilterTestNoAutoMinMax.png 32 0 -31744)

itk_add_test(NAME itkYenMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkYenMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkYenMaskedThresholdImageFilterTest.png
    itkYenMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkYenMaskedThresholdImageFilterTest.png 1 255 118)

itk_add_test(NAME itkHuangThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkHuangThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkHuangThresholdImageFilterTestShort.png
    itkHuangThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkHuangThresholdImageFilterTestShort.png 256 1 30771)

itk_add_test(NAME itkIntermodesThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIntermodesThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTestShort.png
    itkIntermodesThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTestShort.png 256 1 1000 1 14251)

itk_add_test(NAME itkIsoDataThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIsoDataThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIsoDataThresholdImageFilterTestShort.png
    itkIsoDataThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkIsoDataThresholdImageFilterTestShort.png 256 1 15044)

itk_add_test(NAME itkKittlerIllingworthThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkKittlerIllingworthThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthThresholdImageFilterTestShort.png
    itkKittlerIllingworthThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthThresholdImageFilterTestShort.png 256 1 31168)

itk_add_test(NAME itkLiThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkLiThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLiThresholdImageFilterTestShort.png
    itkLiThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkLiThresholdImageFilterTestShort.png 256 1 26939)

itk_add_test(NAME itkMaximumEntropyThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMaximumEntropyThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTestShort.png
    itkMaximumEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTestShort.png 256 1 22181)

itk_add_test(NAME itkMomentsThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMomentsThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTestShort.png
    itkMomentsThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTestShort.png 256 1 23238)

itk_add_test(NAME itkOtsuThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTestShort.png
    itkOtsuThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTestShort.png 256 1 14516 1)

itk_add_test(NAME itkRenyiEntropyThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkRenyiEntropyThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyThresholdImageFilterTestShort.png
    itkRenyiEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyThresholdImageFilterTestShort.png 256 1 22181)

itk_add_test(NAME itkShanbhagThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkShanbhagThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTestShort.png
    itkShanbhagThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTestShort.png 256 1 28921)

itk_add_test(NAME itkTriangleThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkTriangleThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleThresholdImageFilterTestShort.png
    itkTriangleThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkTriangleThresholdImageFilterTestShort.png 256 1 22313)

itk_add_test(NAME itkYenThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkYenThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkYenThresholdImageFilterTestShort.png
    itkYenThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkYenThresholdImageFilterTestShort.png 256 1 22181)

itk_add_test(NAME itkKappaSigmaThresholdImageCalculatorTest01
      COMMAND ITKThresholdingTestDriver itkKappaSigmaThresholdImageCalculatorTest
              DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence1.png} 255 3.5 10 229)

itk_add_test(NAME itkKappaSigmaThresholdImageCalculatorTest02
      COMMAND ITKThresholdingTestDriver itkKappaSigmaThresholdImageCalculatorTest
              DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence2.png} 255 3.0 10 361)

itk_add_test(NAME itkKappaSigmaThresholdImageFilterTest01
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkKappaSigmaThresholdImageFilterTest01.png}
              ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest01.png
    itkKappaSigmaThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence1.png} ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest01.png 255 3.5 2 69)

itk_add_test(NAME itkKappaSigmaThresholdImageFilterTest02
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkKappaSigmaThresholdImageFilterTest02.png}
              ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest02.png
    itkKappaSigmaThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence2.png} ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest02.png 255 3.0 2 92)

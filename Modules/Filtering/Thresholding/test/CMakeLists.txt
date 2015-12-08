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
itkOtsuMultipleThresholdsImageFilterTest.cxx
itkOtsuThresholdCalculatorVersusOtsuMultipleThresholdsCalculatorTest.cxx
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
    itkOtsuMultipleThresholdsImageFilterTest DATA{${ITK_DATA_ROOT}/Input/peppers.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest.png 64 3 2)

itk_add_test(NAME itkThresholdLabelerImageFilterTest
      COMMAND ITKThresholdingTestDriver itkThresholdLabelerImageFilterTest)
itk_add_test(NAME itkBinaryThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver itkBinaryThresholdImageFilterTest)
itk_add_test(NAME itkThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver itkThresholdImageFilterTest)
itk_add_test(NAME itkOtsuMultipleThresholdsCalculatorTest
      COMMAND ITKThresholdingTestDriver itkOtsuMultipleThresholdsCalculatorTest)
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
    itkHuangThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkHuangThresholdImageFilterTest.png)

itk_add_test(NAME itkHuangMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
       --compare DATA{Baseline/itkHuangMaskedThresholdImageFilterTest.png}
                ${ITK_TEST_OUTPUT_DIR}/itkHuangMaskedThresholdImageFilterTest.png
    itkHuangMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkHuangMaskedThresholdImageFilterTest.png)

itk_add_test(NAME itkIntermodesThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIntermodesThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTest.png
    itkIntermodesThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTest.png)

itk_add_test(NAME itkIntermodesMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIntermodesMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIntermodesMaskedThresholdImageFilterTest.png
    itkIntermodesMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkIntermodesMaskedThresholdImageFilterTest.png)

itk_add_test(NAME itkIsoDataThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIsoDataThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIsoDataThresholdImageFilterTest.png
    itkIsoDataThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkIsoDataThresholdImageFilterTest.png)

itk_add_test(NAME itkIsoDataMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIsoDataMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIsoDataMaskedThresholdImageFilterTest.png
    itkIsoDataMaskedThresholdImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkIsoDataMaskedThresholdImageFilterTest.png)

itk_add_test(NAME itkKittlerIllingworthThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkKittlerIllingworthThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthThresholdImageFilterTest.png
    itkKittlerIllingworthThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthThresholdImageFilterTest.png)

itk_add_test(NAME itkKittlerIllingworthMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkKittlerIllingworthMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthMaskedThresholdImageFilterTest.png
    itkKittlerIllingworthMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthMaskedThresholdImageFilterTest.png)

itk_add_test(NAME itkLiThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkLiThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLiThresholdImageFilterTest.png
    itkLiThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLiThresholdImageFilterTest.png)

itk_add_test(NAME itkLiMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
     --compare DATA{Baseline/itkLiMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLiMaskedThresholdImageFilterTest.png
    itkLiMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkLiMaskedThresholdImageFilterTest.png)

itk_add_test(NAME itkMaximumEntropyThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMaximumEntropyThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTest.png
    itkMaximumEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTest.png)

itk_add_test(NAME itkMaximumEntropyMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
     --compare DATA{Baseline/itkMaximumEntropyMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyMaskedThresholdImageFilterTest.png
    itkMaximumEntropyMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyMaskedThresholdImageFilterTest.png)

itk_add_test(NAME itkMomentsThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMomentsThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTest.png
    itkMomentsThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTest.png)

itk_add_test(NAME itkMomentsMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
     --compare DATA{Baseline/itkMomentsMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMomentsMaskedThresholdImageFilterTest.png
    itkMomentsMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkMomentsMaskedThresholdImageFilterTest.png)

itk_add_test(NAME itkOtsuThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTest.png
    itkOtsuThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTest.png 256 1)

# With one threshold, the OtsuMultipleThresholdsImageFilter should give the same result as the OtsuThresholdImageFilter.
# Therefore, in this test, we compare the output to the output of the OtsuThresholdImageFilter.
# We set the number of histogram bins to 256 because that is the default for the OtsuThresholdImageFilter.
itk_add_test(NAME itkOtsuMultipleThresholdsImageFilterTest2
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest2.png
    itkOtsuMultipleThresholdsImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest2.png 256 1 0)

# Test the algorithm on an image with a small dark region.
itk_add_test(NAME itkOtsuMultipleThresholdsImageFilterTest3
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuMultipleThresholdsImageFilterTest3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest3.png
    itkOtsuMultipleThresholdsImageFilterTest DATA{${ITK_DATA_ROOT}/Input/DotOnVaryingBackground.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest3.png 64 1 2 0)

# Test the valley emphasis algorithm on an image with a small dark region.
itk_add_test(NAME itkOtsuMultipleThresholdsImageFilterTest4
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuMultipleThresholdsImageFilterTest4.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest4.png
    itkOtsuMultipleThresholdsImageFilterTest DATA{${ITK_DATA_ROOT}/Input/DotOnVaryingBackground.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMultipleThresholdsImageFilterTest4.png 64 1 2 1)

itk_add_test(NAME itkOtsuThresholdCalculatorVersusOtsuMultipleThresholdsCalculatorTest
      COMMAND ITKThresholdingTestDriver
      itkOtsuThresholdCalculatorVersusOtsuMultipleThresholdsCalculatorTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png})

itk_add_test(NAME itkOtsuMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuMaskedThresholdImageFilterTest.png
    itkOtsuMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuMaskedThresholdImageFilterTest.png)

itk_add_test(NAME itkRenyiEntropyThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkRenyiEntropyThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyThresholdImageFilterTest.png
    itkRenyiEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyThresholdImageFilterTest.png)

itk_add_test(NAME itkRenyiEntropyMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkRenyiEntropyMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyMaskedThresholdImageFilterTest.png
    itkRenyiEntropyMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyMaskedThresholdImageFilterTest.png)

itk_add_test(NAME itkShanbhagThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkShanbhagThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTest.png
    itkShanbhagThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTest.png)

itk_add_test(NAME itkShanbhagMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
     --compare DATA{Baseline/itkShanbhagMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkShanbhagMaskedThresholdImageFilterTest.png
    itkShanbhagMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkShanbhagMaskedThresholdImageFilterTest.png)


itk_add_test(NAME itkTriangleThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkTriangleThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleThresholdImageFilterTest.png
    itkTriangleThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkTriangleThresholdImageFilterTest.png)

itk_add_test(NAME itkTriangleMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
     --compare DATA{Baseline/itkTriangleMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleMaskedThresholdImageFilterTest.png
    itkTriangleMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkTriangleMaskedThresholdImageFilterTest.png)

itk_add_test(NAME itkYenThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkYenThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkYenThresholdImageFilterTest.png
    itkYenThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkYenThresholdImageFilterTest.png)

itk_add_test(NAME itkYenMaskedThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkYenMaskedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkYenMaskedThresholdImageFilterTest.png
    itkYenMaskedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-threshmask.png} ${ITK_TEST_OUTPUT_DIR}/itkYenMaskedThresholdImageFilterTest.png)

itk_add_test(NAME itkHuangThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkHuangThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkHuangThresholdImageFilterTestShort.png
    itkHuangThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkHuangThresholdImageFilterTestShort.png)

itk_add_test(NAME itkIntermodesThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIntermodesThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTestShort.png
    itkIntermodesThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTestShort.png)

itk_add_test(NAME itkIsoDataThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIsoDataThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIsoDataThresholdImageFilterTestShort.png
    itkIsoDataThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkIsoDataThresholdImageFilterTestShort.png)

itk_add_test(NAME itkKittlerIllingworthThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkKittlerIllingworthThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthThresholdImageFilterTestShort.png
    itkKittlerIllingworthThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthThresholdImageFilterTestShort.png)

itk_add_test(NAME itkLiThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkLiThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLiThresholdImageFilterTestShort.png
    itkLiThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkLiThresholdImageFilterTestShort.png)

itk_add_test(NAME itkMaximumEntropyThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMaximumEntropyThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTestShort.png
    itkMaximumEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTestShort.png)

itk_add_test(NAME itkMomentsThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMomentsThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTestShort.png
    itkMomentsThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTestShort.png)

itk_add_test(NAME itkOtsuThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTestShort.png
    itkOtsuThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTestShort.png)

itk_add_test(NAME itkRenyiEntropyThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkRenyiEntropyThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyThresholdImageFilterTestShort.png
    itkRenyiEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyThresholdImageFilterTestShort.png)

itk_add_test(NAME itkShanbhagThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkShanbhagThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTestShort.png
    itkShanbhagThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTestShort.png)

itk_add_test(NAME itkTriangleThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkTriangleThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleThresholdImageFilterTestShort.png
    itkTriangleThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkTriangleThresholdImageFilterTestShort.png)

itk_add_test(NAME itkYenThresholdImageFilterTestShort
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkYenThresholdImageFilterTestShort.png}
              ${ITK_TEST_OUTPUT_DIR}/itkYenThresholdImageFilterTestShort.png
    itkYenThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkYenThresholdImageFilterTestShort.png)

itk_add_test(NAME itkKappaSigmaThresholdImageCalculatorTest01
      COMMAND ITKThresholdingTestDriver itkKappaSigmaThresholdImageCalculatorTest
              DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence1.png} 10 3.5 19)

itk_add_test(NAME itkKappaSigmaThresholdImageCalculatorTest02
      COMMAND ITKThresholdingTestDriver itkKappaSigmaThresholdImageCalculatorTest
              DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence2.png} 10 3.0 19)

itk_add_test(NAME itkKappaSigmaThresholdImageFilterTest01
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkKappaSigmaThresholdImageFilterTest01.png} ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest01.png
    itkKappaSigmaThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence1.png} ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest01.png 2 3.5)

itk_add_test(NAME itkKappaSigmaThresholdImageFilterTest02
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkKappaSigmaThresholdImageFilterTest02.png} ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest02.png
    itkKappaSigmaThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence2.png} ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest02.png 2 3.0)

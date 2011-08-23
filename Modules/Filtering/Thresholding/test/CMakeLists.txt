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
itkOtsuThresholdCalculatorTest.cxx
itkOtsuThresholdImageFilterTest.cxx
itkRenyiEntropyThresholdImageFilterTest.cxx
itkShanbhagThresholdImageFilterTest.cxx
itkThresholdImageFilterTest.cxx
itkThresholdLabelerImageFilterTest.cxx
itkTriangleThresholdImageFilterTest.cxx
itkYenThresholdImageFilterTest.cxx
)

CreateTestDriver(ITKThresholding  "${ITKThresholding-Test_LIBRARIES}" "${ITKThresholdingTests}")

itk_add_test(NAME itkOtsuThresholdCalculatorTest
      COMMAND ITKThresholdingTestDriver itkOtsuThresholdCalculatorTest)
itk_add_test(NAME itkOtsuMultipleThresholdsImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuMultipleThresholdsImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png
    itkOtsuMultipleThresholdsImageFilterTest DATA{${ITK_DATA_ROOT}/Input/peppers.png} ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png 64 3 2)
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
    itkBinaryThresholdImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png)
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
itk_add_test(NAME itkIntermodesThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIntermodesThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTest.png
    itkIntermodesThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkIntermodesThresholdImageFilterTest.png)
itk_add_test(NAME itkIsoDataThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkIsoDataThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIsoDataThresholdImageFilterTest.png
    itkIsoDataThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkIsoDataThresholdImageFilterTest.png)
itk_add_test(NAME itkKittlerIllingworthThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkKittlerIllingworthThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthThresholdImageFilterTest.png
    itkKittlerIllingworthThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkKittlerIllingworthThresholdImageFilterTest.png)
itk_add_test(NAME itkLiThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkLiThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLiThresholdImageFilterTest.png
    itkLiThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLiThresholdImageFilterTest.png)
itk_add_test(NAME itkMaximumEntropyThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMaximumEntropyThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTest.png
    itkMaximumEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkMaximumEntropyThresholdImageFilterTest.png)
itk_add_test(NAME itkMomentsThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkMomentsThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTest.png
    itkMomentsThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkMomentsThresholdImageFilterTest.png)
itk_add_test(NAME itkOtsuThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkOtsuThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTest.png
    itkOtsuThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkOtsuThresholdImageFilterTest.png)
itk_add_test(NAME itkRenyiEntropyThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkRenyiEntropyThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyThresholdImageFilterTest.png
    itkRenyiEntropyThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkRenyiEntropyThresholdImageFilterTest.png)
itk_add_test(NAME itkShanbhagThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkShanbhagThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTest.png
    itkShanbhagThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkShanbhagThresholdImageFilterTest.png)
itk_add_test(NAME itkTriangleThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkTriangleThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleThresholdImageFilterTest.png
    itkTriangleThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkTriangleThresholdImageFilterTest.png)
itk_add_test(NAME itkYenThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare DATA{Baseline/itkYenThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkYenThresholdImageFilterTest.png
    itkYenThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkYenThresholdImageFilterTest.png)

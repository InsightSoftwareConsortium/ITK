itk_module_test()
set(ITK-ConnectedComponentsTests
itkRelabelComponentImageFilterTest.cxx
itkHardConnectedComponentImageFilterTest.cxx
itkConnectedComponentImageFilterTestRGB.cxx
itkConnectedComponentsHeaderTest.cxx
itkConnectedComponentImageFilterTest.cxx
itkThresholdMaximumConnectedComponentsImageFilterTest.cxx
itkScalarConnectedComponentImageFilterTest.cxx
itkVectorConnectedComponentImageFilterTest.cxx
itkConnectedComponentImageFilterTooManyObjectsTest.cxx
itkMaskConnectedComponentImageFilterTest.cxx
)

CreateTestDriver(ITK-ConnectedComponents  "${ITK-ConnectedComponents-Test_LIBRARIES}" "${ITK-ConnectedComponentsTests}")

itk_add_test(NAME itkConnectedComponentsHeaderTest
      COMMAND ITK-ConnectedComponentsTestDriver itkConnectedComponentsHeaderTest)
itk_add_test(NAME itkRelabelComponentImageFilterTest
      COMMAND ITK-ConnectedComponentsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RelabelComponentImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/RelabelComponentImageFilterTest.png
    itkRelabelComponentImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/RelabelComponentImageFilterTest.png 130 145)
itk_add_test(NAME itkHardConnectedComponentImageFilterTest
      COMMAND ITK-ConnectedComponentsTestDriver itkHardConnectedComponentImageFilterTest)
itk_add_test(NAME itkConnectedComponentImageFilterTestRGB
      COMMAND ITK-ConnectedComponentsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTestRGB.png
    itkConnectedComponentImageFilterTestRGB ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTestRGB.png 130 145)
itk_add_test(NAME itkConnectedComponentImageFilterTest
      COMMAND ITK-ConnectedComponentsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png
    itkConnectedComponentImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png 130 145)
itk_add_test(NAME itkConnectedComponentImageFilterTest2
      COMMAND ITK-ConnectedComponentsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest2.png
    itkConnectedComponentImageFilterTest ${ITK_DATA_ROOT}/Input/Shapes.png ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest2.png 128 255 0)
itk_add_test(NAME itkConnectedComponentImageFilterTest3
      COMMAND ITK-ConnectedComponentsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest3.png
              ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest3.png
    itkConnectedComponentImageFilterTest ${ITK_DATA_ROOT}/Input/Shapes.png ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest3.png 128 255 1)
itk_add_test(NAME itkThresholdMaximumConnectedComponentsImageFilterTest1
      COMMAND ITK-ConnectedComponentsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/itkThresholdMaximumConnectedComponentsImageFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png
    itkThresholdMaximumConnectedComponentsImageFilterTest ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png 10)
itk_add_test(NAME itkThresholdMaximumConnectedComponentsImageFilterTest2
      COMMAND ITK-ConnectedComponentsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/itkThresholdMaximumConnectedComponentsImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png
    itkThresholdMaximumConnectedComponentsImageFilterTest ${ITK_DATA_ROOT}/Input/CellsFluorescence2.png ${ITK_TEST_OUTPUT_DIR}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png 10)
itk_add_test(NAME itkScalarConnectedComponentImageFilterTest
      COMMAND ITK-ConnectedComponentsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ScalarConnectedComponentImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ScalarConnectedComponentImageFilterTest.png
    itkScalarConnectedComponentImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/ScalarConnectedComponentImageFilterTest.png 20 1)
itk_add_test(NAME itkVectorConnectedComponentImageFilterTest
      COMMAND ITK-ConnectedComponentsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConnectedComponentImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/VectorConnectedComponentImageFilterTest.png
    itkVectorConnectedComponentImageFilterTest ${ITK_TEST_OUTPUT_DIR}/VectorConnectedComponentImageFilterTest.png)
itk_add_test(NAME itkConnectedComponentImageFilterTooManyObjectsTest
      COMMAND ITK-ConnectedComponentsTestDriver itkConnectedComponentImageFilterTooManyObjectsTest)
itk_add_test(NAME itkMaskConnectedComponentImageFilterTest
      COMMAND ITK-ConnectedComponentsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MaskConnectedComponentImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/MaskConnectedComponentImageFilterTest.png
    itkMaskConnectedComponentImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/MaskConnectedComponentImageFilterTest.png 130 145)

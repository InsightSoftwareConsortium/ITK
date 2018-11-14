itk_module_test()
set(ITKConnectedComponentsTests
itkRelabelComponentImageFilterTest.cxx
itkHardConnectedComponentImageFilterTest.cxx
itkConnectedComponentImageFilterTestRGB.cxx
itkConnectedComponentImageFilterTest.cxx
itkConnectedComponentImageFilterBackgroundTest.cxx
itkThresholdMaximumConnectedComponentsImageFilterTest.cxx
itkScalarConnectedComponentImageFilterTest.cxx
itkVectorConnectedComponentImageFilterTest.cxx
itkConnectedComponentImageFilterTooManyObjectsTest.cxx
itkMaskConnectedComponentImageFilterTest.cxx
)

CreateTestDriver(ITKConnectedComponents  "${ITKConnectedComponents-Test_LIBRARIES}" "${ITKConnectedComponentsTests}")

itk_add_test(NAME itkRelabelComponentImageFilterTest
      COMMAND ITKConnectedComponentsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/RelabelComponentImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/RelabelComponentImageFilterTest.png
    itkRelabelComponentImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/RelabelComponentImageFilterTest.png 130 145)
itk_add_test(NAME itkHardConnectedComponentImageFilterTest
      COMMAND ITKConnectedComponentsTestDriver
    --compare DATA{Baseline/HardConnectedComponentImageFilterTest.png}
      ${ITK_TEST_OUTPUT_DIR}/HardConnectedComponentImageFilterTestUnsignedChar.png
    --compare DATA{Baseline/HardConnectedComponentImageFilterTest.png}
      ${ITK_TEST_OUTPUT_DIR}/HardConnectedComponentImageFilterTestUnsignedShort.png
    itkHardConnectedComponentImageFilterTest
      ${ITK_TEST_OUTPUT_DIR}/HardConnectedComponentImageFilterTest)
itk_add_test(NAME itkConnectedComponentImageFilterTestRGB
      COMMAND ITKConnectedComponentsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest.png,:}
              ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTestRGB.png
    itkConnectedComponentImageFilterTestRGB DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTestRGB.png 130 145)
itk_add_test(NAME itkConnectedComponentImageFilterTest
      COMMAND ITKConnectedComponentsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest.png,:}
              ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png
    itkConnectedComponentImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png 130 145)
itk_add_test(NAME itkConnectedComponentImageFilterTest2
      COMMAND ITKConnectedComponentsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest2.png,:}
              ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest2.png
    itkConnectedComponentImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Shapes.png} ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest2.png 128 255 0)
itk_add_test(NAME itkConnectedComponentImageFilterTest3
      COMMAND ITKConnectedComponentsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest3.png,:}
              ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest3.png
    itkConnectedComponentImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Shapes.png} ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest3.png 128 255 1)
itk_add_test(NAME itkConnectedComponentImageFilterBackgroundTest1
      COMMAND ITKConnectedComponentsTestDriver
    itkConnectedComponentImageFilterBackgroundTest 1)
itk_add_test(NAME itkConnectedComponentImageFilterBackgroundTest2
      COMMAND ITKConnectedComponentsTestDriver
    itkConnectedComponentImageFilterBackgroundTest -1)
itk_add_test(NAME itkThresholdMaximumConnectedComponentsImageFilterTest1
      COMMAND ITKConnectedComponentsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/itkThresholdMaximumConnectedComponentsImageFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png
    itkThresholdMaximumConnectedComponentsImageFilterTest DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence1.png} ${ITK_TEST_OUTPUT_DIR}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png 10)
itk_add_test(NAME itkThresholdMaximumConnectedComponentsImageFilterTest2
      COMMAND ITKConnectedComponentsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/itkThresholdMaximumConnectedComponentsImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png
    itkThresholdMaximumConnectedComponentsImageFilterTest DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence2.png} ${ITK_TEST_OUTPUT_DIR}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png 10)
itk_add_test(NAME itkScalarConnectedComponentImageFilterTest
      COMMAND ITKConnectedComponentsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ScalarConnectedComponentImageFilterTest.png,:}
              ${ITK_TEST_OUTPUT_DIR}/ScalarConnectedComponentImageFilterTest.png
    itkScalarConnectedComponentImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/ScalarConnectedComponentImageFilterTest.png 20 1)
itk_add_test(NAME itkVectorConnectedComponentImageFilterTest
      COMMAND ITKConnectedComponentsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConnectedComponentImageFilterTest.png,:}
              ${ITK_TEST_OUTPUT_DIR}/VectorConnectedComponentImageFilterTest.png
    itkVectorConnectedComponentImageFilterTest ${ITK_TEST_OUTPUT_DIR}/VectorConnectedComponentImageFilterTest.png)
itk_add_test(NAME itkConnectedComponentImageFilterTooManyObjectsTest
      COMMAND ITKConnectedComponentsTestDriver itkConnectedComponentImageFilterTooManyObjectsTest)
itk_add_test(NAME itkMaskConnectedComponentImageFilterTest
      COMMAND ITKConnectedComponentsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/MaskConnectedComponentImageFilterTest.png,:}
              ${ITK_TEST_OUTPUT_DIR}/MaskConnectedComponentImageFilterTest.png
    itkMaskConnectedComponentImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/MaskConnectedComponentImageFilterTest.png 130 145)

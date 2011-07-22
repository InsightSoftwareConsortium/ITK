itk_module_test()
set(ITKBinaryMathematicalMorphologyTests
itkBinaryDilateImageFilterTest.cxx
itkBinaryDilateImageFilterTest2.cxx
itkBinaryDilateImageFilterTest3.cxx
itkBinaryErodeImageFilterTest.cxx
itkBinaryErodeImageFilterTest3.cxx
itkBinaryMorphologicalClosingImageFilterTest.cxx
itkBinaryMorphologicalOpeningImageFilterTest.cxx
itkBinaryThinningImageFilterTest.cxx
itkBinaryMathematicalMorphologyHeaderTest.cxx
)

CreateTestDriver(ITKBinaryMathematicalMorphology  "${ITKBinaryMathematicalMorphology-Test_LIBRARIES}" "${ITKBinaryMathematicalMorphologyTests}")

itk_add_test(NAME itkBinaryMathematicalMorphologyHeaderTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver itkBinaryMathematicalMorphologyHeaderTest)
itk_add_test(NAME itkBinaryDilateImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver itkBinaryDilateImageFilterTest)
itk_add_test(NAME itkBinaryDilateImageFilterTest2
      COMMAND ITKBinaryMathematicalMorphologyTestDriver itkBinaryDilateImageFilterTest2)
itk_add_test(NAME itkBinaryDilateImageFilterTest-100-0-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-100-0-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-0-0.png
    itkBinaryDilateImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-0-0.png 100 0 0 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-100-0-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-100-0-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-0-1.png
    itkBinaryDilateImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-0-1.png 100 0 1 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-100-150-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-100-150-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-150-0.png
    itkBinaryDilateImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-150-0.png 100 150 0 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-100-150-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-100-150-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-150-1.png
    itkBinaryDilateImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-150-1.png 100 150 1 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-200-0-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-200-0-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-0-0.png
    itkBinaryDilateImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-0-0.png 200 0 0 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-200-0-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-200-0-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-0-1.png
    itkBinaryDilateImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-0-1.png 200 0 1 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-200-150-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-200-150-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-150-0.png
    itkBinaryDilateImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-150-0.png 200 150 0 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-200-150-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-200-150-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-150-1.png
    itkBinaryDilateImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-150-1.png 200 150 1 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver itkBinaryErodeImageFilterTest)
itk_add_test(NAME itkBinaryErodeImageFilterTest-100-0-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-100-0-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-0-0.png
    itkBinaryErodeImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-0-0.png 100 0 0 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-100-0-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-100-0-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-0-1.png
    itkBinaryErodeImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-0-1.png 100 0 1 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-100-150-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-100-150-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-150-0.png
    itkBinaryErodeImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-150-0.png 100 150 0 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-100-150-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-100-150-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-150-1.png
    itkBinaryErodeImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-150-1.png 100 150 1 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-200-0-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-200-0-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-0-0.png
    itkBinaryErodeImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-0-0.png 200 0 0 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-200-0-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-200-0-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-0-1.png
    itkBinaryErodeImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-0-1.png 200 0 1 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-200-150-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-200-150-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-150-0.png
    itkBinaryErodeImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-150-0.png 200 150 0 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-200-150-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-200-150-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-150-1.png
    itkBinaryErodeImageFilterTest3 ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-150-1.png 200 150 1 10)
itk_add_test(NAME itkBinaryMorphologicalClosingImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryMorphologicalClosingImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalClosingImageFilterTest.png
    itkBinaryMorphologicalClosingImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalClosingImageFilterTest.png 40 1 200)
itk_add_test(NAME itkBinaryMorphologicalClosingImageFilterTestUnsafe
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
    itkBinaryMorphologicalClosingImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png 40 0 200)
itk_add_test(NAME itkBinaryMorphologicalOpeningImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryMorphologicalOpeningImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalOpeningImageFilterTest.png
    itkBinaryMorphologicalOpeningImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalOpeningImageFilterTest.png 8 150 200)
itk_add_test(NAME itkBinaryThinningImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/BinaryThinningImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/BinaryThinningImageFilterTest.png
    itkBinaryThinningImageFilterTest ${ITK_DATA_ROOT}/Input/Shapes.png ${ITK_TEST_OUTPUT_DIR}/BinaryThinningImageFilterTest.png)

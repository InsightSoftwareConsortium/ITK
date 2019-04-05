itk_module_test()
set(ITKBinaryMathematicalMorphologyTests
itkBinaryClosingByReconstructionImageFilterTest.cxx
itkBinaryDilateImageFilterTest.cxx
itkBinaryDilateImageFilterTest2.cxx
itkBinaryDilateImageFilterTest3.cxx
itkBinaryErodeImageFilterTest.cxx
itkBinaryErodeImageFilterTest3.cxx
itkBinaryMorphologicalClosingImageFilterTest.cxx
itkBinaryMorphologicalOpeningImageFilterTest.cxx
itkBinaryOpeningByReconstructionImageFilterTest.cxx
itkBinaryThinningImageFilterTest.cxx
itkErodeObjectMorphologyImageFilterTest.cxx
)

CreateTestDriver(ITKBinaryMathematicalMorphology  "${ITKBinaryMathematicalMorphology-Test_LIBRARIES}" "${ITKBinaryMathematicalMorphologyTests}")

itk_add_test(NAME itkErodeObjectMorphologyImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver itkErodeObjectMorphologyImageFilterTest)
itk_add_test(NAME itkBinaryClosingByReconstructionImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkBinaryClosingByReconstructionImageFilterTest.png
    "4eadd941a629c7b52f9aa1be048c854c"
    itkBinaryClosingByReconstructionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1-bin.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryClosingByReconstructionImageFilterTest.png 0 255 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver itkBinaryDilateImageFilterTest)
itk_add_test(NAME itkBinaryDilateImageFilterTest2
      COMMAND ITKBinaryMathematicalMorphologyTestDriver itkBinaryDilateImageFilterTest2)
itk_add_test(NAME itkBinaryDilateImageFilterTest-100-0-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-0-0.png
    "6fd97580a84c71b381d220b132b1335a"
    itkBinaryDilateImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-0-0.png 100 0 0 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-100-0-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-100-0-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-0-1.png
    itkBinaryDilateImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-0-1.png 100 0 1 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-100-150-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-100-150-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-150-0.png
    itkBinaryDilateImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-150-0.png 100 150 0 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-100-150-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-100-150-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-150-1.png
    itkBinaryDilateImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-100-150-1.png 100 150 1 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-200-0-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-200-0-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-0-0.png
    itkBinaryDilateImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-0-0.png 200 0 0 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-200-0-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-200-0-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-0-1.png
    itkBinaryDilateImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-0-1.png 200 0 1 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-200-150-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-200-150-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-150-0.png
    itkBinaryDilateImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-150-0.png 200 150 0 10)
itk_add_test(NAME itkBinaryDilateImageFilterTest-200-150-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryDilateImageFilterTest-200-150-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-150-1.png
    itkBinaryDilateImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryDilateImageFilterTest-200-150-1.png 200 150 1 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver itkBinaryErodeImageFilterTest)
itk_add_test(NAME itkBinaryErodeImageFilterTest-100-0-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-100-0-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-0-0.png
    itkBinaryErodeImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-0-0.png 100 0 0 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-100-0-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-100-0-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-0-1.png
    itkBinaryErodeImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-0-1.png 100 0 1 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-100-150-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-100-150-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-150-0.png
    itkBinaryErodeImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-150-0.png 100 150 0 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-100-150-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-100-150-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-150-1.png
    itkBinaryErodeImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-100-150-1.png 100 150 1 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-200-0-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-200-0-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-0-0.png
    itkBinaryErodeImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-0-0.png 200 0 0 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-200-0-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-200-0-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-0-1.png
    itkBinaryErodeImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-0-1.png 200 0 1 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-200-150-0
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-200-150-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-150-0.png
    itkBinaryErodeImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-150-0.png 200 150 0 10)
itk_add_test(NAME itkBinaryErodeImageFilterTest-200-150-1
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBinaryErodeImageFilterTest-200-150-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-150-1.png
    itkBinaryErodeImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryErodeImageFilterTest-200-150-1.png 200 150 1 10)
itk_add_test(NAME itkBinaryMorphologicalClosingImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkBinaryMorphologicalClosingImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalClosingImageFilterTest.png
    itkBinaryMorphologicalClosingImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalClosingImageFilterTest.png 40 1 200)
itk_add_test(NAME itkBinaryMorphologicalClosingImageFilterTestUnsafe
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
    itkBinaryMorphologicalClosingImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png 40 0 200)
itk_add_test(NAME itkBinaryMorphologicalOpeningImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkBinaryMorphologicalOpeningImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalOpeningImageFilterTest.png
    itkBinaryMorphologicalOpeningImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryMorphologicalOpeningImageFilterTest.png 8 150 200)
itk_add_test(NAME itkBinaryOpeningByReconstructionImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkBinaryOpeningByReconstructionImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryOpeningByReconstructionImageFilterTest.png
    itkBinaryOpeningByReconstructionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1-bin.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryOpeningByReconstructionImageFilterTest.png 0 255 10)
itk_add_test(NAME itkBinaryThinningImageFilterTest
      COMMAND ITKBinaryMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/BinaryThinningImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/BinaryThinningImageFilterTest.png
    itkBinaryThinningImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Shapes.png} ${ITK_TEST_OUTPUT_DIR}/BinaryThinningImageFilterTest.png)

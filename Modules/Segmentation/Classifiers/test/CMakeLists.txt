itk_module_test()
set(ITK-ClassifiersTests
itkClassifiersHeaderTest.cxx
itkScalarImageKmeansImageFilterTest.cxx
itkSupervisedImageClassifierTest.cxx
itkBayesianClassifierImageFilterTest.cxx
itkImageClassifierFilterTest.cxx
itkSampleClassifierFilterTest1.cxx
itkSampleClassifierFilterTest2.cxx
itkSampleClassifierFilterTest3.cxx
itkSampleClassifierFilterTest4.cxx
itkSampleClassifierFilterTest5.cxx
itkSampleClassifierFilterTest6.cxx
itkSampleClassifierFilterTest7.cxx
)

CreateTestDriver(ITK-Classifiers  "${ITK-Classifiers-Test_LIBRARIES}" "${ITK-ClassifiersTests}")

add_test(NAME itkClassifiersHeaderTest
      COMMAND ITK-ClassifiersTestDriver itkClassifiersHeaderTest)
add_test(NAME itkBayesianClassifierImageFilterTest
      COMMAND ITK-ClassifiersTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/itkBayesianClassifierImageFilterTestLabelMap.png
              ${ITK_TEST_OUTPUT_DIR}/itkBayesianClassifierImageFilterTestLabelMap.png
    itkBayesianClassifierImageFilterTest ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkBayesianClassifierImageFilterTestLabelMap.png 4 2)
add_test(NAME itkImageClassifierFilterTest
      COMMAND ITK-ClassifiersTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Statistics/ImageClassifierFilterTestClassifiedImageTest.png
              ${ITK_TEST_OUTPUT_DIR}/ImageClassifierFilterTestClassifiedImage.png
    itkImageClassifierFilterTest ${ITK_TEST_OUTPUT_DIR}/ImageClassifierFilterTestClassifiedImage.png)
add_test(NAME itkSampleClassifierFilterTest1
      COMMAND ITK-ClassifiersTestDriver itkSampleClassifierFilterTest1)
add_test(NAME itkSampleClassifierFilterTest2
      COMMAND ITK-ClassifiersTestDriver itkSampleClassifierFilterTest2)
add_test(NAME itkSampleClassifierFilterTest3
      COMMAND ITK-ClassifiersTestDriver itkSampleClassifierFilterTest3)
add_test(NAME itkSampleClassifierFilterTest4
      COMMAND ITK-ClassifiersTestDriver itkSampleClassifierFilterTest4)
add_test(NAME itkSampleClassifierFilterTest5
      COMMAND ITK-ClassifiersTestDriver itkSampleClassifierFilterTest5)
add_test(NAME itkSampleClassifierFilterTest6
      COMMAND ITK-ClassifiersTestDriver itkSampleClassifierFilterTest6)
add_test(NAME itkSampleClassifierFilterTest7
      COMMAND ITK-ClassifiersTestDriver itkSampleClassifierFilterTest7
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussianTesting.dat)
add_test(NAME itkSupervisedImageClassifierTest
      COMMAND ITK-ClassifiersTestDriver itkSupervisedImageClassifierTest)

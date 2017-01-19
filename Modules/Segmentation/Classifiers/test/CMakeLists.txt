itk_module_test()
set(ITKClassifiersTests
itkScalarImageKmeansImageFilterTest.cxx
itkSupervisedImageClassifierTest.cxx
itkBayesianClassifierImageFilterTest.cxx
itkKmeansModelEstimatorTest.cxx
itkImageClassifierFilterTest.cxx
itkSampleClassifierFilterTest1.cxx
itkSampleClassifierFilterTest2.cxx
itkSampleClassifierFilterTest3.cxx
itkSampleClassifierFilterTest4.cxx
itkSampleClassifierFilterTest5.cxx
itkSampleClassifierFilterTest6.cxx
itkSampleClassifierFilterTest7.cxx
itkScalarImageKmeansImageFilter3DTest.cxx
)

CreateTestDriver(ITKClassifiers  "${ITKClassifiers-Test_LIBRARIES}" "${ITKClassifiersTests}")

itk_add_test(NAME itkBayesianClassifierImageFilterTest1
      COMMAND ITKClassifiersTestDriver
    --compare DATA{Baseline/itkBayesianClassifierImageFilterTestLabelMapNoPriors.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBayesianClassifierImageFilterTestLabelMapNoPriors.png
    itkBayesianClassifierImageFilterTest ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png
    ${ITK_TEST_OUTPUT_DIR}/itkBayesianClassifierImageFilterTestLabelMapNoPriors.png 4 2 0)
itk_add_test(NAME itkBayesianClassifierImageFilterTest2
      COMMAND ITKClassifiersTestDriver
    --compare DATA{Baseline/itkBayesianClassifierImageFilterTestLabelMapPriors.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBayesianClassifierImageFilterTestLabelMapPriors.png
    itkBayesianClassifierImageFilterTest ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png
    ${ITK_TEST_OUTPUT_DIR}/itkBayesianClassifierImageFilterTestLabelMapPriors.png 4 2 1)
itk_add_test(NAME itkKmeansModelEstimatorTest
      COMMAND ITKClassifiersTestDriver itkKmeansModelEstimatorTest)
itk_add_test(NAME itkImageClassifierFilterTest
      COMMAND ITKClassifiersTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Statistics/ImageClassifierFilterTestClassifiedImageTest.png}
              ${ITK_TEST_OUTPUT_DIR}/ImageClassifierFilterTestClassifiedImage.png
    itkImageClassifierFilterTest ${ITK_TEST_OUTPUT_DIR}/ImageClassifierFilterTestClassifiedImage.png)
itk_add_test(NAME itkSampleClassifierFilterTest1
      COMMAND ITKClassifiersTestDriver itkSampleClassifierFilterTest1)
itk_add_test(NAME itkSampleClassifierFilterTest2
      COMMAND ITKClassifiersTestDriver itkSampleClassifierFilterTest2)
itk_add_test(NAME itkSampleClassifierFilterTest3
      COMMAND ITKClassifiersTestDriver itkSampleClassifierFilterTest3)
itk_add_test(NAME itkSampleClassifierFilterTest4
      COMMAND ITKClassifiersTestDriver itkSampleClassifierFilterTest4)
itk_add_test(NAME itkSampleClassifierFilterTest5
      COMMAND ITKClassifiersTestDriver itkSampleClassifierFilterTest5)
itk_add_test(NAME itkSampleClassifierFilterTest6
      COMMAND ITKClassifiersTestDriver itkSampleClassifierFilterTest6)
itk_add_test(NAME itkSampleClassifierFilterTest7
      COMMAND ITKClassifiersTestDriver itkSampleClassifierFilterTest7
              DATA{${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat} DATA{${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussianTesting.dat})
itk_add_test(NAME itkSupervisedImageClassifierTest
      COMMAND ITKClassifiersTestDriver itkSupervisedImageClassifierTest)
itk_add_test(NAME itkScalarImageKmeansImageFilter3DTest
      COMMAND ITKClassifiersTestDriver
    --compare ${ITK_EXAMPLE_DATA_ROOT}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
    itkScalarImageKmeansImageFilter3DTest ${ITK_EXAMPLE_DATA_ROOT}/KmeansTest_T1UCharRaw.nii.gz ${ITK_EXAMPLE_DATA_ROOT}/KmeansTest_T1RawSkullStrip.nii.gz ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz)

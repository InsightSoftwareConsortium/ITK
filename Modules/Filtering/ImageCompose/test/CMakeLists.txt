itk_module_test()
set(ITK-ImageComposeTests
itkCompose2DCovariantVectorImageFilterTest.cxx
itkImageComposeHeaderTest.cxx
itkCompose3DVectorImageFilterTest.cxx
itkCompose3DCovariantVectorImageFilterTest.cxx
itkCompose2DVectorImageFilterTest.cxx
itkComposeRGBImageFilterTest.cxx
itkImageToVectorImageFilterTest.cxx
itkImageReadRealAndImaginaryWriteComplexTest.cxx
itkComposeRGBAImageFilterTest.cxx
itkJoinSeriesImageFilterTest.cxx
itkJoinSeriesImageFilterStreamingTest.cxx
itkJoinImageFilterTest.cxx
)

CreateTestDriver(ITK-ImageCompose  "${ITK-ImageCompose-Test_LIBRARIES}" "${ITK-ImageComposeTests}")

itk_add_test(NAME itkImageComposeHeaderTest
      COMMAND ITK-ImageComposeTestDriver itkImageComposeHeaderTest)
itk_add_test(NAME itkCompose2DCovariantVectorImageFilterTest
      COMMAND ITK-ImageComposeTestDriver itkCompose2DCovariantVectorImageFilterTest)
itk_add_test(NAME itkCompose3DVectorImageFilterTest
      COMMAND ITK-ImageComposeTestDriver itkCompose3DVectorImageFilterTest)
itk_add_test(NAME itkCompose3DCovariantVectorImageFilterTest
      COMMAND ITK-ImageComposeTestDriver itkCompose3DCovariantVectorImageFilterTest)
itk_add_test(NAME itkCompose2DVectorImageFilterTest
      COMMAND ITK-ImageComposeTestDriver itkCompose2DVectorImageFilterTest)
itk_add_test(NAME itkComposeRGBImageFilterTest
      COMMAND ITK-ImageComposeTestDriver itkComposeRGBImageFilterTest)
itk_add_test(NAME itkImageToVectorImageFilterTest
      COMMAND ITK-ImageComposeTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ImageToVectorImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ImageToVectorImageFilterTest.png
    itkImageToVectorImageFilterTest ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_TEST_OUTPUT_DIR}/ImageToVectorImageFilterTest.png)
itk_add_test(NAME itkImageReadRealAndImaginaryWriteComplexTest
      COMMAND ITK-ImageComposeTestDriver itkImageReadRealAndImaginaryWriteComplexTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_EXAMPLE_DATA_ROOT}/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplex.mha)
itk_add_test(NAME itkComposeRGBAImageFilterTest
      COMMAND ITK-ImageComposeTestDriver itkComposeRGBAImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkComposeRGBAImageFilterTestOutput.png ${ITK_DATA_ROOT}/Input/Number1inText.png ${ITK_DATA_ROOT}/Input/Number2inText.png ${ITK_DATA_ROOT}/Input/Number3inText.png ${ITK_DATA_ROOT}/Input/IntensityRamp64.png)
itk_add_test(NAME itkJoinSeriesImageFilterTest
      COMMAND ITK-ImageComposeTestDriver itkJoinSeriesImageFilterTest)
itk_add_test(NAME itkJoinSeriesImageFilterStreamingTest
      COMMAND ITK-ImageComposeTestDriver
    --compare ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkJoinSeriesImageFilterStreamingTest.mha
    itkJoinSeriesImageFilterStreamingTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkJoinSeriesImageFilterStreamingTest.mha)
itk_add_test(NAME itkJoinImageFilterTest
      COMMAND ITK-ImageComposeTestDriver itkJoinImageFilterTest)

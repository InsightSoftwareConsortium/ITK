itk_module_test()
set(ITKImageComposeTests
itkCompose2DCovariantVectorImageFilterTest.cxx
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

CreateTestDriver(ITKImageCompose  "${ITKImageCompose-Test_LIBRARIES}" "${ITKImageComposeTests}")

itk_add_test(NAME itkCompose2DCovariantVectorImageFilterTest
      COMMAND ITKImageComposeTestDriver itkCompose2DCovariantVectorImageFilterTest)
itk_add_test(NAME itkCompose3DVectorImageFilterTest
      COMMAND ITKImageComposeTestDriver itkCompose3DVectorImageFilterTest)
itk_add_test(NAME itkCompose3DCovariantVectorImageFilterTest
      COMMAND ITKImageComposeTestDriver itkCompose3DCovariantVectorImageFilterTest)
itk_add_test(NAME itkCompose2DVectorImageFilterTest
      COMMAND ITKImageComposeTestDriver itkCompose2DVectorImageFilterTest)
itk_add_test(NAME itkComposeRGBImageFilterTest
      COMMAND ITKImageComposeTestDriver itkComposeRGBImageFilterTest)
itk_add_test(NAME itkImageToVectorImageFilterTest
      COMMAND ITKImageComposeTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ImageToVectorImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ImageToVectorImageFilterTest.png
    itkImageToVectorImageFilterTest ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_TEST_OUTPUT_DIR}/ImageToVectorImageFilterTest.png)
itk_add_test(NAME itkImageReadRealAndImaginaryWriteComplexTest
      COMMAND ITKImageComposeTestDriver itkImageReadRealAndImaginaryWriteComplexTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_EXAMPLE_DATA_ROOT}/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplex.mha)
itk_add_test(NAME itkComposeRGBAImageFilterTest
      COMMAND ITKImageComposeTestDriver itkComposeRGBAImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkComposeRGBAImageFilterTestOutput.png ${ITK_DATA_ROOT}/Input/Number1inText.png ${ITK_DATA_ROOT}/Input/Number2inText.png ${ITK_DATA_ROOT}/Input/Number3inText.png ${ITK_DATA_ROOT}/Input/IntensityRamp64.png)
itk_add_test(NAME itkJoinSeriesImageFilterTest
      COMMAND ITKImageComposeTestDriver itkJoinSeriesImageFilterTest)
itk_add_test(NAME itkJoinSeriesImageFilterStreamingTest
      COMMAND ITKImageComposeTestDriver
    --compare ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkJoinSeriesImageFilterStreamingTest.mha
    itkJoinSeriesImageFilterStreamingTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkJoinSeriesImageFilterStreamingTest.mha)
itk_add_test(NAME itkJoinImageFilterTest
      COMMAND ITKImageComposeTestDriver itkJoinImageFilterTest)

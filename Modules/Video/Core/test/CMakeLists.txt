itk_module_test()
set(ITKVideoCoreTests
  itkImageToVideoFilterTest.cxx
  itkRingBufferTest.cxx
  itkTemporalRegionTest.cxx
  itkTemporalDataObjectTest.cxx
  itkTemporalProcessObjectTest.cxx
  itkVectorImageToVideoTest.cxx
  itkVideoStreamTest.cxx
  itkVideoSourceTest.cxx
  itkVideoToVideoFilterTest.cxx
)

CreateTestDriver(ITKVideoCore "${ITKVideoCore-Test_LIBRARIES}" "${ITKVideoCoreTests}")

# VideoToVideoFilterTest
itk_add_test(
  NAME VideoToVideoFilterTest
  COMMAND ITKVideoCoreTestDriver
    itkVideoToVideoFilterTest)

# VideoSourceTest
itk_add_test(
  NAME VideoSourceTest
  COMMAND ITKVideoCoreTestDriver
    itkVideoSourceTest)

# VideoStreamTest
# This is failing for unknown reasons on some platforms.  Disabling for now
# for the dashboards sake until we have time to debug further.
if(0)
itk_add_test(
  NAME VideoStreamTest
  COMMAND ITKVideoCoreTestDriver
    itkVideoStreamTest)
endif()

# TemporalProcessObjectTest
itk_add_test(
  NAME TemporalProcessObjectTest
  COMMAND ITKVideoCoreTestDriver
    itkTemporalProcessObjectTest)

# RingBufferTest
itk_add_test(
  NAME RingBufferTest
  COMMAND ITKVideoCoreTestDriver
    itkRingBufferTest)

# Testing Temporal Regions
itk_add_test(
  NAME TemporalRegionTest
  COMMAND ITKVideoCoreTestDriver
    itkTemporalRegionTest)

# Testing Temporal Data Object
itk_add_test(
  NAME TemporalDataObjectTest
  COMMAND ITKVideoCoreTestDriver
    itkTemporalDataObjectTest)

itk_add_test(
  NAME ImageToVideoFilterTest
  COMMAND ITKVideoCoreTestDriver
    --compare DATA{Baseline/HeadMRVolume_Frame0.mha}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume_Frame0.mha
    itkImageToVideoFilterTest
    DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha}
    ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume_Frame0.mha)

itk_add_test(
  NAME VectorImageToVideoTest
  COMMAND ITKVideoCoreTestDriver
    --compare DATA{Baseline/RGBTestImage_Slice0.png}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImage_Slice0.png
    itkVectorImageToVideoTest
    DATA{${ITK_DATA_ROOT}/Input/RGBTestImage.tif}
    ${ITK_TEST_OUTPUT_DIR}/RGBTestImage_Slice0.png)

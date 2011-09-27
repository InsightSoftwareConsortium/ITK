itk_module_test()
set(ITKVideoCoreTests
  itkRingBufferTest.cxx
  itkTemporalRegionTest.cxx
  itkTemporalDataObjectTest.cxx
  itkTemporalProcessObjectTest.cxx
  itkVideoStreamTest.cxx
  itkVideoSourceTest.cxx
  itkVideoToVideoFilterTest.cxx
)

CreateTestDriver(ITKVideoCore "${ITKVideoCore-Test_LIBRARIES}" "${ITKVideoCoreTests}" )

# VideoToVideoFilterTest
itk_add_test(
  NAME VideoToVideoFilterTest
  COMMAND ITKVideoCoreTestDriver
    itkVideoToVideoFilterTest )

# VideoSourceTest
itk_add_test(
  NAME VideoSourceTest
  COMMAND ITKVideoCoreTestDriver
    itkVideoSourceTest )

# VideoStreamTest
itk_add_test(
  NAME VideoStreamTest
  COMMAND ITKVideoCoreTestDriver
    itkVideoStreamTest )

# TemporalProcessObjectTest
itk_add_test(
  NAME TemporalProcessObjectTest
  COMMAND ITKVideoCoreTestDriver
    itkTemporalProcessObjectTest )

# RingBufferTest
itk_add_test(
  NAME RingBufferTest
  COMMAND ITKVideoCoreTestDriver
    itkRingBufferTest )

# Testing Temporal Regions
itk_add_test(
  NAME TemporalRegionTest
  COMMAND ITKVideoCoreTestDriver
    itkTemporalRegionTest )

# Testing Temporal Data Object
itk_add_test(
  NAME TemporalDataObjectTest
  COMMAND ITKVideoCoreTestDriver
    itkTemporalDataObjectTest )

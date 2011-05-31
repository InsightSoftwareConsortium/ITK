itk_module_test()
set(Video-Core-CommonTests
  itkRingBufferTest.cxx
  itkTemporalRegionTest.cxx
  itkTemporalDataObjectTest.cxx
  itkTemporalProcessObjectTest.cxx
  itkVideoStreamTest.cxx
  itkVideoSourceTest.cxx
  itkVideoToVideoFilterTest.cxx
)


# For adding midas test with "Video" label
include(${Video-Core-Common_SOURCE_DIR}/add_video_test.cmake)

CreateTestDriver(Video-Core-Common  "${Video-Core-Common-Test_LIBRARIES}" "${Video-Core-CommonTests}" )

# VideoToVideoFilterTest
add_video_test( NAME VideoToVideoFilterTest
                COMMAND Video-Core-CommonTestDriver itkVideoToVideoFilterTest )

# VideoSourceTest
add_video_test( NAME VideoSourceTest
                COMMAND Video-Core-CommonTestDriver itkVideoSourceTest )

# VideoStreamTest
add_video_test( NAME VideoStreamTest
                COMMAND Video-Core-CommonTestDriver itkVideoStreamTest )

# TemporalProcessObjectTest
add_video_test( NAME TemporalProcessObjectTest
                COMMAND Video-Core-CommonTestDriver itkTemporalProcessObjectTest )

# RingBufferTest
add_video_test( NAME RingBufferTest
                COMMAND Video-Core-CommonTestDriver itkRingBufferTest )

# Testing Temporal Regions
add_video_test( NAME TemporalRegionTest
                COMMAND Video-Core-CommonTestDriver itkTemporalRegionTest )

# Testing Temporal Data Object
add_video_test( NAME TemporalDataObjectTest
                COMMAND Video-Core-CommonTestDriver itkTemporalDataObjectTest )

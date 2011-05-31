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


# For Midas-stored testing data
include(${Video-Core-Common_SOURCE_DIR}/MIDAS.cmake)

CreateTestDriver(Video-Core-Common  "${Video-Core-Common-Test_LIBRARIES}" "${Video-Core-CommonTests}" )

# VideoToVideoFilterTest
add_test( NAME VideoToVideoFilterTest
          COMMAND Video-Core-CommonTestDriver itkVideoToVideoFilterTest )

# VideoSourceTest
add_test( NAME VideoSourceTest
          COMMAND Video-Core-CommonTestDriver itkVideoSourceTest )

# VideoStreamTest
add_test( NAME VideoStreamTest
          COMMAND Video-Core-CommonTestDriver itkVideoStreamTest )

# TemporalProcessObjectTest
add_test( NAME TemporalProcessObjectTest
          COMMAND Video-Core-CommonTestDriver itkTemporalProcessObjectTest )

# RingBufferTest
add_test( NAME RingBufferTest
          COMMAND Video-Core-CommonTestDriver itkRingBufferTest )

# Testing Temporal Regions
add_test( NAME TemporalRegionTest
          COMMAND Video-Core-CommonTestDriver itkTemporalRegionTest )

# Testing Temporal Data Object
add_test( NAME TemporalDataObjectTest
          COMMAND Video-Core-CommonTestDriver itkTemporalDataObjectTest )

itk_module_test()
set(Video-CommonTests
  itkRingBufferTest.cxx
  itkTemporalRegionTest.cxx
  itkTemporalDataObjectTest.cxx
  itkTemporalProcessObjectTest.cxx
  itkVideoStreamTest.cxx
  itkVideoSourceTest.cxx
  itkVideoToVideoFilterTest.cxx
)


# For Midas-stored testing data
include(${Video-Common_SOURCE_DIR}/MIDAS.cmake)

CreateTestDriver(Video-Common  "${Video-Common-Test_LIBRARIES}" "${Video-CommonTests}" )

# VideoToVideoFilterTest
add_test( NAME VideoToVideoFilterTest
          COMMAND Video-CommonTestDriver itkVideoToVideoFilterTest )

# VideoSourceTest
add_test( NAME VideoSourceTest
          COMMAND Video-CommonTestDriver itkVideoSourceTest )

# VideoStreamTest
add_test( NAME VideoStreamTest
          COMMAND Video-CommonTestDriver itkVideoStreamTest )

# TemporalProcessObjectTest
add_test( NAME TemporalProcessObjectTest
          COMMAND Video-CommonTestDriver itkTemporalProcessObjectTest )

# RingBufferTest
add_test( NAME RingBufferTest
          COMMAND Video-CommonTestDriver itkRingBufferTest )

# Testing Temporal Regions
add_test( NAME TemporalRegionTest
          COMMAND Video-CommonTestDriver itkTemporalRegionTest )

# Testing Temporal Data Object
add_test( NAME TemporalDataObjectTest
          COMMAND Video-CommonTestDriver itkTemporalDataObjectTest )

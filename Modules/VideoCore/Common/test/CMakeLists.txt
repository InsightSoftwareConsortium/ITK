itk_module_test()
set(Video-CommonTests
  itkRingBufferTest.cxx
  itkTemporalRegionTest.cxx
  itkTemporalDataObjectTest.cxx
  itkTemporalProcessObjectTest.cxx
)


# For Midas-stored testing data
include(${Video-Common_SOURCE_DIR}/MIDAS.cmake)

message( ${Video-Common-Test_LIBRARIES} )
CreateTestDriver(Video-Common  "${Video-Common-Test_LIBRARIES}" "${Video-CommonTests}" )

# TemporalProcessObjectTest
add_test( NAME TemporalProcessObjectTest
          COMMAND Video-CommonTestDriver itkTemporalProcessObjectTest )

# RingBufferTest
add_test( NAME RingBufferTest
          COMMAND Video-CommonTestDriver itkRingBufferTest )

# Testing Temporal Regions
add_test( NAME TemporalRegionTest
          COMMAND Video-CommonTestDriver itkTemporalRegionTest )

# Testing Temporal Regions
add_test( NAME TemporalDataObjectTest
          COMMAND Video-CommonTestDriver itkTemporalDataObjectTest )

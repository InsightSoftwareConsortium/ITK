itk_module_test()
set(Video-CommonTests
  itkRingBufferTest.cxx
  itkTemporalRegionTest.cxx
)


# For Midas-stored testing data
include(${Video-Common_SOURCE_DIR}/MIDAS.cmake)

CreateTestDriver(Video-Common  "${Video-Common-Test_LIBRARIES}" "${Video-CommonTests}")

# RingBufferTest
add_test( NAME RingBufferTest
          COMMAND Video-CommonTestDriver itkRingBufferTest )

# Testing Temporal Regions
add_test( NAME itkTemporalRegionTest
          COMMAND Video-CommonTestDriver itkTemporalRegionTest )

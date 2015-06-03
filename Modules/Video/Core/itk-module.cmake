set(DOCUMENTATION "This module contains base classes for processing data that
possesses a temporal element. The classes contained in this module extend the
traditional data object and process objects to handle time properly and also
contain convenience classes for passing temporal regions and dealing with ITK
images over time in the form of VideoStreams and VideoToVideoFilters.")

itk_module(ITKVideoCore
  ENABLE_SHARED
  DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
    ITKVideoIO
  DESCRIPTION
    "${DOCUMENTATION}"
)


PROJECT(FilteringExamples)

ADD_EXECUTABLE(GradientMagnitudeImageFilter GradientMagnitudeImageFilter.cxx )
TARGET_LINK_LIBRARIES(GradientMagnitudeImageFilter ITKCommon ITKIO ITKMetaIO itkzlib itkpng)




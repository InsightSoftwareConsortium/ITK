set(DOCUMENTATION "The Statistics module contains basic data structures,
statistical algorithms, and a classification for general statistical analysis
and classification problems.  This includes, for examples, classes for
calculating histograms, calculating sample statistics, creating decision rules,
or for performing statistical pattern classification.  Statistics are calculated
on a Sample, which contains MeasurementVector's.")

itk_module(ITKStatistics
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKNetlib
  TEST_DEPENDS
    ITKTestKernel
    ITKImageIntensity
    ITKImageCompose
    ITKIOImageBase
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra test dependency of ImageIntensity is introduced by itkImageToListSampleAdaptorTest.cxx.

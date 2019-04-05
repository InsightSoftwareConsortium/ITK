itk_module_test()
set(ITKImageFrequencyTests
    #### Frequency Manipulation ###
    # Iterator with Index.
    itkFrequencyFFTLayoutImageRegionIteratorWithIndexTest.cxx
    # BandPass Filter
    itkFrequencyBandImageFilterTest.cxx
  )

CreateTestDriver(ITKImageFrequency "${ITKImageFrequency-Test_LIBRARIES}" "${ITKImageFrequencyTests}")

# FrequencyIterator
itk_add_test(NAME itkFrequencyFFTLayoutImageRegionIteratorWithIndexTest
  COMMAND ITKImageFrequencyTestDriver itkFrequencyFFTLayoutImageRegionIteratorWithIndexTest)
## BandPass
itk_add_test(NAME itkFrequencyBandImageFilterEvenTest
  COMMAND ITKImageFrequencyTestDriver itkFrequencyBandImageFilterTest "Even")
itk_add_test(NAME itkFrequencyBandImageFilterOddTest
  COMMAND ITKImageFrequencyTestDriver itkFrequencyBandImageFilterTest "Odd")

set(${itk-module}GTests
  itkFrequencyIteratorsGTest.cxx
  )

CreateGoogleTestDriver(${itk-module} "${${itk-module}-Test_LIBRARIES}" "${${itk-module}GTests}")

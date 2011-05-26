itk_module_test()
set(ITK-RegistrationCommonTests
itkMultiResolutionImageRegistrationMethodTest_1.cxx
itkMultiResolutionImageRegistrationMethodTest_2.cxx
itkImageRegistrationMethodTest_10.cxx
itkImageRegistrationMethodTest_11.cxx
itkImageRegistrationMethodTest_12.cxx
itkImageRegistrationMethodTest_13.cxx
itkImageRegistrationMethodTest_14.cxx
itkImageRegistrationMethodTest_15.cxx
itkImageRegistrationMethodTest_16.cxx
itkMeanSquaresPointSetToImageMetricTest.cxx
itkGradientDifferenceImageToImageMetricTest.cxx
itkHistogramImageToImageMetricTest.cxx
itkMutualInformationHistogramImageToImageMetricTest.cxx
itkMeanReciprocalSquareDifferencePointSetToImageMetricTest.cxx
itkRegistrationCommonHeaderTest.cxx
itkMultiResolutionImageRegistrationMethodTest.cxx
itkCompareHistogramImageToImageMetricTest.cxx
itkMeanSquaresHistogramImageToImageMetricTest.cxx
itkImageRegistrationMethodTest.cxx
itkCorrelationCoefficientHistogramImageToImageMetricTest.cxx
itkNormalizedCorrelationPointSetToImageMetricTest.cxx
itkKullbackLeiblerCompareHistogramImageToImageMetricTest.cxx
itkNormalizedMutualInformationHistogramImageToImageMetricTest.cxx
itkPointSetToSpatialObjectDemonsRegistrationTest.cxx
itkKappaStatisticImageToImageMetricTest.cxx
itkMattesMutualInformationImageToImageMetricTest.cxx
itkMatchCardinalityImageToImageMetricTest.cxx
itkMultiResolutionPyramidImageFilterTest.cxx
itkImageRegistrationMethodTest_1.cxx
itkImageRegistrationMethodTest_2.cxx
itkImageRegistrationMethodTest_3.cxx
itkImageRegistrationMethodTest_4.cxx
itkImageRegistrationMethodTest_5.cxx
itkImageRegistrationMethodTest_6.cxx
itkImageRegistrationMethodTest_7.cxx
itkImageRegistrationMethodTest_8.cxx
itkImageRegistrationMethodTest_9.cxx
itkRecursiveMultiResolutionPyramidImageFilterTest.cxx
)

CreateTestDriver(ITK-RegistrationCommon  "${ITK-RegistrationCommon-Test_LIBRARIES}" "${ITK-RegistrationCommonTests}")

add_test(NAME itkRegistrationCommonHeaderTest
      COMMAND ITK-RegistrationCommonTestDriver itkRegistrationCommonHeaderTest)
add_test(NAME itkMultiResolutionImageRegistrationMethodTest_1
      COMMAND ITK-RegistrationCommonTestDriver itkMultiResolutionImageRegistrationMethodTest_1)
add_test(NAME itkMultiResolutionImageRegistrationMethodTest_2
      COMMAND ITK-RegistrationCommonTestDriver itkMultiResolutionImageRegistrationMethodTest_2)
add_test(NAME itkImageRegistrationMethodTest_10
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_10)
add_test(NAME itkImageRegistrationMethodTest_11
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_11)
add_test(NAME itkImageRegistrationMethodTest_12
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_12)
add_test(NAME itkImageRegistrationMethodTest_13
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_13)
add_test(NAME itkImageRegistrationMethodTest_14
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_14)
add_test(NAME itkImageRegistrationMethodTest_15
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_15)
add_test(NAME itkImageRegistrationMethodTest_16
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_16)
add_test(NAME itkMeanSquaresPointSetToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkMeanSquaresPointSetToImageMetricTest)
add_test(NAME itkGradientDifferenceImageToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkGradientDifferenceImageToImageMetricTest)
add_test(NAME itkHistogramImageToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkHistogramImageToImageMetricTest)
add_test(NAME itkMutualInformationHistogramImageToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkMutualInformationHistogramImageToImageMetricTest)
add_test(NAME itkMeanReciprocalSquareDifferencePointSetToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkMeanReciprocalSquareDifferencePointSetToImageMetricTest)
add_test(NAME itkMultiResolutionImageRegistrationMethodTest
      COMMAND ITK-RegistrationCommonTestDriver itkMultiResolutionImageRegistrationMethodTest)
add_test(NAME itkCompareHistogramImageToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkCompareHistogramImageToImageMetricTest)
add_test(NAME itkMeanSquaresHistogramImageToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkMeanSquaresHistogramImageToImageMetricTest)
add_test(NAME itkImageRegistrationMethodTest
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest)
add_test(NAME itkCorrelationCoefficientHistogramImageToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkCorrelationCoefficientHistogramImageToImageMetricTest)
add_test(NAME itkNormalizedCorrelationPointSetToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkNormalizedCorrelationPointSetToImageMetricTest)
add_test(NAME itkKullbackLeiblerCompareHistogramImageToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkKullbackLeiblerCompareHistogramImageToImageMetricTest)
add_test(NAME itkNormalizedMutualInformationHistogramImageToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkNormalizedMutualInformationHistogramImageToImageMetricTest)
add_test(NAME itkPointSetToSpatialObjectDemonsRegistrationTest
      COMMAND ITK-RegistrationCommonTestDriver itkPointSetToSpatialObjectDemonsRegistrationTest)
add_test(NAME itkKappaStatisticImageToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkKappaStatisticImageToImageMetricTest
              ${ITK_DATA_ROOT}/Input/Spots.png)
add_test(NAME itkMattesMutualInformationImageToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkMattesMutualInformationImageToImageMetricTest
              1 1)
add_test(NAME itkMattesMutualInformationImageToImageMetricTest2
      COMMAND ITK-RegistrationCommonTestDriver itkMattesMutualInformationImageToImageMetricTest
              1 0)
add_test(NAME itkMattesMutualInformationImageToImageMetricTest3
      COMMAND ITK-RegistrationCommonTestDriver itkMattesMutualInformationImageToImageMetricTest
              0 1)
add_test(NAME itkMattesMutualInformationImageToImageMetricTest4
      COMMAND ITK-RegistrationCommonTestDriver itkMattesMutualInformationImageToImageMetricTest
              0 0)
add_test(NAME itkMatchCardinalityImageToImageMetricTest
      COMMAND ITK-RegistrationCommonTestDriver itkMatchCardinalityImageToImageMetricTest
              ${ITK_DATA_ROOT}/Input/Spots.png)
add_test(NAME itkMultiResolutionPyramidImageFilterWithResampleFilterTest
      COMMAND ITK-RegistrationCommonTestDriver itkMultiResolutionPyramidImageFilterTest
              Resample)
add_test(NAME itkMultiResolutionPyramidImageFilterWithShrinkFilterTest
      COMMAND ITK-RegistrationCommonTestDriver itkMultiResolutionPyramidImageFilterTest
              Shrink)
add_test(NAME itkRecursiveMultiResolutionPyramidImageFilterWithResampleFilterTest2
      COMMAND ITK-RegistrationCommonTestDriver itkMultiResolutionPyramidImageFilterTest
              Resample TestRecursive)
add_test(NAME itkRecursiveMultiResolutionPyramidImageFilterWithShrinkFilterTest2
      COMMAND ITK-RegistrationCommonTestDriver itkMultiResolutionPyramidImageFilterTest
              Shrink TestRecursive)
add_test(NAME itkImageRegistrationMethodTest_1
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_1)
add_test(NAME itkImageRegistrationMethodTest_2
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_2)
add_test(NAME itkImageRegistrationMethodTest_3
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_3)
add_test(NAME itkImageRegistrationMethodTest_4
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_4)
add_test(NAME itkImageRegistrationMethodTest_5
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_5)
add_test(NAME itkImageRegistrationMethodTest_6
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_6)
add_test(NAME itkImageRegistrationMethodTest_7
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_7)
add_test(NAME itkImageRegistrationMethodTest_8
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_8)
add_test(NAME itkImageRegistrationMethodTest_9
      COMMAND ITK-RegistrationCommonTestDriver itkImageRegistrationMethodTest_9)
add_test(NAME itkRecursiveMultiResolutionPyramidImageFilterWithResampleFilterTest
      COMMAND ITK-RegistrationCommonTestDriver itkRecursiveMultiResolutionPyramidImageFilterTest
              Resample)
add_test(NAME itkRecursiveMultiResolutionPyramidImageFilterWithShrinkFilterTest
      COMMAND ITK-RegistrationCommonTestDriver itkRecursiveMultiResolutionPyramidImageFilterTest
              Shrink)

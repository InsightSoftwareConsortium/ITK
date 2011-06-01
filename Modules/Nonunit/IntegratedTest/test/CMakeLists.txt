itk_module_test()
set(ITK-IntegratedTestTests
itkFilterImageAddTest.cxx
itkFloodFilledSpatialFunctionTest.cxx
itkFloodFillIteratorTest.cxx
itkGaussianDistributionTest.cxx
itkGaussianMembershipFunctionTest.cxx
itkGaussianMixtureModelComponentTest.cxx
itkGibbsTest.cxx
itkGrayscaleGeodesicErodeDilateImageFilterTest.cxx
itkHashTableTest.cxx
itkHConvexConcaveImageFilterTest.cxx
itkHessianRecursiveGaussianFilterScaleSpaceTest.cxx
itkHessianRecursiveGaussianFilterTest.cxx
itkHMaximaMinimaImageFilterTest.cxx
itkHoughTransform2DCirclesImageTest.cxx
itkHoughTransform2DLinesImageTest.cxx
itkImageAdaptorPipeLineTest.cxx
itkImageComputeOffsetAndIndexTest.cxx
itkImageDuplicatorTest.cxx
itkImageIteratorsForwardBackwardTest.cxx
itkImageLinearIteratorTest.cxx
itkImageMomentsTest.cxx
itkImageRandomIteratorTest.cxx
itkImageRandomIteratorTest2.cxx
itkImageRandomNonRepeatingIteratorWithIndexTest.cxx
itkImageRandomNonRepeatingIteratorWithIndexTest2.cxx
itkImageSliceIteratorTest.cxx
itkImageToHistogramFilterTest.cxx
itkImageToHistogramFilterTest2.cxx
itkImageToHistogramFilterTest3.cxx
itkImageToListSampleAdaptorTest.cxx
itkImageToListSampleAdaptorTest2.cxx
itkImageToListSampleFilterTest.cxx
itkImageToListSampleFilterTest2.cxx
itkImageToListSampleFilterTest3.cxx
itkImportContainerTest.cxx
itkImportImageTest.cxx
itkInterpolateTest.cxx
itkIOPluginTest.cxx
itkIteratorTests.cxx
itkJoinSeriesImageFilterStreamingTest.cxx
itkJointDomainImageToListSampleAdaptorTest.cxx
itkKdTreeBasedKmeansEstimatorTest.cxx
itkKdTreeGeneratorTest.cxx
itkKdTreeTest1.cxx
itkKdTreeTest2.cxx
itkKdTreeTestSamplePoints.cxx
itkKmeansModelEstimatorTest.cxx
itkLargeImageWriteConvertReadTest.cxx
itkLargeImageWriteReadTest.cxx
itkListSampleTest.cxx
itkMathCastWithRangeCheckTest.cxx
itkMathematicalMorphologyImageFilterTest.cxx
itkMathRoundProfileTest1.cxx
itkMathRoundTest.cxx
itkMathRoundTest2.cxx
itkMatrixImageWriteReadTest.cxx
itkMeanReciprocalSquareDifferenceImageMetricTest.cxx
itkMeanSampleFilterTest.cxx
itkMeanSampleFilterTest2.cxx
itkMeanSampleFilterTest3.cxx
itkMeanSquaresImageMetricTest.cxx
itkMeasurementVectorTraitsTest.cxx
itkMedialNodeCorrespondencesTest.cxx
itkMembershipFunctionBaseTest.cxx
itkMembershipFunctionBaseTest2.cxx
itkMembershipSampleTest1.cxx
itkMembershipSampleTest2.cxx
itkMembershipSampleTest3.cxx
itkMembershipSampleTest4.cxx
itkMemoryLeakTest.cxx
itkMirrorPadImageTest.cxx
itkMixtureModelComponentBaseTest.cxx
itkModifiedTimeTest.cxx
itkMutualInformationMetricTest.cxx
itkNeighborhoodSamplerTest1.cxx
itkNewTest.cxx
itkNormalVariateGeneratorTest1.cxx
itkNumericsPrintTest.cxx
itkNumericsTest.cxx
itkOptimizersHierarchyTest.cxx
itkPixelAccessTest.cxx
itkPointGeometryTest.cxx
itkPointSetToImageRegistrationTest_1.cxx
itkPointSetToListSampleAdaptorTest.cxx
itkPointSetToPointSetRegistrationTest.cxx
itkPriorityQueueTest.cxx
itkProbabilityDistributionTest.cxx
itkProjectionImageFilterTest.cxx
itkPromoteDimensionImageTest.cxx
itkPushPopTileImageFilterTest.cxx
itkRandomVariateGeneratorBaseTest.cxx
itkReadWriteImageWithDictionaryTest.cxx
itkReadWriteSpatialObjectTest.cxx
itkRecursiveGaussianImageFiltersOnTensorsTest.cxx
itkRecursiveGaussianImageFiltersOnVectorImageTest.cxx
itkRecursiveGaussianImageFiltersTest.cxx
itkRecursiveGaussianScaleSpaceTest1.cxx
itkReleaseDataFilterTest.cxx
itkRemoveBoundaryObjectsTest.cxx
itkRemoveBoundaryObjectsTest2.cxx
itkResampleImageTest.cxx
itkResampleImageTest2.cxx
itkResamplePhasedArray3DSpecialCoordinatesImageTest.cxx
itkRGBInterpolateImageFunctionTest.cxx
itkSampleTest.cxx
itkSampleTest2.cxx
itkSampleTest3.cxx
itkSampleTest4.cxx
itkSampleToHistogramFilterTest.cxx
itkSampleToHistogramFilterTest2.cxx
itkSampleToHistogramFilterTest3.cxx
itkSampleToHistogramFilterTest4.cxx
itkSampleToHistogramFilterTest5.cxx
itkSampleToHistogramFilterTest6.cxx
itkSampleToHistogramFilterTest7.cxx
itkSampleToSubsampleFilterTest1.cxx
itkScalarImageKmeansImageFilter3DTest.cxx
itkScalarImageToCooccurrenceListSampleFilterTest.cxx
itkScalarImageToCooccurrenceMatrixFilterTest.cxx
itkScalarImageToCooccurrenceMatrixFilterTest2.cxx
itkScalarImageToTextureFeaturesFilterTest.cxx
itkShrinkImagePreserveObjectPhysicalLocations.cxx
itkShrinkImageStreamingTest.cxx
itkShrinkImageTest.cxx
itkSimpleFilterWatcherTest.cxx
itkSparseFrequencyContainer2Test.cxx
itkSparseImageTest.cxx
itkSpatialObjectToImageRegistrationTest.cxx
itkStandardDeviationPerComponentSampleFilterTest.cxx
itkStatisticsAlgorithmTest.cxx
itkStatisticsAlgorithmTest2.cxx
itkStatisticsTypesTest.cxx
itkStatisticsPrintTest.cxx
itkSTLThreadTest.cxx
itkStreamingImageFilterTest.cxx
itkStreamingImageFilterTest2.cxx
itkStreamingImageFilterTest3.cxx
itkSubsampleTest.cxx
itkSubsampleTest2.cxx
itkSubsampleTest3.cxx
itkSymmetricSecondRankTensorImageReadTest.cxx
itkSymmetricSecondRankTensorImageWriteReadTest.cxx
itkTDistributionTest.cxx
itkThreadDefsTest.cxx
itkTimeProbesTest.cxx
itkTopHatImageFilterTest.cxx
itkVectorGeometryTest.cxx
itkVectorImageReadWriteTest.cxx
itkVectorImageTest.cxx
itkVNLRoundProfileTest1.cxx
itkZeroFluxBoundaryConditionTest.cxx
QPropXORTest1.cxx
RBFTest1.cxx
XORTest1.cxx
XORTest2.cxx
itkCommonPrintTest.cxx
itkIOPrintTest.cxx
itkBasicFiltersPrintTest.cxx
itkNeuralNetworksPrintTest.cxx
itkSpatialObjectPrintTest.cxx
)

CreateTestDriver_SupportBuildInIOFactories(ITK-IntegratedTest  "${ITK-IntegratedTest-Test_LIBRARIES}" "${ITK-IntegratedTestTests}")

# temporary solution for all print tests. Each module should have its own print test later.
add_executable(itkAlgorithmsPrintTest  itkAlgorithmsPrintTest.cxx)
itk_module_target_label(itkAlgorithmsPrintTest)
target_link_libraries(itkAlgorithmsPrintTest ${ITK-IntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest2  itkAlgorithmsPrintTest2.cxx)
itk_module_target_label(itkAlgorithmsPrintTest2)
target_link_libraries(itkAlgorithmsPrintTest2 ${ITK-IntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest2 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest3  itkAlgorithmsPrintTest3.cxx)
itk_module_target_label(itkAlgorithmsPrintTest3)
target_link_libraries(itkAlgorithmsPrintTest3 ${ITK-IntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest3 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest4  itkAlgorithmsPrintTest4.cxx)
itk_module_target_label(itkAlgorithmsPrintTest4)
target_link_libraries(itkAlgorithmsPrintTest4 ${ITK-IntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest4 COMMAND itkAlgorithmsPrintTest)

#-------------------------------------------------------------------------
itk_add_test(NAME itkStatisticsPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsPrintTest)
itk_add_test(NAME itkSpatialObjectPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkSpatialObjectPrintTest)
itk_add_test(NAME itkIOPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkIOPrintTest)
itk_add_test(NAME itkCommonPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkCommonPrintTest)
itk_add_test(NAME itkBasicFiltersPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkBasicFiltersPrintTest)
itk_add_test(NAME itkFloodFilledSpatialFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkFloodFilledSpatialFunctionTest)
itk_add_test(NAME itkFloodFillIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkFloodFillIteratorTest)
itk_add_test(NAME itkGaussianDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkGaussianDistributionTest)
itk_add_test(NAME itkGaussianMembershipFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkGaussianMembershipFunctionTest)
itk_add_test(NAME itkGaussianMixtureModelComponentTest
      COMMAND ITK-IntegratedTestTestDriver itkGaussianMixtureModelComponentTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
itk_add_test(NAME itkGibbsTest
      COMMAND ITK-IntegratedTestTestDriver itkGibbsTest)
itk_add_test(NAME itkGrayscaleGeodesicErodeDilateImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png
    itkGrayscaleGeodesicErodeDilateImageFilterTest ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png 35)
itk_add_test(NAME itkHashTableTest
      COMMAND ITK-IntegratedTestTestDriver itkHashTableTest)
itk_add_test(NAME itkHConvexConcaveImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HConvexConcaveImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png
    itkHConvexConcaveImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png 50)
itk_add_test(NAME itkHessianRecursiveGaussianFilterScaleSpaceTest
      COMMAND ITK-IntegratedTestTestDriver itkHessianRecursiveGaussianFilterScaleSpaceTest)
itk_add_test(NAME itkHessianRecursiveGaussianFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkHessianRecursiveGaussianFilterTest)
itk_add_test(NAME itkHMaximaMinimaImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png
    itkHMaximaMinimaImageFilterTest ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png 35)
itk_add_test(NAME itkHMaximaMinimaImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png
    itkHMaximaMinimaImageFilterTest ${ITK_DATA_ROOT}/Input/cake_hard.png ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png 35)
itk_add_test(NAME itkHoughTransform2DCirclesImageTest
      COMMAND ITK-IntegratedTestTestDriver itkHoughTransform2DCirclesImageTest)
itk_add_test(NAME itkHoughTransform2DLinesImageTest
      COMMAND ITK-IntegratedTestTestDriver itkHoughTransform2DLinesImageTest)
itk_add_test(NAME itkImageAdaptorPipeLineTest
      COMMAND ITK-IntegratedTestTestDriver itkImageAdaptorPipeLineTest)
itk_add_test(NAME itkImageDuplicatorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageDuplicatorTest)

itk_add_test(NAME itkImageIteratorsForwardBackwardTest
      COMMAND ITK-IntegratedTestTestDriver itkImageIteratorsForwardBackwardTest)
itk_add_test(NAME itkImageLinearIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageLinearIteratorTest)
itk_add_test(NAME itkImageMomentsTest
      COMMAND ITK-IntegratedTestTestDriver itkImageMomentsTest)
itk_add_test(NAME itkImageRandomIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomIteratorTest)
itk_add_test(NAME itkImageRandomIteratorTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomIteratorTest2
              ${ITK_TEST_OUTPUT_DIR}/itkImageRandomIteratorTest2Output.mha)
itk_add_test(NAME itkImageRandomNonRepeatingIteratorWithIndexTest
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomNonRepeatingIteratorWithIndexTest)
itk_add_test(NAME itkImageRandomNonRepeatingIteratorWithIndexTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomNonRepeatingIteratorWithIndexTest2)
itk_add_test(NAME itkImageSliceIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageSliceIteratorTest)
itk_add_test(NAME itkImageToHistogramFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkImageToHistogramFilterTest)
itk_add_test(NAME itkImageToHistogramFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageToHistogramFilterTest2
              ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/itkImageToHistogramFilterTest2.txt)
itk_add_test(NAME itkImageToHistogramFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkImageToHistogramFilterTest3
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkImageToHistogramFilterTest3.txt)
itk_add_test(NAME itkImageToListSampleAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleAdaptorTest)
itk_add_test(NAME itkImageToListSampleAdaptorTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleAdaptorTest2)
itk_add_test(NAME itkImageToListSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleFilterTest)
itk_add_test(NAME itkImageToListSampleFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleFilterTest2)
itk_add_test(NAME itkImageToListSampleFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleFilterTest3)
itk_add_test(NAME itkImportContainerTest
      COMMAND ITK-IntegratedTestTestDriver itkImportContainerTest)
itk_add_test(NAME itkImportImageTest
      COMMAND ITK-IntegratedTestTestDriver itkImportImageTest)
itk_add_test(NAME itkInterpolateTest
      COMMAND ITK-IntegratedTestTestDriver itkInterpolateTest)

if(ITK_BUILD_SHARED_LIBS)
  itk_add_test(NAME itkIOPluginTest
           COMMAND ITK-IntegratedTestTestDriver itkIOPluginTest
             ${ITK_TEST_OUTPUT_DIR}
             "FileFreeIO::Size=128,256:Spacing=.5,.8:Origin=5,6:Direction=-1,0,0,-1"
             ${ITK_TEST_OUTPUT_DIR}/itkIOPluginTest.png
    )
  ## Create a library to test ITK IO plugins
  set(FileFreeTest_Source itkFileFreeImageIO.cxx itkFileFreeImageIOFactory.cxx)
  add_library(FileFreeTestLib MODULE ${FileFreeTest_Source})
  itk_module_target_label(FileFreeTestLib)
  target_link_libraries(FileFreeTestLib ${ITK-IO-Base_LIBRARIES})
  set_property(TARGET FileFreeTestLib PROPERTY LIBRARY_OUTPUT_DIRECTORY ${ITK_TEST_OUTPUT_DIR})
  add_dependencies(ITK-IntegratedTestTestDriver FileFreeTestLib)
endif(ITK_BUILD_SHARED_LIBS)


itk_add_test(NAME itkIteratorTests
      COMMAND ITK-IntegratedTestTestDriver itkIteratorTests)
itk_add_test(NAME itkJoinSeriesImageFilterStreamingTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkJoinSeriesImageFilterStreamingTest.mha
    itkJoinSeriesImageFilterStreamingTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkJoinSeriesImageFilterStreamingTest.mha)
itk_add_test(NAME itkJointDomainImageToListSampleAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkJointDomainImageToListSampleAdaptorTest)
itk_add_test(NAME itkKdTreeBasedKmeansEstimatorTest
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeBasedKmeansEstimatorTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat 1 28.54746 0.07)
itk_add_test(NAME itkKdTreeGeneratorTest
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeGeneratorTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
itk_add_test(NAME itkKdTreeTest1
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              100 100 1 ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest1_100.dot)
itk_add_test(NAME itkKdTreeTest4
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 1)
itk_add_test(NAME itkKdTreeTest5
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 2)
itk_add_test(NAME itkKdTreeTest6
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 3)
itk_add_test(NAME itkKdTreeTest7
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 5)
itk_add_test(NAME itkKdTreeTest8
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 10)
itk_add_test(NAME itkKdTreeTest9
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 20)
itk_add_test(NAME itkKdTreeTest10
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 100)
itk_add_test(NAME itkKdTreeTest2
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest2
              ${ITK_DATA_ROOT}/Input/pointsForKdTree.txt 1 ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest2.dot)
itk_add_test(NAME itkKdTreeTest3
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest2
              ${ITK_DATA_ROOT}/Input/pointsForKdTree2.txt 1 ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest3.dot)

#
#  The following tests generate graphical plots of the KdTree organization.
#  They are very helpful for debugging and verification purposes, but not so
#  much for regression testing. They need the "dot" tool that is part of the
#  Graphviz package. The dot executable is found as part of the process of
#  configuring the Doxygen documentation. Therefore, to enable these tests
#  below, you must enable BUILD_DOXYGEN and BUILD_DOCUMENTATION.
#
if(DOXYGEN_DOT_EXECUTABLE)
itk_add_test(NAME itkKdTreeTest1Plot
      COMMAND ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest1_100.png ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest1_100.dot)
set_tests_properties( itkKdTreeTest1Plot PROPERTIES DEPENDS itkKdTreeTest1 )
itk_add_test(NAME itkKdTreeTest2Plot
      COMMAND ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest2.png ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest2.dot)
set_tests_properties( itkKdTreeTest2Plot PROPERTIES DEPENDS itkKdTreeTest2 )
itk_add_test(NAME itkKdTreeTest3Plot
      COMMAND ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest3.png ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest3.dot)
set_tests_properties( itkKdTreeTest3Plot PROPERTIES DEPENDS itkKdTreeTest3 )
endif(DOXYGEN_DOT_EXECUTABLE)

itk_add_test(NAME itkKdTreeTestSamplePoints
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTestSamplePoints)
itk_add_test(NAME itkKmeansModelEstimatorTest
      COMMAND ITK-IntegratedTestTestDriver itkKmeansModelEstimatorTest)
itk_add_test(NAME itkListSampleTest1
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              1)
itk_add_test(NAME itkListSampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              5)
itk_add_test(NAME itkListSampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              10)
itk_add_test(NAME itkListSampleTest4
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              100)
itk_add_test(NAME itkMathCastWithRangeCheckTest
      COMMAND ITK-IntegratedTestTestDriver itkMathCastWithRangeCheckTest)
itk_add_test(NAME itkMathematicalMorphologyImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkMathematicalMorphologyImageFilterTest)
itk_add_test(NAME itkMathRoundProfileTest1
      COMMAND ITK-IntegratedTestTestDriver itkMathRoundProfileTest1)
itk_add_test(NAME itkMathRoundTest
      COMMAND ITK-IntegratedTestTestDriver itkMathRoundTest)
itk_add_test(NAME itkMathRoundTest2
      COMMAND ITK-IntegratedTestTestDriver itkMathRoundTest2)
itk_add_test(NAME itkMatrixImageWriteReadTest
      COMMAND ITK-IntegratedTestTestDriver itkMatrixImageWriteReadTest
              ${ITK_TEST_OUTPUT_DIR}/testMatrix1.mha)
itk_add_test(NAME itkMeanReciprocalSquareDifferenceImageMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkMeanReciprocalSquareDifferenceImageMetricTest)
itk_add_test(NAME itkMeanSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkMeanSampleFilterTest)
itk_add_test(NAME itkMeanSampleFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkMeanSampleFilterTest2)
itk_add_test(NAME itkMeanSampleFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkMeanSampleFilterTest3)
itk_add_test(NAME itkMeanSquaresImageMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkMeanSquaresImageMetricTest)
itk_add_test(NAME itkMeasurementVectorTraitsTest
      COMMAND ITK-IntegratedTestTestDriver itkMeasurementVectorTraitsTest)
itk_add_test(NAME itkMedialNodeCorrespondencesTest
      COMMAND ITK-IntegratedTestTestDriver itkMedialNodeCorrespondencesTest)
itk_add_test(NAME itkMembershipFunctionBaseTest
      COMMAND ITK-IntegratedTestTestDriver itkMembershipFunctionBaseTest)
itk_add_test(NAME itkMembershipFunctionBaseTest2
      COMMAND ITK-IntegratedTestTestDriver itkMembershipFunctionBaseTest2)
itk_add_test(NAME itkMembershipSampleTest1
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest1)
itk_add_test(NAME itkMembershipSampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest2)
itk_add_test(NAME itkMembershipSampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest3)
itk_add_test(NAME itkMembershipSampleTest4
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest4)
itk_add_test(NAME itkMirrorPadImageTest
      COMMAND ITK-IntegratedTestTestDriver itkMirrorPadImageTest)
itk_add_test(NAME itkMixtureModelComponentBaseTest
      COMMAND ITK-IntegratedTestTestDriver itkMixtureModelComponentBaseTest)
itk_add_test(NAME itkModifiedTimeTest
      COMMAND ITK-IntegratedTestTestDriver itkModifiedTimeTest)
itk_add_test(NAME itkMutualInformationMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkMutualInformationMetricTest)
itk_add_test(NAME itkNeighborhoodSamplerTest1
      COMMAND ITK-IntegratedTestTestDriver itkNeighborhoodSamplerTest1)
itk_add_test(NAME itkNeuralNetworksPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkNeuralNetworksPrintTest)
itk_add_test(NAME itkNewTest
      COMMAND ITK-IntegratedTestTestDriver itkNewTest)
itk_add_test(NAME itkNormalVariateGeneratorTest1
      COMMAND ITK-IntegratedTestTestDriver itkNormalVariateGeneratorTest1)
itk_add_test(NAME itkNumericsPrintTest
     COMMAND ITK-IntegratedTestTestDriver itkNumericsPrintTest)
itk_add_test(NAME itkNumericsTest
      COMMAND ITK-IntegratedTestTestDriver itkNumericsTest)
itk_add_test(NAME itkOptimizersHierarchyTest
      COMMAND ITK-IntegratedTestTestDriver itkOptimizersHierarchyTest)
itk_add_test(NAME itkPixelAccessTest
      COMMAND ITK-IntegratedTestTestDriver itkPixelAccessTest)
itk_add_test(NAME itkPointGeometryTest
      COMMAND ITK-IntegratedTestTestDriver itkPointGeometryTest)
itk_add_test(NAME itkPointSetToImageRegistrationTest_1
      COMMAND ITK-IntegratedTestTestDriver itkPointSetToImageRegistrationTest_1)
itk_add_test(NAME itkPointSetToListSampleAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkPointSetToListSampleAdaptorTest)
itk_add_test(NAME itkPointSetToPointSetRegistrationTest
      COMMAND ITK-IntegratedTestTestDriver itkPointSetToPointSetRegistrationTest)
itk_add_test(NAME itkPriorityQueueTest
      COMMAND ITK-IntegratedTestTestDriver itkPriorityQueueTest)
itk_add_test(NAME itkProbabilityDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkProbabilityDistributionTest)
itk_add_test(NAME itkProjectionImageFilterTest1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryProjection100.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeProjection100.tif
    itkProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeProjection100.tif 100 0)
itk_add_test(NAME itkPromoteDimensionImageTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PromoteDimensionImageTest.mhd
              ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd
    itkPromoteDimensionImageTest ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd)
itk_add_test(NAME itkPushPopTileImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PushPopTileImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png
    itkPushPopTileImageFilterTest ${ITK_DATA_ROOT}/Input/Shapes.png ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png)
itk_add_test(NAME itkRandomVariateGeneratorBaseTest
      COMMAND ITK-IntegratedTestTestDriver itkRandomVariateGeneratorBaseTest)
itk_add_test(NAME itkReadWriteImageWithDictionaryTest
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteImageWithDictionaryTest
              ${ITK_TEST_OUTPUT_DIR}/test.hdr)
itk_add_test(NAME itkReadWriteImageWithDictionaryTest1
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteImageWithDictionaryTest
              ${ITK_TEST_OUTPUT_DIR}/test.mha)
itk_add_test(NAME itkReadWriteSpatialObjectTest
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects.meta)
itk_add_test(NAME itkReadWriteSpatialObjectTest1
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects1.meta binary)
itk_add_test(NAME itkReadWriteSpatialObjectTest2
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects2.meta ${ITK_DATA_ROOT}/Input/SpatialObjects.meta)
itk_add_test(NAME itkRecursiveGaussianImageFiltersOnTensorsTest
      COMMAND ITK-IntegratedTestTestDriver itkRecursiveGaussianImageFiltersOnTensorsTest)
itk_add_test(NAME itkRecursiveGaussianImageFiltersOnVectorImageTest
      COMMAND ITK-IntegratedTestTestDriver itkRecursiveGaussianImageFiltersOnVectorImageTest)
itk_add_test(NAME itkRecursiveGaussianImageFiltersTest
      COMMAND ITK-IntegratedTestTestDriver itkRecursiveGaussianImageFiltersTest)
itk_add_test(NAME itkRemoveBoundaryObjectsTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest.png
              ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png
    itkRemoveBoundaryObjectsTest ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png)
itk_add_test(NAME itkRemoveBoundaryObjectsTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest2.png
              ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png
    itkRemoveBoundaryObjectsTest2 ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png)
itk_add_test(NAME itkResampleImageTest
      COMMAND ITK-IntegratedTestTestDriver itkResampleImageTest)
itk_add_test(NAME itkResampleImageTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ResampleImageTest2.png
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2a.png
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ResampleImageTest2.png
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2b.png
    itkResampleImageTest2 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/circle.png ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2a.png ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2b.png)
itk_add_test(NAME itkResamplePhasedArray3DSpecialCoordinatesImageTest
      COMMAND ITK-IntegratedTestTestDriver itkResamplePhasedArray3DSpecialCoordinatesImageTest)
itk_add_test(NAME itkRGBInterpolateImageFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkRGBInterpolateImageFunctionTest)

itk_add_test(NAME itkSampleTest
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest)
itk_add_test(NAME itkSampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest2)
itk_add_test(NAME itkSampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest3)
itk_add_test(NAME itkSampleTest4
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest4)
itk_add_test(NAME itkSampleToHistogramFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest)
itk_add_test(NAME itkSampleToHistogramFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest2)
itk_add_test(NAME itkSampleToHistogramFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest3)
itk_add_test(NAME itkSampleToHistogramFilterTest4
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest4)
itk_add_test(NAME itkSampleToHistogramFilterTest5
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest5
              ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png)
itk_add_test(NAME itkSampleToHistogramFilterTest6
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest6)
itk_add_test(NAME itkSampleToHistogramFilterTest7
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest7)
itk_add_test(NAME itkSampleToSubsampleFilterTest1
      COMMAND ITK-IntegratedTestTestDriver itkSampleToSubsampleFilterTest1)
itk_add_test(NAME itkScalarImageKmeansImageFilter3DTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_EXAMPLE_DATA_ROOT}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
    itkScalarImageKmeansImageFilter3DTest ${ITK_EXAMPLE_DATA_ROOT}/KmeansTest_T1UCharRaw.nii.gz ${ITK_EXAMPLE_DATA_ROOT}/KmeansTest_T1RawSkullStrip.nii.gz ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz)
itk_add_test(NAME itkScalarImageToCooccurrenceListSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToCooccurrenceListSampleFilterTest)
itk_add_test(NAME itkScalarImageToCooccurrenceMatrixFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToCooccurrenceMatrixFilterTest)
itk_add_test(NAME itkScalarImageToCooccurrenceMatrixFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToCooccurrenceMatrixFilterTest2)
itk_add_test(NAME itkScalarImageToTextureFeaturesFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToTextureFeaturesFilterTest)
itk_add_test(NAME itkShrinkImagePreserveObjectPhysicalLocations
      COMMAND ITK-IntegratedTestTestDriver itkShrinkImagePreserveObjectPhysicalLocations)
itk_add_test(NAME itkShrinkImageStreamingTest
      COMMAND ITK-IntegratedTestTestDriver itkShrinkImageStreamingTest)
itk_add_test(NAME itkShrinkImageTest
      COMMAND ITK-IntegratedTestTestDriver itkShrinkImageTest)
itk_add_test(NAME itkSimpleFilterWatcherTest
      COMMAND ITK-IntegratedTestTestDriver itkSimpleFilterWatcherTest)
itk_add_test(NAME itkSparseFrequencyContainer2Test
      COMMAND ITK-IntegratedTestTestDriver itkSparseFrequencyContainer2Test)
itk_add_test(NAME itkSparseImageTest
      COMMAND ITK-IntegratedTestTestDriver itkSparseImageTest)
itk_add_test(NAME itkSpatialObjectToImageRegistrationTest
      COMMAND ITK-IntegratedTestTestDriver itkSpatialObjectToImageRegistrationTest)
itk_add_test(NAME itkStandardDeviationPerComponentSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkStandardDeviationPerComponentSampleFilterTest)
itk_add_test(NAME itkStatisticsAlgorithmTest
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsAlgorithmTest)
itk_add_test(NAME itkStatisticsAlgorithmTest2
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsAlgorithmTest2)
itk_add_test(NAME itkStatisticsTypesTest
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsTypesTest)
itk_add_test(NAME itkSTLThreadTest
      COMMAND ITK-IntegratedTestTestDriver itkSTLThreadTest)
itk_add_test(NAME itkStreamingImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkStreamingImageFilterTest)
itk_add_test(NAME itkStreamingImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkStreamingImageFilterTest2)
itk_add_test(NAME itkStreamingImageFilterTest3_1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png
              ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png
    itkStreamingImageFilterTest3 ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png 10)
itk_add_test(NAME itkStreamingImageFilterTest3_2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png
              ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png
    itkStreamingImageFilterTest3 ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png 1000)
itk_add_test(NAME itkSubsampleTest
      COMMAND ITK-IntegratedTestTestDriver itkSubsampleTest)
itk_add_test(NAME itkSubsampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkSubsampleTest2)
itk_add_test(NAME itkSubsampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkSubsampleTest3)

itk_add_test(NAME itkSymmetricSecondRankTensorImageReadTest
      COMMAND ITK-IntegratedTestTestDriver itkSymmetricSecondRankTensorImageReadTest
              ${ITK_TEST_OUTPUT_DIR}/testSymmetricTensor.nrrd)
itk_add_test(NAME itkSymmetricSecondRankTensorImageWriteReadTest
      COMMAND ITK-IntegratedTestTestDriver itkSymmetricSecondRankTensorImageWriteReadTest
              ${ITK_TEST_OUTPUT_DIR}/testSymmetricTensorWriteRead.mha)
itk_add_test(NAME itkTDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkTDistributionTest)
itk_add_test(NAME itkThreadDefsTest
      COMMAND ITK-IntegratedTestTestDriver itkThreadDefsTest)
itk_add_test(NAME itkTimeProbesTest
      COMMAND ITK-IntegratedTestTestDriver itkTimeProbesTest)
itk_add_test(NAME itkBlackTopHatImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBlackTopHatImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png
    itkTopHatImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png 0 3)
itk_add_test(NAME itkWhiteTopHatImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkWhiteTopHatImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png
    itkTopHatImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png 1 3)


add_executable(itkUnicodeIOTest itkUnicodeIOTest.cxx)
itk_module_target_label(itkUnicodeIOTest)
itk_add_test(NAME itkUnicodeIOTest COMMAND itkUnicodeIOTest)
itk_add_test(NAME itkVectorGeometryTest
      COMMAND ITK-IntegratedTestTestDriver itkVectorGeometryTest)
itk_add_test(NAME itkVectorImageReadWriteTest
      COMMAND ITK-IntegratedTestTestDriver itkVectorImageReadWriteTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImageReadWriteTest.mhd)
itk_add_test(NAME itkVectorImageReadWriteTest2
      COMMAND ITK-IntegratedTestTestDriver itkVectorImageReadWriteTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImageReadWriteTest.nrrd)
itk_add_test(NAME itkVectorImageTest
      COMMAND ITK-IntegratedTestTestDriver itkVectorImageTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImage.nrrd ${ITK_TEST_OUTPUT_DIR}/VectorImage.mhd)
itk_add_test(NAME itkVNLRoundProfileTest1
      COMMAND ITK-IntegratedTestTestDriver itkVNLRoundProfileTest1)
itk_add_test(NAME itkZeroFluxBoundaryConditionTest
      COMMAND ITK-IntegratedTestTestDriver itkZeroFluxBoundaryConditionTest)
itk_add_test(NAME QPropXORTest1
      COMMAND ITK-IntegratedTestTestDriver QPropXORTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/qpropxortest.txt)
itk_add_test(NAME RBFTest1
      COMMAND ITK-IntegratedTestTestDriver RBFTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_test.txt)
itk_add_test(NAME XORTest1
      COMMAND ITK-IntegratedTestTestDriver XORTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
itk_add_test(NAME XORTest2
      COMMAND ITK-IntegratedTestTestDriver XORTest2
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
itk_add_test(NAME itkRecursiveGaussianScaleSpaceTest1
      COMMAND ITK-IntegratedTestTestDriver
              itkRecursiveGaussianScaleSpaceTest1)

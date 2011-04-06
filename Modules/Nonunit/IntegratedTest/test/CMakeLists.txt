itk_module_test()
set(ITK-IntegratedTestTests
itkAdaptorComparisonTest.cxx
itkBasicArchitectureTest.cxx
itkBloxCoreAtomTest.cxx
itkBoundaryConditionTest.cxx
itkBSplineInterpolateImageFunctionTest.cxx
itkBSplineResampleImageFilterTest.cxx
itkBSplineResampleImageFunctionTest.cxx
itkByteSwapTest.cxx
itkChiSquareDistributionTest.cxx
itkConstantPadImageTest.cxx
itkConvertBufferTest.cxx
itkConvertBufferTest2.cxx
itkCovariantVectorGeometryTest.cxx
itkCurvatureFlowTest.cxx
itkCyclicReferences.cxx
itkDataTypeTest.cxx
itkDecisionRuleTest.cxx
itkDecoratorTest.cxx
itkDenseFrequencyContainer2Test.cxx
itkDicomImageIODirection2DTest.cxx
itkExpectationMaximizationMixtureModelEstimatorTest.cxx
itkExpImageFilterAndAdaptorTest.cxx
itkExtractImage3Dto2DTest.cxx
itkExtractImageTest.cxx
itkFilterDispatchTest.cxx
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
itkTriangleCellTest.cxx
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
itkCellInterfaceTest.cxx
itkIOPrintTest.cxx
itkBasicFiltersPrintTest.cxx
itkNeuralNetworksPrintTest.cxx
itkSpatialObjectPrintTest.cxx
)

CreateTestDriver_SupportBuildInIOFactories(ITK-IntegratedTest  "${ITK-IntegratedTest-Test_LIBRARIES}" "${ITK-IntegratedTestTests}")

# temporary solution for all print tests. Each module should have its own print test later.
add_executable(itkAlgorithmsPrintTest  itkAlgorithmsPrintTest.cxx)
target_link_libraries(itkAlgorithmsPrintTest ${ITK-IntegratedTest-Test_LIBRARIES})
add_test(NAME itkAlgorithmsPrintTest COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest2  itkAlgorithmsPrintTest2.cxx)
target_link_libraries(itkAlgorithmsPrintTest2 ${ITK-IntegratedTest-Test_LIBRARIES})
add_test(NAME itkAlgorithmsPrintTest2 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest3  itkAlgorithmsPrintTest3.cxx)
target_link_libraries(itkAlgorithmsPrintTest3 ${ITK-IntegratedTest-Test_LIBRARIES})
add_test(NAME itkAlgorithmsPrintTest3 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest4  itkAlgorithmsPrintTest4.cxx)
target_link_libraries(itkAlgorithmsPrintTest4 ${ITK-IntegratedTest-Test_LIBRARIES})
add_test(NAME itkAlgorithmsPrintTest4 COMMAND itkAlgorithmsPrintTest)

#-------------------------------------------------------------------------
add_test(NAME itkStatisticsPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsPrintTest)
add_test(NAME itkSpatialObjectPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkSpatialObjectPrintTest)
add_test(NAME itkIOPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkIOPrintTest)
add_test(NAME itkCommonPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkCommonPrintTest)
add_test(NAME itkCellInterfaceTest
      COMMAND ITK-IntegratedTestTestDriver itkCellInterfaceTest)
add_test(NAME itkBasicFiltersPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkBasicFiltersPrintTest)
add_test(NAME itkAdaptorComparisonTest
      COMMAND ITK-IntegratedTestTestDriver itkAdaptorComparisonTest)
add_test(NAME itkBasicArchitectureTest
      COMMAND ITK-IntegratedTestTestDriver itkBasicArchitectureTest)

add_test(NAME itkBloxCoreAtomTest
      COMMAND ITK-IntegratedTestTestDriver itkBloxCoreAtomTest)
add_test(NAME itkBoundaryConditionTest
      COMMAND ITK-IntegratedTestTestDriver itkBoundaryConditionTest)
add_test(NAME itkBSplineInterpolateImageFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkBSplineInterpolateImageFunctionTest)
add_test(NAME itkBSplineResampleImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkBSplineResampleImageFilterTest)
add_test(NAME itkBSplineResampleImageFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkBSplineResampleImageFunctionTest)
add_test(NAME itkByteSwapTest
      COMMAND ITK-IntegratedTestTestDriver itkByteSwapTest)
add_test(NAME itkChiSquareDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkChiSquareDistributionTest)
add_test(NAME itkConstantPadImageTest
      COMMAND ITK-IntegratedTestTestDriver itkConstantPadImageTest)
add_test(NAME itkConvertBufferTest
      COMMAND ITK-IntegratedTestTestDriver itkConvertBufferTest)
add_test(NAME itkConvertBufferTest2
      COMMAND ITK-IntegratedTestTestDriver itkConvertBufferTest2)
add_test(NAME itkCovariantVectorGeometryTest
      COMMAND ITK-IntegratedTestTestDriver itkCovariantVectorGeometryTest)
add_test(NAME itkCurvatureFlowTest
      COMMAND ITK-IntegratedTestTestDriver itkCurvatureFlowTest
              ${ITK_TEST_OUTPUT_DIR}/itkCurvatureFlowTest.vtk)
add_test(NAME itkCyclicReferences
      COMMAND ITK-IntegratedTestTestDriver itkCyclicReferences)
add_test(NAME itkDataTypeTest
      COMMAND ITK-IntegratedTestTestDriver itkDataTypeTest)
add_test(NAME itkDecisionRuleTest
      COMMAND ITK-IntegratedTestTestDriver itkDecisionRuleTest)
add_test(NAME itkDecoratorTest
      COMMAND ITK-IntegratedTestTestDriver itkDecoratorTest)
add_test(NAME itkDenseFrequencyContainer2Test
      COMMAND ITK-IntegratedTestTestDriver itkDenseFrequencyContainer2Test)
add_test(NAME itkDicomImageIODirection2DTest
      COMMAND ITK-IntegratedTestTestDriver itkDicomImageIODirection2DTest
              ${ITK_TEST_OUTPUT_DIR}/itkDicomImage3DIODirection2DTest.dcm ${ITK_TEST_OUTPUT_DIR}/itkDicomImage3DIODirection2DTest.mhd ${ITK_TEST_OUTPUT_DIR}/itkDicomImage2DIODirection2DTest.mhd)
add_test(NAME itkExpectationMaximizationMixtureModelEstimatorTest
      COMMAND ITK-IntegratedTestTestDriver itkExpectationMaximizationMixtureModelEstimatorTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
add_test(NAME itkExpImageFilterAndAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkExpImageFilterAndAdaptorTest)
add_test(NAME itkExtractImage3Dto2DTest
      COMMAND ITK-IntegratedTestTestDriver itkExtractImage3Dto2DTest)
add_test(NAME itkExtractImageTest
      COMMAND ITK-IntegratedTestTestDriver itkExtractImageTest)
add_test(NAME itkFilterDispatchTest
      COMMAND ITK-IntegratedTestTestDriver itkFilterDispatchTest)
add_test(NAME itkFloodFilledSpatialFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkFloodFilledSpatialFunctionTest)
add_test(NAME itkFloodFillIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkFloodFillIteratorTest)
add_test(NAME itkGaussianDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkGaussianDistributionTest)
add_test(NAME itkGaussianMembershipFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkGaussianMembershipFunctionTest)
add_test(NAME itkGaussianMixtureModelComponentTest
      COMMAND ITK-IntegratedTestTestDriver itkGaussianMixtureModelComponentTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
add_test(NAME itkGibbsTest
      COMMAND ITK-IntegratedTestTestDriver itkGibbsTest)
add_test(NAME itkGrayscaleGeodesicErodeDilateImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png
    itkGrayscaleGeodesicErodeDilateImageFilterTest ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png 35)
add_test(NAME itkHashTableTest
      COMMAND ITK-IntegratedTestTestDriver itkHashTableTest)
add_test(NAME itkHConvexConcaveImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HConvexConcaveImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png
    itkHConvexConcaveImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png 50)
add_test(NAME itkHessianRecursiveGaussianFilterScaleSpaceTest
      COMMAND ITK-IntegratedTestTestDriver itkHessianRecursiveGaussianFilterScaleSpaceTest)
add_test(NAME itkHessianRecursiveGaussianFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkHessianRecursiveGaussianFilterTest)
add_test(NAME itkHMaximaMinimaImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png
    itkHMaximaMinimaImageFilterTest ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png 35)
add_test(NAME itkHMaximaMinimaImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png
    itkHMaximaMinimaImageFilterTest ${ITK_DATA_ROOT}/Input/cake_hard.png ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png 35)
add_test(NAME itkHoughTransform2DCirclesImageTest
      COMMAND ITK-IntegratedTestTestDriver itkHoughTransform2DCirclesImageTest)
add_test(NAME itkHoughTransform2DLinesImageTest
      COMMAND ITK-IntegratedTestTestDriver itkHoughTransform2DLinesImageTest)
add_test(NAME itkImageAdaptorPipeLineTest
      COMMAND ITK-IntegratedTestTestDriver itkImageAdaptorPipeLineTest)
add_test(NAME itkImageDuplicatorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageDuplicatorTest)

add_test(NAME itkImageIteratorsForwardBackwardTest
      COMMAND ITK-IntegratedTestTestDriver itkImageIteratorsForwardBackwardTest)
add_test(NAME itkImageLinearIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageLinearIteratorTest)
add_test(NAME itkImageMomentsTest
      COMMAND ITK-IntegratedTestTestDriver itkImageMomentsTest)
add_test(NAME itkImageRandomIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomIteratorTest)
add_test(NAME itkImageRandomIteratorTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomIteratorTest2
              ${ITK_TEST_OUTPUT_DIR}/itkImageRandomIteratorTest2Output.mha)
add_test(NAME itkImageRandomNonRepeatingIteratorWithIndexTest
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomNonRepeatingIteratorWithIndexTest)
add_test(NAME itkImageRandomNonRepeatingIteratorWithIndexTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomNonRepeatingIteratorWithIndexTest2)
add_test(NAME itkImageSliceIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageSliceIteratorTest)
add_test(NAME itkImageToHistogramFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkImageToHistogramFilterTest)
add_test(NAME itkImageToHistogramFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageToHistogramFilterTest2
              ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/itkImageToHistogramFilterTest2.txt)
add_test(NAME itkImageToHistogramFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkImageToHistogramFilterTest3
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkImageToHistogramFilterTest3.txt)
add_test(NAME itkImageToListSampleAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleAdaptorTest)
add_test(NAME itkImageToListSampleAdaptorTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleAdaptorTest2)
add_test(NAME itkImageToListSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleFilterTest)
add_test(NAME itkImageToListSampleFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleFilterTest2)
add_test(NAME itkImageToListSampleFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleFilterTest3)
add_test(NAME itkImportContainerTest
      COMMAND ITK-IntegratedTestTestDriver itkImportContainerTest)
add_test(NAME itkImportImageTest
      COMMAND ITK-IntegratedTestTestDriver itkImportImageTest)
add_test(NAME itkInterpolateTest
      COMMAND ITK-IntegratedTestTestDriver itkInterpolateTest)

if(ITK_BUILD_SHARED_LIBS)
  add_test(NAME itkIOPluginTest
           COMMAND ITK-IntegratedTestTestDriver itkIOPluginTest
             ${ITK_TEST_OUTPUT_DIR}
             "FileFreeIO::Size=128,256:Spacing=.5,.8:Origin=5,6:Direction=-1,0,0,-1"
             ${ITK_TEST_OUTPUT_DIR}/itkIOPluginTest.png
    )
  ## Create a library to test ITK IO plugins
  set(FileFreeTest_Source itkFileFreeImageIO.cxx itkFileFreeImageIOFactory.cxx)
  add_library(FileFreeTestLib MODULE ${FileFreeTest_Source})
  target_link_libraries(FileFreeTestLib ${ITK-IO-Base_LIBRARIES})
  set_property(TARGET FileFreeTestLib PROPERTY LIBRARY_OUTPUT_DIRECTORY ${ITK_TEST_OUTPUT_DIR})
  add_dependencies(ITK-IntegratedTestTestDriver FileFreeTestLib)
endif(ITK_BUILD_SHARED_LIBS)


add_test(NAME itkIteratorTests
      COMMAND ITK-IntegratedTestTestDriver itkIteratorTests)
add_test(NAME itkJoinSeriesImageFilterStreamingTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkJoinSeriesImageFilterStreamingTest.mha
    itkJoinSeriesImageFilterStreamingTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkJoinSeriesImageFilterStreamingTest.mha)
add_test(NAME itkJointDomainImageToListSampleAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkJointDomainImageToListSampleAdaptorTest)
add_test(NAME itkKdTreeBasedKmeansEstimatorTest
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeBasedKmeansEstimatorTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat 1 28.54746 0.07)
add_test(NAME itkKdTreeGeneratorTest
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeGeneratorTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
add_test(NAME itkKdTreeTest1
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              100 100 1 ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest1_100.dot)
add_test(NAME itkKdTreeTest4
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 1)
add_test(NAME itkKdTreeTest5
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 2)
add_test(NAME itkKdTreeTest6
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 3)
add_test(NAME itkKdTreeTest7
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 5)
add_test(NAME itkKdTreeTest8
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 10)
add_test(NAME itkKdTreeTest9
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 20)
add_test(NAME itkKdTreeTest10
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 100)
add_test(NAME itkKdTreeTest2
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest2
              ${ITK_DATA_ROOT}/Input/pointsForKdTree.txt 1 ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest2.dot)
add_test(NAME itkKdTreeTest3
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
add_test(NAME itkKdTreeTest1Plot
      COMMAND ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest1_100.png ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest1_100.dot)
set_tests_properties( itkKdTreeTest1Plot PROPERTIES DEPENDS itkKdTreeTest1 )
add_test(NAME itkKdTreeTest2Plot
      COMMAND ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest2.png ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest2.dot)
set_tests_properties( itkKdTreeTest2Plot PROPERTIES DEPENDS itkKdTreeTest2 )
add_test(NAME itkKdTreeTest3Plot
      COMMAND ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest3.png ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest3.dot)
set_tests_properties( itkKdTreeTest3Plot PROPERTIES DEPENDS itkKdTreeTest3 )
endif(DOXYGEN_DOT_EXECUTABLE)

add_test(NAME itkKdTreeTestSamplePoints
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTestSamplePoints)
add_test(NAME itkKmeansModelEstimatorTest
      COMMAND ITK-IntegratedTestTestDriver itkKmeansModelEstimatorTest)
add_test(NAME itkListSampleTest1
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              1)
add_test(NAME itkListSampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              5)
add_test(NAME itkListSampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              10)
add_test(NAME itkListSampleTest4
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              100)
add_test(NAME itkMathCastWithRangeCheckTest
      COMMAND ITK-IntegratedTestTestDriver itkMathCastWithRangeCheckTest)
add_test(NAME itkMathematicalMorphologyImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkMathematicalMorphologyImageFilterTest)
add_test(NAME itkMathRoundProfileTest1
      COMMAND ITK-IntegratedTestTestDriver itkMathRoundProfileTest1)
add_test(NAME itkMathRoundTest
      COMMAND ITK-IntegratedTestTestDriver itkMathRoundTest)
add_test(NAME itkMathRoundTest2
      COMMAND ITK-IntegratedTestTestDriver itkMathRoundTest2)
add_test(NAME itkMatrixImageWriteReadTest
      COMMAND ITK-IntegratedTestTestDriver itkMatrixImageWriteReadTest
              ${ITK_TEST_OUTPUT_DIR}/testMatrix1.mha)
add_test(NAME itkMeanReciprocalSquareDifferenceImageMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkMeanReciprocalSquareDifferenceImageMetricTest)
add_test(NAME itkMeanSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkMeanSampleFilterTest)
add_test(NAME itkMeanSampleFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkMeanSampleFilterTest2)
add_test(NAME itkMeanSampleFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkMeanSampleFilterTest3)
add_test(NAME itkMeanSquaresImageMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkMeanSquaresImageMetricTest)
add_test(NAME itkMeasurementVectorTraitsTest
      COMMAND ITK-IntegratedTestTestDriver itkMeasurementVectorTraitsTest)
add_test(NAME itkMedialNodeCorrespondencesTest
      COMMAND ITK-IntegratedTestTestDriver itkMedialNodeCorrespondencesTest)
add_test(NAME itkMembershipFunctionBaseTest
      COMMAND ITK-IntegratedTestTestDriver itkMembershipFunctionBaseTest)
add_test(NAME itkMembershipFunctionBaseTest2
      COMMAND ITK-IntegratedTestTestDriver itkMembershipFunctionBaseTest2)
add_test(NAME itkMembershipSampleTest1
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest1)
add_test(NAME itkMembershipSampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest2)
add_test(NAME itkMembershipSampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest3)
add_test(NAME itkMembershipSampleTest4
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest4)
add_test(NAME itkMirrorPadImageTest
      COMMAND ITK-IntegratedTestTestDriver itkMirrorPadImageTest)
add_test(NAME itkMixtureModelComponentBaseTest
      COMMAND ITK-IntegratedTestTestDriver itkMixtureModelComponentBaseTest)
add_test(NAME itkModifiedTimeTest
      COMMAND ITK-IntegratedTestTestDriver itkModifiedTimeTest)
add_test(NAME itkMutualInformationMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkMutualInformationMetricTest)
add_test(NAME itkNeighborhoodSamplerTest1
      COMMAND ITK-IntegratedTestTestDriver itkNeighborhoodSamplerTest1)
add_test(NAME itkNeuralNetworksPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkNeuralNetworksPrintTest)
add_test(NAME itkNewTest
      COMMAND ITK-IntegratedTestTestDriver itkNewTest)
add_test(NAME itkNormalVariateGeneratorTest1
      COMMAND ITK-IntegratedTestTestDriver itkNormalVariateGeneratorTest1)
add_test(NAME itkNumericsPrintTest
     COMMAND ITK-IntegratedTestTestDriver itkNumericsPrintTest)
add_test(NAME itkNumericsTest
      COMMAND ITK-IntegratedTestTestDriver itkNumericsTest)
add_test(NAME itkOptimizersHierarchyTest
      COMMAND ITK-IntegratedTestTestDriver itkOptimizersHierarchyTest)
add_test(NAME itkPixelAccessTest
      COMMAND ITK-IntegratedTestTestDriver itkPixelAccessTest)
add_test(NAME itkPointGeometryTest
      COMMAND ITK-IntegratedTestTestDriver itkPointGeometryTest)
add_test(NAME itkPointSetToImageRegistrationTest_1
      COMMAND ITK-IntegratedTestTestDriver itkPointSetToImageRegistrationTest_1)
add_test(NAME itkPointSetToListSampleAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkPointSetToListSampleAdaptorTest)
add_test(NAME itkPointSetToPointSetRegistrationTest
      COMMAND ITK-IntegratedTestTestDriver itkPointSetToPointSetRegistrationTest)
add_test(NAME itkPriorityQueueTest
      COMMAND ITK-IntegratedTestTestDriver itkPriorityQueueTest)
add_test(NAME itkProbabilityDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkProbabilityDistributionTest)
add_test(NAME itkProjectionImageFilterTest1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryProjection100.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeProjection100.tif
    itkProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeProjection100.tif 100 0)
add_test(NAME itkPromoteDimensionImageTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PromoteDimensionImageTest.mhd
              ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd
    itkPromoteDimensionImageTest ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd)
add_test(NAME itkPushPopTileImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PushPopTileImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png
    itkPushPopTileImageFilterTest ${ITK_DATA_ROOT}/Input/Shapes.png ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png)
add_test(NAME itkRandomVariateGeneratorBaseTest
      COMMAND ITK-IntegratedTestTestDriver itkRandomVariateGeneratorBaseTest)
add_test(NAME itkReadWriteImageWithDictionaryTest
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteImageWithDictionaryTest
              ${ITK_TEST_OUTPUT_DIR}/test.hdr)
add_test(NAME itkReadWriteImageWithDictionaryTest1
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteImageWithDictionaryTest
              ${ITK_TEST_OUTPUT_DIR}/test.mha)
add_test(NAME itkReadWriteSpatialObjectTest
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects.meta)
add_test(NAME itkReadWriteSpatialObjectTest1
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects1.meta binary)
add_test(NAME itkReadWriteSpatialObjectTest2
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects2.meta ${ITK_DATA_ROOT}/Input/SpatialObjects.meta)
add_test(NAME itkRecursiveGaussianImageFiltersOnTensorsTest
      COMMAND ITK-IntegratedTestTestDriver itkRecursiveGaussianImageFiltersOnTensorsTest)
add_test(NAME itkRecursiveGaussianImageFiltersOnVectorImageTest
      COMMAND ITK-IntegratedTestTestDriver itkRecursiveGaussianImageFiltersOnVectorImageTest)
add_test(NAME itkRecursiveGaussianImageFiltersTest
      COMMAND ITK-IntegratedTestTestDriver itkRecursiveGaussianImageFiltersTest)
add_test(NAME itkRemoveBoundaryObjectsTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest.png
              ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png
    itkRemoveBoundaryObjectsTest ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png)
add_test(NAME itkRemoveBoundaryObjectsTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest2.png
              ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png
    itkRemoveBoundaryObjectsTest2 ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png)
add_test(NAME itkResampleImageTest
      COMMAND ITK-IntegratedTestTestDriver itkResampleImageTest)
add_test(NAME itkResampleImageTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ResampleImageTest2.png
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2.png
    itkResampleImageTest2 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/circle.png ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2.png)
add_test(NAME itkResamplePhasedArray3DSpecialCoordinatesImageTest
      COMMAND ITK-IntegratedTestTestDriver itkResamplePhasedArray3DSpecialCoordinatesImageTest)
add_test(NAME itkRGBInterpolateImageFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkRGBInterpolateImageFunctionTest)

add_test(NAME itkSampleTest
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest)
add_test(NAME itkSampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest2)
add_test(NAME itkSampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest3)
add_test(NAME itkSampleTest4
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest4)
add_test(NAME itkSampleToHistogramFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest)
add_test(NAME itkSampleToHistogramFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest2)
add_test(NAME itkSampleToHistogramFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest3)
add_test(NAME itkSampleToHistogramFilterTest4
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest4)
add_test(NAME itkSampleToHistogramFilterTest5
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest5
              ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png)
add_test(NAME itkSampleToHistogramFilterTest6
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest6)
add_test(NAME itkSampleToHistogramFilterTest7
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest7)
add_test(NAME itkSampleToSubsampleFilterTest1
      COMMAND ITK-IntegratedTestTestDriver itkSampleToSubsampleFilterTest1)
add_test(NAME itkScalarImageKmeansImageFilter3DTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_EXAMPLE_DATA_ROOT}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
    itkScalarImageKmeansImageFilter3DTest ${ITK_EXAMPLE_DATA_ROOT}/KmeansTest_T1UCharRaw.nii.gz ${ITK_EXAMPLE_DATA_ROOT}/KmeansTest_T1RawSkullStrip.nii.gz ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz)
add_test(NAME itkScalarImageToCooccurrenceListSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToCooccurrenceListSampleFilterTest)
add_test(NAME itkScalarImageToCooccurrenceMatrixFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToCooccurrenceMatrixFilterTest)
add_test(NAME itkScalarImageToCooccurrenceMatrixFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToCooccurrenceMatrixFilterTest2)
add_test(NAME itkScalarImageToTextureFeaturesFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToTextureFeaturesFilterTest)
add_test(NAME itkShrinkImagePreserveObjectPhysicalLocations
      COMMAND ITK-IntegratedTestTestDriver itkShrinkImagePreserveObjectPhysicalLocations)
add_test(NAME itkShrinkImageStreamingTest
      COMMAND ITK-IntegratedTestTestDriver itkShrinkImageStreamingTest)
add_test(NAME itkShrinkImageTest
      COMMAND ITK-IntegratedTestTestDriver itkShrinkImageTest)
add_test(NAME itkSimpleFilterWatcherTest
      COMMAND ITK-IntegratedTestTestDriver itkSimpleFilterWatcherTest)
add_test(NAME itkSparseFrequencyContainer2Test
      COMMAND ITK-IntegratedTestTestDriver itkSparseFrequencyContainer2Test)
add_test(NAME itkSparseImageTest
      COMMAND ITK-IntegratedTestTestDriver itkSparseImageTest)
add_test(NAME itkSpatialObjectToImageRegistrationTest
      COMMAND ITK-IntegratedTestTestDriver itkSpatialObjectToImageRegistrationTest)
add_test(NAME itkStandardDeviationPerComponentSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkStandardDeviationPerComponentSampleFilterTest)
add_test(NAME itkStatisticsAlgorithmTest
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsAlgorithmTest)
add_test(NAME itkStatisticsAlgorithmTest2
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsAlgorithmTest2)
add_test(NAME itkStatisticsTypesTest
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsTypesTest)
add_test(NAME itkSTLThreadTest
      COMMAND ITK-IntegratedTestTestDriver itkSTLThreadTest)
add_test(NAME itkStreamingImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkStreamingImageFilterTest)
add_test(NAME itkStreamingImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkStreamingImageFilterTest2)
add_test(NAME itkStreamingImageFilterTest3_1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png
              ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png
    itkStreamingImageFilterTest3 ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png 10)
add_test(NAME itkStreamingImageFilterTest3_2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png
              ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png
    itkStreamingImageFilterTest3 ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png 1000)
add_test(NAME itkSubsampleTest
      COMMAND ITK-IntegratedTestTestDriver itkSubsampleTest)
add_test(NAME itkSubsampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkSubsampleTest2)
add_test(NAME itkSubsampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkSubsampleTest3)

add_test(NAME itkSymmetricSecondRankTensorImageReadTest
      COMMAND ITK-IntegratedTestTestDriver itkSymmetricSecondRankTensorImageReadTest
              ${ITK_TEST_OUTPUT_DIR}/testSymmetricTensor.nrrd)
add_test(NAME itkSymmetricSecondRankTensorImageWriteReadTest
      COMMAND ITK-IntegratedTestTestDriver itkSymmetricSecondRankTensorImageWriteReadTest
              ${ITK_TEST_OUTPUT_DIR}/testSymmetricTensorWriteRead.mha)
add_test(NAME itkTDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkTDistributionTest)
add_test(NAME itkThreadDefsTest
      COMMAND ITK-IntegratedTestTestDriver itkThreadDefsTest)
add_test(NAME itkTimeProbesTest
      COMMAND ITK-IntegratedTestTestDriver itkTimeProbesTest)
add_test(NAME itkBlackTopHatImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBlackTopHatImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png
    itkTopHatImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png 0 3)
add_test(NAME itkWhiteTopHatImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkWhiteTopHatImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png
    itkTopHatImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png 1 3)
add_test(NAME itkTriangleCellTest
      COMMAND ITK-IntegratedTestTestDriver itkTriangleCellTest)


add_executable(itkUnicodeIOTest itkUnicodeIOTest.cxx)
add_test(NAME itkUnicodeIOTest COMMAND itkUnicodeIOTest)
add_test(NAME itkVectorGeometryTest
      COMMAND ITK-IntegratedTestTestDriver itkVectorGeometryTest)
add_test(NAME itkVectorImageReadWriteTest
      COMMAND ITK-IntegratedTestTestDriver itkVectorImageReadWriteTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImageReadWriteTest.mhd)
add_test(NAME itkVectorImageReadWriteTest2
      COMMAND ITK-IntegratedTestTestDriver itkVectorImageReadWriteTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImageReadWriteTest.nrrd)
add_test(NAME itkVectorImageTest
      COMMAND ITK-IntegratedTestTestDriver itkVectorImageTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImage.nrrd ${ITK_TEST_OUTPUT_DIR}/VectorImage.mhd)
add_test(NAME itkVNLRoundProfileTest1
      COMMAND ITK-IntegratedTestTestDriver itkVNLRoundProfileTest1)
add_test(NAME itkZeroFluxBoundaryConditionTest
      COMMAND ITK-IntegratedTestTestDriver itkZeroFluxBoundaryConditionTest)
add_test(NAME QPropXORTest1
      COMMAND ITK-IntegratedTestTestDriver QPropXORTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/qpropxortest.txt)
add_test(NAME RBFTest1
      COMMAND ITK-IntegratedTestTestDriver RBFTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_test.txt)
add_test(NAME XORTest1
      COMMAND ITK-IntegratedTestTestDriver XORTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
add_test(NAME XORTest2
      COMMAND ITK-IntegratedTestTestDriver XORTest2
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
add_test(NAME itkRecursiveGaussianScaleSpaceTest1
      COMMAND ITK-IntegratedTestTestDriver
              itkRecursiveGaussianScaleSpaceTest1)

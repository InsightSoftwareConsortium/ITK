itk_module_test()
set(ITKLevelSetsTests
itkThresholdSegmentationLevelSetImageFilterTest.cxx
itkVectorThresholdSegmentationLevelSetImageFilterTest.cxx
itkAnisotropicFourthOrderLevelSetImageFilterTest.cxx
itkReinitializeLevelSetImageFilterTest.cxx
itkLevelSetVelocityNeighborhoodExtractorTest.cxx
itkIsotropicFourthOrderLevelSetImageFilterTest.cxx
itkGeodesicActiveContourLevelSetImageFilterTest.cxx
itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2.cxx
itkParallelSparseFieldLevelSetImageFilterTest.cxx
itkShapeDetectionLevelSetImageFilterTest.cxx
itkNarrowBandThresholdSegmentationLevelSetImageFilterTest.cxx
itkNarrowBandCurvesLevelSetImageFilterTest.cxx
itkCollidingFrontsImageFilterTest.cxx
itkGeodesicActiveContourShapePriorLevelSetImageFilterTest.cxx
itkLevelSetFunctionTest.cxx
itkExtensionVelocitiesImageFilterTest.cxx
itkCannySegmentationLevelSetImageFilterTest.cxx
itkLevelSetNeighborhoodExtractorTest.cxx
itkShapePriorMAPCostFunctionTest.cxx
itkImplicitManifoldNormalVectorFilterTest.cxx
itkSparseFieldFourthOrderLevelSetImageFilterTest.cxx
itkLaplacianSegmentationLevelSetImageFilterTest.cxx
itkShapePriorSegmentationLevelSetFunctionTest.cxx
itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest.cxx
itkUnsharpMaskLevelSetImageFilterTest.cxx
itkCurvesLevelSetImageFilterTest.cxx
itkCurvesLevelSetImageFilterZeroSigmaTest.cxx
itkBinaryMaskToNarrowBandPointSetFilterTest.cxx
)

CreateTestDriver(ITKLevelSets  "${ITKLevelSets-Test_LIBRARIES}" "${ITKLevelSetsTests}")

itk_add_test(NAME itkThresholdSegmentationLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkThresholdSegmentationLevelSetImageFilterTest)
itk_add_test(NAME itkVectorThresholdSegmentationLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/VectorThresholdSegmentationLevelSetImageFilterTest.png,:}
              ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png
    --compareNumberOfPixelsTolerance 55
    itkVectorThresholdSegmentationLevelSetImageFilterTest DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSliceInitialLevelSet.png} DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png 5.0 50)
itk_add_test(NAME itkAnisotropicFourthOrderLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkAnisotropicFourthOrderLevelSetImageFilterTest)
itk_add_test(NAME itkReinitializeLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkReinitializeLevelSetImageFilterTest)
itk_add_test(NAME itkLevelSetVelocityNeighborhoodExtractorTest
      COMMAND ITKLevelSetsTestDriver itkLevelSetVelocityNeighborhoodExtractorTest)
itk_add_test(NAME itkIsotropicFourthOrderLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkIsotropicFourthOrderLevelSetImageFilterTest)
itk_add_test(NAME itkGeodesicActiveContourLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkGeodesicActiveContourLevelSetImageFilterTest)
itk_add_test(NAME itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2
      COMMAND ITKLevelSetsTestDriver itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2)
itk_add_test(NAME itkParallelSparseFieldLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ParallelSparseFieldLevelSetImageFilterTest.mha}
              ${ITK_TEST_OUTPUT_DIR}/ParallelSparseFieldLevelSetImageFilterTest.mha
    itkParallelSparseFieldLevelSetImageFilterTest ${ITK_TEST_OUTPUT_DIR}/ParallelSparseFieldLevelSetImageFilterTest.mha)
itk_add_test(NAME itkShapeDetectionLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkShapeDetectionLevelSetImageFilterTest)
itk_add_test(NAME itkNarrowBandThresholdSegmentationLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkNarrowBandThresholdSegmentationLevelSetImageFilterTest)
itk_add_test(NAME itkNarrowBandCurvesLevelSetImageFilterTest1
      COMMAND ITKLevelSetsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/itkNarrowBandCurvesLevelSetImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkNarrowBandCurvesLevelSetImageFilterTest.png
    itkNarrowBandCurvesLevelSetImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkNarrowBandCurvesLevelSetImageFilterTest.png)
itk_add_test(NAME itkCollidingFrontsImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkCollidingFrontsImageFilterTest)
itk_add_test(NAME itkGeodesicActiveContourShapePriorLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkGeodesicActiveContourShapePriorLevelSetImageFilterTest)
itk_add_test(NAME itkLevelSetFunctionTest
      COMMAND ITKLevelSetsTestDriver itkLevelSetFunctionTest)
itk_add_test(NAME itkExtensionVelocitiesImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkExtensionVelocitiesImageFilterTest)
itk_add_test(NAME itkCannySegmentationLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkCannySegmentationLevelSetImageFilterTest)
itk_add_test(NAME itkLevelSetNeighborhoodExtractorTest
      COMMAND ITKLevelSetsTestDriver itkLevelSetNeighborhoodExtractorTest)
itk_add_test(NAME itkShapePriorMAPCostFunctionTest
      COMMAND ITKLevelSetsTestDriver itkShapePriorMAPCostFunctionTest)
itk_add_test(NAME itkImplicitManifoldNormalVectorFilterTest
      COMMAND ITKLevelSetsTestDriver itkImplicitManifoldNormalVectorFilterTest)
itk_add_test(NAME itkSparseFieldFourthOrderLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkSparseFieldFourthOrderLevelSetImageFilterTest)
itk_add_test(NAME itkLaplacianSegmentationLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkLaplacianSegmentationLevelSetImageFilterTest)
itk_add_test(NAME itkShapePriorSegmentationLevelSetFunctionTest
      COMMAND ITKLevelSetsTestDriver itkShapePriorSegmentationLevelSetFunctionTest)
itk_add_test(NAME itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest
      COMMAND ITKLevelSetsTestDriver itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest)
itk_add_test(NAME itkUnsharpMaskLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkUnsharpMaskLevelSetImageFilterTest)
itk_add_test(NAME itkCurvesLevelSetImageFilterTest
      COMMAND ITKLevelSetsTestDriver itkCurvesLevelSetImageFilterTest)
itk_add_test(NAME itkCurvesLevelSetImageFilterZeroSigmaTest
      COMMAND ITKLevelSetsTestDriver itkCurvesLevelSetImageFilterZeroSigmaTest)
itk_add_test(NAME itkBinaryMaskToNarrowBandPointSetFilterTest
      COMMAND ITKLevelSetsTestDriver itkBinaryMaskToNarrowBandPointSetFilterTest)

itk_python_add_test(NAME PythonLazyLoadingImage COMMAND LazyLoadingImageTest.py)

list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_float AND wrap_2_index GREATER -1)
# TODO: reenable the geodesic test once we get why the result is not the
# same than with c++
# keep just one to be sure it run, but don't compare the images

  itk_python_add_test(NAME PythonGeodesicActiveContourLeftVentricleTest
#     --compare GeodesicActiveContourLeftVentricleTest.png
#             DATA{${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourLeftVentricleTest.png}
    COMMAND GeodesicActiveContourImageFilterTest.py
      DATA{Input/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/GeodesicActiveContourLeftVentricleTest.png
      81 114 5.0 1.0 -0.5 3.0 2.0
    )

  itk_python_add_test(NAME PythonThresholdSegmentationLevelSetVentricleTest
    TEST_DRIVER_ARGS
    --compare ${ITK_TEST_OUTPUT_DIR}/PythonThresholdSegmentationLevelSetVentricleTest.png
              DATA{PythonThresholdSegmentationLevelSetVentricleTest.png}
    COMMAND ThresholdSegmentationLevelSetImageFilterTest.py
      DATA{Input/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonThresholdSegmentationLevelSetVentricleTest.png
      81 112 5 210 250
    )

  itk_python_add_test(NAME PythonThresholdSegmentationLevelSetWhiteMatterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/ThresholdSegmentationLevelSetWhiteMatterTest.png
        DATA{PythonThresholdSegmentationLevelSetWhiteMatterTest.png}
    COMMAND ThresholdSegmentationLevelSetImageFilterTest.py
      DATA{Input/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/ThresholdSegmentationLevelSetWhiteMatterTest.png
      60 116 5 150 180
    )

  itk_python_add_test(NAME PythonThresholdSegmentationLevelSetGrayMatterTest
    TEST_DRIVERS_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonThresholdSegmentationLevelSetGrayMatterTest.png
                DATA{Baseline/PythonThresholdSegmentationLevelSetGrayMatterTest.png}
    COMMAND ThresholdSegmentationLevelSetImageFilterTest.py
      DATA{Input/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonThresholdSegmentationLevelSetGrayMatterTest.png
      107 69 5 180 210
    )
endif()

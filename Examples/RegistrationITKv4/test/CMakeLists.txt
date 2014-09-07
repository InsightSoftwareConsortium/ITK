set(BASELINE ${ITK_DATA_ROOT}/Baseline/Registration)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

itk_add_test(NAME ImageRegistrationHistogramPlotterTest COMMAND  ${ITK_TEST_DRIVER}
  --compareNumberOfPixelsTolerance 30
  --compareIntensityTolerance 10
  --compareRadiusTolerance 10
  --compare DATA{${BASELINE}/JointEntropyHistogramAfterRegistration.png,:}
            ${TEMP}/JointEntropyHistogramAfterRegistration.png
  $<TARGET_FILE:ImageRegistrationHistogramPlotter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/RegisteredImage.png
        0
        ${TEMP}/JointEntropyHistogramPriorToRegistration.png
        ${TEMP}/JointEntropyHistogramAfterRegistration.png
        512
)
set_property(TEST ImageRegistrationHistogramPlotterTest APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME ImageRegistration1Test1 COMMAND  ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration1TestPixelCentered.png}
            ${TEMP}/ImageRegistration1Test1PixelCentered.png
  $<TARGET_FILE:ImageRegistration1>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1Test1PixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceAfterTest1PixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceBeforeTest1PixelCentered.png
)

itk_add_test(NAME ImageRegistration1Test2 COMMAND  ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration1TestPixelCentered.png}
            ${TEMP}/ImageRegistration1Test2PixelCentered.png
  $<TARGET_FILE:ImageRegistration1>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1Test2PixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceAfterTest2PixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceBeforeTest2PixelCentered.png
        1
)

itk_add_test(NAME ImageRegistration2Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration2Test.png}
            ${TEMP}/ImageRegistration2Test.png
  $<TARGET_FILE:ImageRegistration2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration2Test.png
        ${TEMP}/ImageRegistration2BeforeTest.png
        ${TEMP}/ImageRegistration2AfterTest.png
)
set_property(TEST ImageRegistration2Test APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME ImageRegistration3Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration3TestPixelCentered.png}
            ${TEMP}/ImageRegistration3TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration3>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration3TestPixelCentered.png
)

itk_add_test(NAME ImageRegistration4Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration4Test.png}
            ${TEMP}/ImageRegistration4Test.png
  $<TARGET_FILE:ImageRegistration4>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration4Test.png 0
        ${TEMP}/ImageRegistration4BeforeTest.png
        ${TEMP}/ImageRegistration4AfterTest.png
        24
)

itk_add_test(NAME ImageRegistration4Test2 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:ImageRegistration4>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration4Test2.png 0
        ${TEMP}/ImageRegistration4BeforeTest2.png
        ${TEMP}/ImageRegistration4AfterTest2.png
        5
)

itk_add_test(NAME ImageRegistration5Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration5Test1PixelCentered.png}
            ${TEMP}/ImageRegistration5Test1PixelCentered.png
            --compareNumberOfPixelsTolerance 115
  $<TARGET_FILE:ImageRegistration5>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceRotated10.png
        ${TEMP}/ImageRegistration5Test1PixelCentered.png
)

itk_add_test(NAME ImageRegistration5Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration5Test2PixelCentered.png}
            ${TEMP}/ImageRegistration5Test2PixelCentered.png
  $<TARGET_FILE:ImageRegistration5>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration5Test2PixelCentered.png
        ${TEMP}/ImageRegistration5Test2DifferenceAfter.png
        ${TEMP}/ImageRegistration5Test2DifferenceBefore.png
        1.3
)

itk_add_test(NAME ImageRegistration6Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration6TestPixelCentered.png}
            ${TEMP}/ImageRegistration6TestPixelCentered.png
            --compareNumberOfPixelsTolerance 100
  $<TARGET_FILE:ImageRegistration6>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration6TestPixelCentered.png
)

itk_add_test(NAME ImageRegistration7Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration7TestPixelCentered.png}
            ${TEMP}/ImageRegistration7TestPixelCentered.png
            --compareNumberOfPixelsTolerance 100
  $<TARGET_FILE:ImageRegistration7>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17S12.png
        ${TEMP}/ImageRegistration7TestPixelCentered.png
        ${TEMP}/ImageRegistration7BeforeTestPixelCentered.png
        ${TEMP}/ImageRegistration7AfterTestPixelCentered.png
        1.0   1.0  0.0
)

if(ITK_USE_BRAINWEB_DATA)

## A test needed to prepare for making the latex documentation.
itk_add_test(NAME ConfidenceConnected3DTest COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:ConfidenceConnected3D>
        DATA{../../Data/BrainWeb/brainweb165a10f17.mha}
        ${TEMP}/WhiteMatterSegmentation.mhd
)

itk_add_test(NAME ImageRegistration8Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration8RegisteredSlice.png}
            ${TEMP}/ImageRegistration8v4RegisteredSlice.png
  $<TARGET_FILE:ImageRegistration8>
        DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
        DATA{../../Data/BrainWeb/brainweb1e1a10f20Rot10Tx15.mha}
        ${TEMP}/ImageRegistration8v4Output.mhd
        ${TEMP}/ImageRegistration8v4DifferenceBefore.mhd
        ${TEMP}/ImageRegistration8v4DifferenceAfter.mhd
        ${TEMP}/ImageRegistration8v4Output.png
        ${TEMP}/ImageRegistration8v4DifferenceBefore.png
        ${TEMP}/ImageRegistration8v4DifferenceAfter.png
        ${TEMP}/ImageRegistration8v4RegisteredSlice.png
)
set_property(TEST ImageRegistration8Test APPEND PROPERTY LABELS RUNS_LONG)

if( ${CMAKE_BUILD_TYPE} MATCHES "Release")
  # This test is computationally demanding.
  # Therefore we only run it in a Release build
  itk_add_test(NAME ImageRegistration20Test COMMAND ${ITK_TEST_DRIVER}
    $<TARGET_FILE:ImageRegistration20>
      DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
      DATA{../../Data/BrainWeb/brainweb1e1a10f20Rot10Tx15.mha}
      ${TEMP}/ImageRegistration20Output.mhd
      ${TEMP}/ImageRegistration20DifferenceBefore.mhd
      ${TEMP}/ImageRegistration20DifferenceAfter.mhd
      0.1  # Step length
      300  # maximum number of iterations
    )
  set_tests_properties(ImageRegistration20Test PROPERTIES RUN_SERIAL 1)
endif()

endif()

itk_add_test(NAME ImageRegistration9Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration9TestPixelCentered.png}
            ${TEMP}/ImageRegistration9TestPixelCentered.png
            --compareNumberOfPixelsTolerance 50
  $<TARGET_FILE:ImageRegistration9>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration9TestPixelCentered.png
)

itk_add_test(NAME ImageRegistration10Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration10TestPixelCentered.png,:}
            ${TEMP}/ImageRegistration10TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration10>
        DATA{${ITK_SOURCE_DIR}/Testing/Data/Baseline/Segmentation/WatershedSegmentation1Test1.png}
        DATA{${ITK_SOURCE_DIR}/Testing/Data/Baseline/Segmentation/WatershedSegmentation1Test2.png}
        ${TEMP}/ImageRegistration10TestPixelCentered.png
        ${TEMP}/ImageRegistration10DifferenceTestPixelCentered.png
        -11 -13
)

itk_add_test(NAME ImageRegistration13Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration13TestPixelCentered.png}
            ${TEMP}/ImageRegistration13TestPixelCentered.png
  --compareNumberOfPixelsTolerance 35
  $<TARGET_FILE:ImageRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13TestPixelCentered.png
)

itk_add_test(NAME ImageRegistration14Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration14Test.png,:}
            ${TEMP}/ImageRegistration14Test.png
  --compareNumberOfPixelsTolerance 2000
  $<TARGET_FILE:ImageRegistration14>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration14Test.png 32 0.01 0.1  0.15  10.0  14.0
)
set_property(TEST ImageRegistration14Test APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME ImageRegistration15Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration15TestPixelCentered.png}
            ${TEMP}/ImageRegistration15TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration15>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration15TestPixelCentered.png 32 0.1 0.05  0.0 0.0
)
set_property(TEST ImageRegistration15Test APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME ImageRegistration16Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration16TestPixelCentered.png}
            ${TEMP}/ImageRegistration16TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration16>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration16TestPixelCentered.png 0.0 0.0
)

itk_add_test(NAME ImageRegistration16Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration16TestPixelCentered.png}
            ${TEMP}/ImageRegistration16Test2PixelCentered.png
  $<TARGET_FILE:ImageRegistration16>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration16Test2PixelCentered.png 0.0 0.0
        0
)

itk_add_test(NAME ImageRegistration17Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration17TestPixelCentered.png,:}
            ${TEMP}/ImageRegistration17TestPixelCentered.png
  --compareNumberOfPixelsTolerance 5
  $<TARGET_FILE:ImageRegistration17>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration17TestPixelCentered.png 0.0 0.0
)

itk_add_test(NAME ImageRegistration18Test COMMAND  ImageRegistration18
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration18Test.png 0.0 0.0
)

itk_add_test(NAME ImageRegistration19Test COMMAND ImageRegistration19
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration19Test.png
        ${TEMP}/ImageRegistration19DifferenceTest.png
        2.0 2.0 # Initial translation parameters
)

itk_add_test(NAME MultiResImageRegistration1Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/MultiResImageRegistration1Test.png,:}
            ${TEMP}/MultiResImageRegistration1Test.png
  --compareNumberOfPixelsTolerance 100
  --compareIntensityTolerance 7
  $<TARGET_FILE:MultiResImageRegistration1>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration1Test.png 100
        ${TEMP}/MultiResImageRegistration1CheckerBoardBeforeTest.png
        ${TEMP}/MultiResImageRegistration1CheckerBoardAfterTest.png
)
set_property(TEST MultiResImageRegistration1Test APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME MultiResImageRegistration1Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/MultiResImageRegistration1Test.png,:}
            ${TEMP}/MultiResImageRegistration1Test2.png
  --compareNumberOfPixelsTolerance 100
  --compareIntensityTolerance 7
  $<TARGET_FILE:MultiResImageRegistration1>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration1Test2.png 100
        ${TEMP}/MultiResImageRegistration1CheckerBoardBeforeTest2.png
        ${TEMP}/MultiResImageRegistration1CheckerBoardAfterTest2.png
        32
)
set_property(TEST MultiResImageRegistration1Test2 APPEND PROPERTY LABELS RUNS_LONG)


itk_add_test(NAME MultiResImageRegistration2Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/MultiResImageRegistration2Test.png,:}
            ${TEMP}/MultiResImageRegistration2Test.png
  --compareNumberOfPixelsTolerance 10000
  $<TARGET_FILE:MultiResImageRegistration2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration2Test.png  0
        ${TEMP}/MultiResImageRegistration2CheckerBoardBeforeTest.png
        ${TEMP}/MultiResImageRegistration2CheckerBoardAfterTest.png
)
set_property(TEST MultiResImageRegistration2Test APPEND PROPERTY LABELS RUNS_LONG)


itk_add_test(NAME MultiResImageRegistration2Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/MultiResImageRegistration2Test.png,:}
            ${TEMP}/MultiResImageRegistration2Test2.png
  --compareNumberOfPixelsTolerance 10000
  $<TARGET_FILE:MultiResImageRegistration2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration2Test2.png 0
        ${TEMP}/MultiResImageRegistration2CheckerBoardBeforeTest2.png
        ${TEMP}/MultiResImageRegistration2CheckerBoardAfterTest2.png
        0
)
set_property(TEST MultiResImageRegistration2Test2 APPEND PROPERTY LABELS RUNS_LONG)


itk_add_test(NAME MultiStageImageRegistration1Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration9TestPixelCentered.png,:}
            ${TEMP}/MultiStageImageRegistration1Test.png
  --compareNumberOfPixelsTolerance 10000
  $<TARGET_FILE:MultiStageImageRegistration1>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/MultiStageImageRegistration1Test.png  100
        ${TEMP}/MultiStageImageRegistration1CheckerBoardBeforeTest.png
        ${TEMP}/MultiStageImageRegistration1CheckerBoardAfterTest.png
)
set_property(TEST MultiStageImageRegistration1Test APPEND PROPERTY LABELS RUNS_LONG)


itk_add_test(NAME MultiStageImageRegistration2Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration9TestPixelCentered.png,:}
            ${TEMP}/MultiStageImageRegistration2Test.png
  --compareNumberOfPixelsTolerance 10000
  $<TARGET_FILE:MultiStageImageRegistration2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/MultiStageImageRegistration2Test.png  100
        ${TEMP}/MultiStageImageRegistration2CheckerBoardBeforeTest.png
        ${TEMP}/MultiStageImageRegistration2CheckerBoardAfterTest.png
)
set_property(TEST MultiStageImageRegistration2Test APPEND PROPERTY LABELS RUNS_LONG)


itk_add_test(NAME DeformableRegistration2Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/DeformableRegistration2Test.png}
            ${TEMP}/DeformableRegistration2Test.png
  $<TARGET_FILE:DeformableRegistration2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration2Test.png
)

itk_add_test(NAME DeformableRegistration3Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/DeformableRegistration3Test.png,:}
            ${TEMP}/DeformableRegistration3Test.png
  --compareNumberOfPixelsTolerance 100
  $<TARGET_FILE:DeformableRegistration3>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration3Test.png
)

itk_add_test(NAME DeformableRegistration4Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/DeformableRegistration1v4Test.png}
            ${TEMP}/DeformableRegistration4TestPixelCentered.png
  --compareNumberOfPixelsTolerance 30
  --compareIntensityTolerance 10
  $<TARGET_FILE:DeformableRegistration4>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration4TestPixelCentered.png
)

itk_add_test(NAME DeformableRegistration5Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/DeformableRegistration5Test.png,:}
            ${TEMP}/DeformableRegistration5Test.png
  --compareNumberOfPixelsTolerance 100
  --compareIntensityTolerance 70
  $<TARGET_FILE:DeformableRegistration5>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration5Test.png
)

itk_add_test(NAME DeformableRegistration6Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/DeformableRegistration1v4Test.png}
            ${TEMP}/DeformableRegistration6Test.png
  --compareNumberOfPixelsTolerance 20
  --compareIntensityTolerance 70
  $<TARGET_FILE:DeformableRegistration6>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration6Test.png
)
set_property(TEST DeformableRegistration6Test APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME BSplineWarping1Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/BSplineWarping1Test.png}
            ${TEMP}/BSplineWarping1Test.png
  $<TARGET_FILE:BSplineWarping1>
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${TEMP}/BSplineWarping1Test.png
        ${TEMP}/BSplineWarping1TestDisplacementField.mhd
        ${TEMP}/BSplineWarping1TestTransform.txt
)

itk_add_test(NAME DeformableRegistration12Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/DeformableRegistration12Test.png}
            ${TEMP}/DeformableRegistration12Test.png
  --compareNumberOfPixelsTolerance 600
  $<TARGET_FILE:DeformableRegistration12>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        DATA{Baseline/BSplineWarping1Test.png}
        ${TEMP}/DeformableRegistration12Test.png
        ${TEMP}/DeformableRegistration12TestDifferenceAfter.png
        ${TEMP}/DeformableRegistration12TestDifferenceBefore.png
        ${TEMP}/DeformableRegistration12TestDisplacementField.mhd
        ${TEMP}/DeformableRegistration12TestFinalTransformParameters.txt
)
set_property(TEST DeformableRegistration12Test APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME DeformableRegistration13Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/DeformableRegistration13Test.png}
            ${TEMP}/DeformableRegistration13Test1.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        DATA{Baseline/BSplineWarping1Test.png}
        ${TEMP}/DeformableRegistration13Test1.png
        ${TEMP}/DeformableRegistration13Test1DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test1DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test1DisplacementField.mhd
        0 0
        ${TEMP}/DeformableRegistration13Test1FinalTransformParameters.txt
)
set_property(TEST DeformableRegistration13Test1 APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME DeformableRegistration13Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/DeformableRegistration13Test.png}
            ${TEMP}/DeformableRegistration13Test2.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        DATA{Baseline/BSplineWarping1Test.png}
        ${TEMP}/DeformableRegistration13Test2.png
        ${TEMP}/DeformableRegistration13Test2DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test2DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test2DisplacementField.mhd
        0 1
        ${TEMP}/DeformableRegistration13Test2FinalTransformParameters.txt
)

itk_add_test(NAME DeformableRegistration13Test3 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/DeformableRegistration13Test.png}
            ${TEMP}/DeformableRegistration13Test3.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        DATA{Baseline/BSplineWarping1Test.png}
        ${TEMP}/DeformableRegistration13Test3.png
        ${TEMP}/DeformableRegistration13Test3DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test3DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test3DisplacementField.mhd
        1 0
        ${TEMP}/DeformableRegistration13Test3FinalTransformParameters.txt
)
set_property(TEST DeformableRegistration13Test3 APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME DeformableRegistration13Test4 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/DeformableRegistration13Test.png}
            ${TEMP}/DeformableRegistration13Test4.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        DATA{Baseline/BSplineWarping1Test.png}
        ${TEMP}/DeformableRegistration13Test4.png
        ${TEMP}/DeformableRegistration13Test4DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test4DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test4DisplacementField.mhd
        1 1
        ${TEMP}/DeformableRegistration13Test4FinalTransformParameters.txt
)

if( ITK_USE_BRAINWEB_DATA )

  itk_add_test(NAME BSplineWarping2Test COMMAND ${ITK_TEST_DRIVER}
    $<TARGET_FILE:BSplineWarping2>
          ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements2.txt
          DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
          DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
          ${TEMP}/BSplineWarping2Test.mhd
          ${TEMP}/BSplineWarping2TestDisplacementField.mhd
          ${TEMP}/BSplineWarping2TestTransform.txt
  )
  set_property(TEST BSplineWarping2Test APPEND PROPERTY LABELS RUNS_LONG)

  set( NUMBER_OF_BSPLINE_GRID_NODES 5 )

  if( DART_TESTING_TIMEOUT GREATER 3000 )

    itk_add_test(NAME DeformableRegistration8Test1 COMMAND ${ITK_TEST_DRIVER}
        --compare DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
                  ${TEMP}/DeformableRegistration8Test1.mhd
        --compareNumberOfPixelsTolerance 700000
        --compareIntensityTolerance 15
      $<TARGET_FILE:DeformableRegistration8>
            DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test1.mhd
            ${TEMP}/DeformableRegistration8Test1DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test1DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test1DisplacementField.mhd
            ${TEMP}/DeformableRegistration8Test1FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
            ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
    )
    set_property(TEST DeformableRegistration8Test1 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST DeformableRegistration8Test1 APPEND PROPERTY LABELS RUNS_LONG)

    itk_add_test(NAME DeformableRegistration14Test1 COMMAND ${ITK_TEST_DRIVER}
        --compare DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
                  ${TEMP}/DeformableRegistration14Test1.mhd
        --compareNumberOfPixelsTolerance 700000
        --compareIntensityTolerance 15
      $<TARGET_FILE:DeformableRegistration14>
            DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration14Test1.mhd
            ${TEMP}/DeformableRegistration14Test1DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration14Test1DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration14Test1DisplacementField.mhd
            0 0
            ${TEMP}/DeformableRegistration14Test1FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
            ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
            10.0 50
    )
    set_property(TEST DeformableRegistration14Test1 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST DeformableRegistration14Test1 APPEND PROPERTY LABELS RUNS_LONG)

    itk_add_test(NAME DeformableRegistration14Test2 COMMAND ${ITK_TEST_DRIVER}
        --compare DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
                  ${TEMP}/DeformableRegistration14Test2.mhd
        --compareNumberOfPixelsTolerance 700000
        --compareIntensityTolerance 15
      $<TARGET_FILE:DeformableRegistration14>
            DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration14Test2.mhd
            ${TEMP}/DeformableRegistration14Test2DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration14Test2DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration14Test2DisplacementField.mhd
            0 1
            ${TEMP}/DeformableRegistration14Test2FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
            ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
            10.0 50
    )
    set_property(TEST DeformableRegistration14Test2 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST DeformableRegistration14Test2 APPEND PROPERTY LABELS RUNS_LONG)

    itk_add_test(NAME DeformableRegistration14Test3 COMMAND ${ITK_TEST_DRIVER}
        --compare DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
                  ${TEMP}/DeformableRegistration14Test3.mhd
        --compareNumberOfPixelsTolerance 700000
        --compareIntensityTolerance 15
      $<TARGET_FILE:DeformableRegistration14>
            DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration14Test3.mhd
            ${TEMP}/DeformableRegistration14Test3DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration14Test3DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration14Test3DisplacementField.mhd
            1 0
            ${TEMP}/DeformableRegistration14Test3FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
            ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
            10.0 50
    )
    set_property(TEST DeformableRegistration14Test3 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST DeformableRegistration14Test3 APPEND PROPERTY LABELS RUNS_LONG)

    itk_add_test(NAME DeformableRegistration14Test4 COMMAND ${ITK_TEST_DRIVER}
        --compare DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
                  ${TEMP}/DeformableRegistration14Test4.mhd
        --compareNumberOfPixelsTolerance 700000
        --compareIntensityTolerance 15
      $<TARGET_FILE:DeformableRegistration14>
            DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration14Test4.mhd
            ${TEMP}/DeformableRegistration14Test4DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration14Test4DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration14Test4DisplacementField.mhd
            1 1
            ${TEMP}/DeformableRegistration14Test4FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
            ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
            10.0 50
    )
    set_property(TEST DeformableRegistration14Test4 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST DeformableRegistration14Test4 APPEND PROPERTY LABELS RUNS_LONG)

  endif() # DART_TESTING_TIMEOUT GREATER 3000

  itk_add_test(NAME ChangeInformationTest1 COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:ChangeInformationImageFilter>
          DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
          ${TEMP}/BSplinedTranlatedScaledRotated.mhd
          1.05
          1.0 2.0 0.0
          10.0
  )

  if( DART_TESTING_TIMEOUT GREATER 10000 )
    itk_add_test(NAME DeformableRegistration15Test1 COMMAND ${ITK_TEST_DRIVER}
        --compare DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
                  ${TEMP}/DeformableRegistration15Test1.mhd
        --compareNumberOfPixelsTolerance 700000
        --compareIntensityTolerance 15
      $<TARGET_FILE:DeformableRegistration15>
            DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplinedTranlatedScaledRotated.mhd
            ${TEMP}/DeformableRegistration15Test1.mhd
            ${TEMP}/DeformableRegistration15Test1DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration15Test1DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration15Test1DisplacementField.mhd
            0 0
            ${TEMP}/DeformableRegistration15Test1FinalTransformParameters.txt
            5 100
            10.0 50
    )
    set_property(TEST DeformableRegistration15Test1 PROPERTY DEPENDS ChangeInformationTest1)
    set_property(TEST DeformableRegistration15Test1 APPEND PROPERTY LABELS RUNS_LONG)
  endif()

  if( "${ITK_COMPUTER_MEMORY_SIZE}" GREATER 6 )

    set(NUMBER_OF_BSPLINE_GRID_NODES 32)
    itk_add_test(NAME DeformableRegistration8Test2 COMMAND ${ITK_TEST_DRIVER}
      $<TARGET_FILE:DeformableRegistration8>
            DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test2.mhd
            ${TEMP}/DeformableRegistration8Test2DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test2DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test2DisplacementField.mhd
            ${TEMP}/DeformableRegistration8Test2FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
    )
    set_property(TEST DeformableRegistration8Test2 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST DeformableRegistration8Test2 APPEND PROPERTY LABELS RUNS_LONG)

    set(NUMBER_OF_BSPLINE_GRID_NODES 100)
    itk_add_test(NAME DeformableRegistration8Test3 COMMAND ${ITK_TEST_DRIVER}
      $<TARGET_FILE:DeformableRegistration8>
            DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test3.mhd
            ${TEMP}/DeformableRegistration8Test3DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test3DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test3DisplacementField.mhd
            ${TEMP}/DeformableRegistration8Test3FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
    )
    set_property(TEST DeformableRegistration8Test3 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST DeformableRegistration8Test3 APPEND PROPERTY LABELS RUNS_LONG)

    # Due to the large memory requirements this tests must be run one by one
    set_tests_properties(
      DeformableRegistration8Test2
      DeformableRegistration8Test3
      PROPERTIES
      RESOURCE_LOCK MEMORY_SIZE
      )

  endif() # ITK_COMPUTER_MEMORY_SIZE GREATER 6

endif() # ITK_USE_BRAINWEB_DATA


if(ITK_MRI_UNC_DATABASE_DATA_ROOT)

if( DART_TESTING_TIMEOUT GREATER 10000 )
  itk_add_test(NAME DeformableRegistration15Test2 COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:DeformableRegistration15>
          ${ITK_MRI_UNC_DATABASE_DATA_ROOT}/b/I.hdr
          ${ITK_MRI_UNC_DATABASE_DATA_ROOT}/d/I.hdr
          ${TEMP}/DeformableRegistration15Test2.mhd
          ${TEMP}/DeformableRegistration15Test2DifferenceAfter.mhd
          ${TEMP}/DeformableRegistration15Test2DifferenceBefore.mhd
          ${TEMP}/DeformableRegistration15Test2DisplacementField.mhd
          0 0
          ${TEMP}/DeformableRegistration15Test2FinalTransformParameters.txt
          5 100
          10.0 50
  )
  set_property(TEST DeformableRegistration15Test2 APPEND PROPERTY LABELS RUNS_LONG)
endif()

endif()


itk_add_test(NAME LandmarkWarping2Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/LandmarkWarping2Test.png}
            ${TEMP}/LandmarkWarping2Test.png
  $<TARGET_FILE:LandmarkWarping2>
        ${ITK_SOURCE_DIR}/Examples/Data/LandmarkWarping2Landmarks1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${TEMP}/LandmarkWarping2Test.png
)
itk_add_test(NAME ImageRegistration12Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration12TestPixelCentered.png}
            ${TEMP}/ImageRegistration12TestPixelCentered.png
  --compareNumberOfPixelsTolerance 2
  $<TARGET_FILE:ImageRegistration12>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration12TestPixelCentered.png
)

itk_add_test(NAME ModelToImageRegistration2Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:ModelToImageRegistration2>
        DATA{${ITK_SOURCE_DIR}/Testing/Data/Input/cake_easy.png}  12 12
)

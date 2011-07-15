project(ImageRegistration)
include_regular_expression("^.*$")

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Registration)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)


add_executable(ImageRegistration3 ImageRegistration3.cxx )
target_link_libraries(ImageRegistration3 ${ITK_LIBRARIES})

add_executable(ImageRegistration5 ImageRegistration5.cxx )
target_link_libraries(ImageRegistration5 ${ITK_LIBRARIES})

add_executable(ImageRegistration6 ImageRegistration6.cxx )
target_link_libraries(ImageRegistration6 ${ITK_LIBRARIES})

add_executable(ImageRegistration8 ImageRegistration8.cxx )
target_link_libraries(ImageRegistration8 ${ITK_LIBRARIES})

add_executable(ImageRegistration9 ImageRegistration9.cxx )
target_link_libraries(ImageRegistration9 ${ITK_LIBRARIES})

add_executable(ImageRegistration10 ImageRegistration10.cxx )
target_link_libraries(ImageRegistration10 ${ITK_LIBRARIES})

add_executable(DeformableRegistration4 DeformableRegistration4.cxx )
target_link_libraries(DeformableRegistration4 ${ITK_LIBRARIES})

add_executable(ImageRegistration4 ImageRegistration4.cxx )
target_link_libraries(ImageRegistration4 ${ITK_LIBRARIES})

add_executable(ImageRegistration7 ImageRegistration7.cxx )
target_link_libraries(ImageRegistration7 ${ITK_LIBRARIES})

add_executable(ImageRegistration11 ImageRegistration11.cxx )
target_link_libraries(ImageRegistration11 ${ITK_LIBRARIES})

add_executable(ImageRegistration12 ImageRegistration12.cxx )
target_link_libraries(ImageRegistration12 ${ITK_LIBRARIES})

add_executable(ImageRegistration13 ImageRegistration13.cxx )
target_link_libraries(ImageRegistration13 ${ITK_LIBRARIES})

add_executable(ImageRegistration14 ImageRegistration14.cxx )
target_link_libraries(ImageRegistration14 ${ITK_LIBRARIES})

add_executable(ImageRegistration15 ImageRegistration15.cxx )
target_link_libraries(ImageRegistration15 ${ITK_LIBRARIES})

add_executable(ImageRegistration16 ImageRegistration16.cxx )
target_link_libraries(ImageRegistration16 ${ITK_LIBRARIES})

add_executable(ImageRegistration18 ImageRegistration18.cxx )
target_link_libraries(ImageRegistration18 ${ITK_LIBRARIES})

add_executable(ImageRegistration19 ImageRegistration19.cxx )
target_link_libraries(ImageRegistration19 ${ITK_LIBRARIES})

add_executable(ImageRegistration20 ImageRegistration20.cxx )
target_link_libraries(ImageRegistration20 ${ITK_LIBRARIES})

add_executable(DeformableRegistration6 DeformableRegistration6.cxx )
target_link_libraries(DeformableRegistration6 ${ITK_LIBRARIES})

add_executable(DeformableRegistration7 DeformableRegistration7.cxx )
target_link_libraries(DeformableRegistration7 ${ITK_LIBRARIES})

add_executable(DeformableRegistration8 DeformableRegistration8.cxx )
target_link_libraries(DeformableRegistration8 ${ITK_LIBRARIES})

if( USE_FFTWD )
  add_executable(DeformableRegistration9 DeformableRegistration9.cxx )
  target_link_libraries(DeformableRegistration9 ${ITK_LIBRARIES})

  add_executable(DeformableRegistration10 DeformableRegistration10.cxx )
  target_link_libraries(DeformableRegistration10 ${ITK_LIBRARIES})
endif( USE_FFTWD )

add_executable(MultiResImageRegistration1 MultiResImageRegistration1.cxx )
target_link_libraries(MultiResImageRegistration1 ${ITK_LIBRARIES})

add_executable(MultiResImageRegistration2 MultiResImageRegistration2.cxx )
target_link_libraries(MultiResImageRegistration2 ${ITK_LIBRARIES})

add_executable(MultiResImageRegistration3 MultiResImageRegistration3.cxx )
target_link_libraries(MultiResImageRegistration3 ${ITK_LIBRARIES})

## VAM FIXME HACK DeformableRegistration1 and DeformableRegistration11
# both depend on FEM framework, and need to be updated to
# accomodate the api
#add_executable(DeformableRegistration1 DeformableRegistration1.cxx )
#target_link_libraries(DeformableRegistration1 ${ITK_LIBRARIES})
#
#add_executable(DeformableRegistration11 DeformableRegistration11.cxx )
#target_link_libraries(DeformableRegistration11 ${ITK_LIBRARIES})

add_executable(DeformableRegistration2 DeformableRegistration2.cxx )
target_link_libraries(DeformableRegistration2 ${ITK_LIBRARIES})

add_executable(DeformableRegistration3 DeformableRegistration3.cxx )
target_link_libraries(DeformableRegistration3 ${ITK_LIBRARIES})

add_executable(DeformableRegistration5 DeformableRegistration5.cxx )
target_link_libraries(DeformableRegistration5 ${ITK_LIBRARIES})

add_executable(DeformableRegistration12 DeformableRegistration12.cxx )
target_link_libraries(DeformableRegistration12 ${ITK_LIBRARIES})

add_executable(DeformableRegistration13 DeformableRegistration13.cxx )
target_link_libraries(DeformableRegistration13 ${ITK_LIBRARIES})

add_executable(DeformableRegistration14 DeformableRegistration14.cxx )
target_link_libraries(DeformableRegistration14 ${ITK_LIBRARIES})

add_executable(ChangeInformationImageFilter ChangeInformationImageFilter.cxx )
target_link_libraries(ChangeInformationImageFilter ${ITK_LIBRARIES})

add_executable(DeformableRegistration15 DeformableRegistration15.cxx )
target_link_libraries(DeformableRegistration15 ${ITK_LIBRARIES})

add_executable(DeformableRegistration16 DeformableRegistration16.cxx )
target_link_libraries(DeformableRegistration16 ${ITK_LIBRARIES})

add_executable(DeformableRegistration17 DeformableRegistration17.cxx )
target_link_libraries(DeformableRegistration17 ${ITK_LIBRARIES})

add_executable(DeformationFieldInitialization DeformationFieldInitialization.cxx )
target_link_libraries(DeformationFieldInitialization ${ITK_LIBRARIES})

add_executable(LandmarkWarping2 LandmarkWarping2.cxx )
target_link_libraries(LandmarkWarping2 ${ITK_LIBRARIES})

add_executable(BSplineWarping1 BSplineWarping1.cxx )
target_link_libraries(BSplineWarping1 ${ITK_LIBRARIES})

add_executable(BSplineWarping2 BSplineWarping2.cxx )
target_link_libraries(BSplineWarping2 ${ITK_LIBRARIES})

add_executable(ThinPlateSplineWarp ThinPlateSplineWarp.cxx )
target_link_libraries(ThinPlateSplineWarp ${ITK_LIBRARIES})

add_executable(ModelToImageRegistration1 ModelToImageRegistration1.cxx )
target_link_libraries(ModelToImageRegistration1 ${ITK_LIBRARIES})

add_executable(ModelToImageRegistration2 ModelToImageRegistration2.cxx )
target_link_libraries(ModelToImageRegistration2 ${ITK_LIBRARIES})

add_executable(ImageRegistration1 ImageRegistration1.cxx )
target_link_libraries(ImageRegistration1 ${ITK_LIBRARIES})

add_executable(ImageRegistration2 ImageRegistration2.cxx )
target_link_libraries(ImageRegistration2 ${ITK_LIBRARIES})

add_executable(ImageRegistration17 ImageRegistration17.cxx )
target_link_libraries(ImageRegistration17 ${ITK_LIBRARIES})

#TODO needs updating to new statistics  add_executable(ImageRegistrationHistogramPlotter ImageRegistrationHistogramPlotter.cxx )
#TODO needs updating to new statistics  target_link_libraries(ImageRegistrationHistogramPlotter ${ITK_LIBRARIES})

add_executable(MeanSquaresImageMetric1  MeanSquaresImageMetric1.cxx )
target_link_libraries(MeanSquaresImageMetric1 ${ITK_LIBRARIES})

add_executable(DeformationFieldJacobian DeformationFieldJacobian.cxx )
target_link_libraries(DeformationFieldJacobian ${ITK_LIBRARIES})

add_executable(IterativeClosestPoint1   IterativeClosestPoint1.cxx )
target_link_libraries(IterativeClosestPoint1 ${ITK_LIBRARIES})

add_executable(IterativeClosestPoint2   IterativeClosestPoint2.cxx )
target_link_libraries(IterativeClosestPoint2 ${ITK_LIBRARIES})

add_executable(IterativeClosestPoint3   IterativeClosestPoint3.cxx )
target_link_libraries(IterativeClosestPoint3 ${ITK_LIBRARIES})


if( NOT ITK_DISABLE_CXX_TESTING )

if(BUILD_TESTING)

itk_add_test(NAME ImageRegistration1Test COMMAND  ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration1TestPixelCentered.png
            ${TEMP}/ImageRegistration1TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration1>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1TestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceAfterTestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceBeforeTestPixelCentered.png
)

itk_add_test(NAME ImageRegistration2Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration2Test.png
            ${TEMP}/ImageRegistration2Test.png
  $<TARGET_FILE:ImageRegistration2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration2Test.png
        ${TEMP}/ImageRegistration2BeforeTest.png
        ${TEMP}/ImageRegistration2AfterTest.png
)

itk_add_test(NAME ImageRegistration3Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration3TestPixelCentered.png
            ${TEMP}/ImageRegistration3TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration3>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration3TestPixelCentered.png
)

itk_add_test(NAME ImageRegistration4Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration4Test.png
            ${TEMP}/ImageRegistration4Test.png
  $<TARGET_FILE:ImageRegistration4>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration4Test.png 0
        ${TEMP}/ImageRegistration4BeforeTest.png
        ${TEMP}/ImageRegistration4AfterTest.png
        24 10000 1
)

itk_add_test(NAME ImageRegistration4Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration4Test.png
            ${TEMP}/ImageRegistration4Test2.png
  $<TARGET_FILE:ImageRegistration4>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration4Test2.png 0
        ${TEMP}/ImageRegistration4BeforeTest.png
        ${TEMP}/ImageRegistration4AfterTest.png
        24 10000 0
)

itk_add_test(NAME ImageRegistration4Test3 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:ImageRegistration4>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration4Test3.png 0
        ${TEMP}/ImageRegistration4BeforeTest3.png
        ${TEMP}/ImageRegistration4AfterTest3.png
        5 10000 0
)

itk_add_test(NAME ImageRegistration5Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration5Test1PixelCentered.png
            ${TEMP}/ImageRegistration5Test1PixelCentered.png
  $<TARGET_FILE:ImageRegistration5>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceRotated10.png
        ${TEMP}/ImageRegistration5Test1PixelCentered.png
)

itk_add_test(NAME ImageRegistration5Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration5Test2PixelCentered.png
            ${TEMP}/ImageRegistration5Test2PixelCentered.png
  $<TARGET_FILE:ImageRegistration5>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration5Test2PixelCentered.png
)

itk_add_test(NAME ImageRegistration6Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration6TestPixelCentered.png
            ${TEMP}/ImageRegistration6TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration6>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration6TestPixelCentered.png
)

itk_add_test(NAME ImageRegistration7Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration7TestPixelCentered.png
            ${TEMP}/ImageRegistration7TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration7>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17S12.png
        ${TEMP}/ImageRegistration7TestPixelCentered.png
        ${TEMP}/ImageRegistration7BeforeTestPixelCentered.png
        ${TEMP}/ImageRegistration7AfterTestPixelCentered.png
        1.0   1.0  0.0
)

if(ITK_USE_BRAINWEB_DATA)
itk_add_test(NAME ImageRegistration8Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration8RegisteredSlice.png
            ${TEMP}/ImageRegistration8RegisteredSlice.png
  $<TARGET_FILE:ImageRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        DATA{../Data/BrainWeb/brainweb1e1a10f20Rot10Tx15.mha}
        ${TEMP}/ImageRegistration8Output.mhd
        ${TEMP}/ImageRegistration8DifferenceBefore.mhd
        ${TEMP}/ImageRegistration8DifferenceAfter.mhd
        ${TEMP}/ImageRegistration8Output.png
        ${TEMP}/ImageRegistration8DifferenceBefore.png
        ${TEMP}/ImageRegistration8DifferenceAfter.png
        ${TEMP}/ImageRegistration8RegisteredSlice.png
)

if( ${CMAKE_BUILD_TYPE} MATCHES "Release")
  # This test is computationally demanding.
  # Therefore we only run it in a Release build
  itk_add_test(NAME ImageRegistration20Test COMMAND ${ITK_TEST_DRIVER}
    $<TARGET_FILE:ImageRegistration20>
      DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
      DATA{../Data/BrainWeb/brainweb1e1a10f20Rot10Tx15.mha}
      ${TEMP}/ImageRegistration20Output.mhd
      ${TEMP}/ImageRegistration20DifferenceBefore.mhd
      ${TEMP}/ImageRegistration20DifferenceAfter.mhd
      0.1  # Step length
      300  # maximum number of iterations
    )
  set_tests_properties(ImageRegistration20Test PROPERTIES RUN_SERIAL 1)
endif( ${CMAKE_BUILD_TYPE} MATCHES "Release")

endif(ITK_USE_BRAINWEB_DATA)

itk_add_test(NAME ImageRegistration9Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration9TestPixelCentered.png
            ${TEMP}/ImageRegistration9TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration9>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration9TestPixelCentered.png
)

itk_add_test(NAME ImageRegistration10Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration10TestPixelCentered.png
            ${TEMP}/ImageRegistration10TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration10>
        ${ITK_SOURCE_DIR}/Testing/Data/Baseline/Segmentation/WatershedSegmentation1Test1.png
        ${ITK_SOURCE_DIR}/Testing/Data/Baseline/Segmentation/WatershedSegmentation1Test2.png
        ${TEMP}/ImageRegistration10TestPixelCentered.png
        ${TEMP}/ImageRegistration10DifferenceTestPixelCentered.png
        -11 -13
)

itk_add_test(NAME ImageRegistration13Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13TestPixelCentered.png
  --compareNumberOfPixelsTolerance 5
  $<TARGET_FILE:ImageRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13TestPixelCentered.png
)

itk_add_test(NAME ImageRegistration13Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test2PixelCentered.png
  --compareNumberOfPixelsTolerance 10
  $<TARGET_FILE:ImageRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test2PixelCentered.png
        0 0
)

itk_add_test(NAME ImageRegistration13Test3 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test3PixelCentered.png
  --compareNumberOfPixelsTolerance 10
  $<TARGET_FILE:ImageRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test3PixelCentered.png
        0 1
)

itk_add_test(NAME ImageRegistration13Test4 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test4PixelCentered.png
  --compareNumberOfPixelsTolerance 5
  $<TARGET_FILE:ImageRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test4PixelCentered.png
        1 0
)

itk_add_test(NAME ImageRegistration13Test5 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test5PixelCentered.png
  --compareNumberOfPixelsTolerance 5
  $<TARGET_FILE:ImageRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test5PixelCentered.png
        1 1
)

itk_add_test(NAME ImageRegistration14Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration14Test.png
            ${TEMP}/ImageRegistration14Test.png
  --compareNumberOfPixelsTolerance 2000
  $<TARGET_FILE:ImageRegistration14>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration14Test.png 32 0.01 0.1  0.15  10.0  14.0
)

itk_add_test(NAME ImageRegistration15Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration15TestPixelCentered.png
            ${TEMP}/ImageRegistration15TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration15>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration15TestPixelCentered.png 32 0.1 0.05  0.0 0.0
)

itk_add_test(NAME ImageRegistration16Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration16TestPixelCentered.png
            ${TEMP}/ImageRegistration16TestPixelCentered.png
  $<TARGET_FILE:ImageRegistration16>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration16TestPixelCentered.png 0.0 0.0
)

itk_add_test(NAME ImageRegistration16Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration16TestPixelCentered.png
            ${TEMP}/ImageRegistration16Test2PixelCentered.png
  $<TARGET_FILE:ImageRegistration16>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration16Test2PixelCentered.png 0.0 0.0
        0
)

itk_add_test(NAME ImageRegistration17Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration17TestPixelCentered.png
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
  --compare ${BASELINE}/MultiResImageRegistration1Test.png
            ${TEMP}/MultiResImageRegistration1Test.png
  --compareNumberOfPixelsTolerance 100
  $<TARGET_FILE:MultiResImageRegistration1>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration1Test.png 100
        ${TEMP}/MultiResImageRegistration1CheckerBoardBeforeTest.png
        ${TEMP}/MultiResImageRegistration1CheckerBoardAfterTest.png
)

itk_add_test(NAME MultiResImageRegistration1Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/MultiResImageRegistration1Test.png
            ${TEMP}/MultiResImageRegistration1Test2.png
  --compareNumberOfPixelsTolerance 100
  $<TARGET_FILE:MultiResImageRegistration1>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration1Test2.png 100
        ${TEMP}/MultiResImageRegistration1CheckerBoardBeforeTest2.png
        ${TEMP}/MultiResImageRegistration1CheckerBoardAfterTest2.png
        0
)


itk_add_test(NAME MultiResImageRegistration2Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/MultiResImageRegistration2Test.png
            ${TEMP}/MultiResImageRegistration2Test.png
  --compareNumberOfPixelsTolerance 10000
  $<TARGET_FILE:MultiResImageRegistration2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration2Test.png  0
        ${TEMP}/MultiResImageRegistration2CheckerBoardBeforeTest.png
        ${TEMP}/MultiResImageRegistration2CheckerBoardAfterTest.png
)


itk_add_test(NAME MultiResImageRegistration2Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/MultiResImageRegistration2Test.png
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


itk_add_test(NAME DeformableRegistration2Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration2Test.png
            ${TEMP}/DeformableRegistration2Test.png
  $<TARGET_FILE:DeformableRegistration2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration2Test.png
)

itk_add_test(NAME DeformableRegistration3Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration3Test.png
            ${TEMP}/DeformableRegistration3Test.png
  --compareNumberOfPixelsTolerance 100
  $<TARGET_FILE:DeformableRegistration3>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration3Test.png
)

itk_add_test(NAME DeformableRegistration4Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration4TestPixelCentered.png
            ${TEMP}/DeformableRegistration4TestPixelCentered.png
  $<TARGET_FILE:DeformableRegistration4>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration4TestPixelCentered.png
)

itk_add_test(NAME DeformableRegistration5Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration5Test.png
            ${TEMP}/DeformableRegistration5Test.png
  --compareNumberOfPixelsTolerance 5
  $<TARGET_FILE:DeformableRegistration5>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration5Test.png
)

itk_add_test(NAME DeformableRegistration6Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration6TestPixelCentered.png
            ${TEMP}/DeformableRegistration6TestPixelCentered.png
  $<TARGET_FILE:DeformableRegistration6>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration6TestPixelCentered.png
)

itk_add_test(NAME BSplineWarping1Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/BSplineWarping1Test.png
            ${TEMP}/BSplineWarping1Test.png
  $<TARGET_FILE:BSplineWarping1>
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${TEMP}/BSplineWarping1Test.png
        ${TEMP}/BSplineWarping1TestDeformationField.mhd
        ${TEMP}/BSplineWarping1TestTransform.txt
)

itk_add_test(NAME DeformableRegistration12Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration12Test.png
            ${TEMP}/DeformableRegistration12Test1.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration12>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration12Test1.png
        ${TEMP}/DeformableRegistration12Test1DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test1DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test1DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration12Test1FinalTransformParameters.txt
)

itk_add_test(NAME DeformableRegistration12Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration12Test.png
            ${TEMP}/DeformableRegistration12Test2.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration12>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration12Test2.png
        ${TEMP}/DeformableRegistration12Test2DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test2DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test2DeformationField.mhd
        0 1
        ${TEMP}/DeformableRegistration12Test2FinalTransformParameters.txt
)

itk_add_test(NAME DeformableRegistration12Test3 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration12Test.png
            ${TEMP}/DeformableRegistration12Test3.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration12>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration12Test3.png
        ${TEMP}/DeformableRegistration12Test3DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test3DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test3DeformationField.mhd
        1 0
        ${TEMP}/DeformableRegistration12Test3FinalTransformParameters.txt
)

itk_add_test(NAME DeformableRegistration12Test4 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration12Test.png
            ${TEMP}/DeformableRegistration12Test4.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration12>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration12Test4.png
        ${TEMP}/DeformableRegistration12Test4DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test4DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test4DeformationField.mhd
        1 1
        ${TEMP}/DeformableRegistration12Test4FinalTransformParameters.txt
)

itk_add_test(NAME DeformableRegistration13Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration13Test.png
            ${TEMP}/DeformableRegistration13Test1.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration13Test1.png
        ${TEMP}/DeformableRegistration13Test1DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test1DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test1DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration13Test1FinalTransformParameters.txt
)

itk_add_test(NAME DeformableRegistration13Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration13Test.png
            ${TEMP}/DeformableRegistration13Test2.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration13Test2.png
        ${TEMP}/DeformableRegistration13Test2DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test2DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test2DeformationField.mhd
        0 1
        ${TEMP}/DeformableRegistration13Test2FinalTransformParameters.txt
)

itk_add_test(NAME DeformableRegistration13Test3 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration13Test.png
            ${TEMP}/DeformableRegistration13Test3.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration13Test3.png
        ${TEMP}/DeformableRegistration13Test3DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test3DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test3DeformationField.mhd
        1 0
        ${TEMP}/DeformableRegistration13Test3FinalTransformParameters.txt
)

itk_add_test(NAME DeformableRegistration13Test4 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DeformableRegistration13Test.png
            ${TEMP}/DeformableRegistration13Test4.png
  --compareNumberOfPixelsTolerance 400
  $<TARGET_FILE:DeformableRegistration13>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration13Test4.png
        ${TEMP}/DeformableRegistration13Test4DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test4DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test4DeformationField.mhd
        1 1
        ${TEMP}/DeformableRegistration13Test4FinalTransformParameters.txt
)

if(ITK_USE_BRAINWEB_DATA)

itk_add_test(NAME BSplineWarping2Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:BSplineWarping2>
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements2.txt
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/BSplineWarping2TestDeformationField.mhd
        ${TEMP}/BSplineWarping2TestTransform.txt
)

set(NUMBER_OF_BSPLINE_GRID_NODES 5)

if( DART_TESTING_TIMEOUT GREATER 3000 )

itk_add_test(NAME DeformableRegistration8Test1 COMMAND ${ITK_TEST_DRIVER}
    --compare DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
              ${TEMP}/DeformableRegistration8Test1.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test1.mhd
        ${TEMP}/DeformableRegistration8Test1DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test1DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test1DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration8Test1FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
        ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
)
set_property(TEST DeformableRegistration8Test1 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration8Test2 COMMAND ${ITK_TEST_DRIVER}
    --compare DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
              ${TEMP}/DeformableRegistration8Test2.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test2.mhd
        ${TEMP}/DeformableRegistration8Test2DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test2DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test2DeformationField.mhd
        1 0
        ${TEMP}/DeformableRegistration8Test2FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
        ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
)
set_property(TEST DeformableRegistration8Test2 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration8Test3 COMMAND ${ITK_TEST_DRIVER}
    --compare DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
              ${TEMP}/DeformableRegistration8Test3.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test3.mhd
        ${TEMP}/DeformableRegistration8Test3DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test3DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test3DeformationField.mhd
        0 1
        ${TEMP}/DeformableRegistration8Test3FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
        ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
)
set_property(TEST DeformableRegistration8Test3 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration8Test4 COMMAND ${ITK_TEST_DRIVER}
    --compare DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
              ${TEMP}/DeformableRegistration8Test4.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test4.mhd
        ${TEMP}/DeformableRegistration8Test4DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test4DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test4DeformationField.mhd
        1 1
        ${TEMP}/DeformableRegistration8Test4FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
        ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
)
set_property(TEST DeformableRegistration8Test4 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration14Test1 COMMAND ${ITK_TEST_DRIVER}
    --compare DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
              ${TEMP}/DeformableRegistration14Test1.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  $<TARGET_FILE:DeformableRegistration14>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration14Test1.mhd
        ${TEMP}/DeformableRegistration14Test1DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration14Test1DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration14Test1DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration14Test1FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
        ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
        10.0 50
)
set_property(TEST DeformableRegistration14Test1 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration14Test2 COMMAND ${ITK_TEST_DRIVER}
    --compare DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
              ${TEMP}/DeformableRegistration14Test2.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  $<TARGET_FILE:DeformableRegistration14>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration14Test2.mhd
        ${TEMP}/DeformableRegistration14Test2DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration14Test2DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration14Test2DeformationField.mhd
        0 1
        ${TEMP}/DeformableRegistration14Test2FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
        ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
        10.0 50
)
set_property(TEST DeformableRegistration14Test2 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration14Test3 COMMAND ${ITK_TEST_DRIVER}
    --compare DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
              ${TEMP}/DeformableRegistration14Test3.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  $<TARGET_FILE:DeformableRegistration14>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration14Test3.mhd
        ${TEMP}/DeformableRegistration14Test3DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration14Test3DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration14Test3DeformationField.mhd
        1 0
        ${TEMP}/DeformableRegistration14Test3FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
        ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
        10.0 50
)
set_property(TEST DeformableRegistration14Test3 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration14Test4 COMMAND ${ITK_TEST_DRIVER}
    --compare DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
              ${TEMP}/DeformableRegistration14Test4.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  $<TARGET_FILE:DeformableRegistration14>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration14Test4.mhd
        ${TEMP}/DeformableRegistration14Test4DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration14Test4DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration14Test4DeformationField.mhd
        1 1
        ${TEMP}/DeformableRegistration14Test4FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
        ${ITK_SOURCE_DIR}/Examples/Data/IdentityTransform.tfm
        10.0 50
)
set_property(TEST DeformableRegistration14Test4 PROPERTY DEPENDS BSplineWarping2Test)
endif( DART_TESTING_TIMEOUT GREATER 3000 )

itk_add_test(NAME ChangeInformationTest1 COMMAND ${ITK_TEST_DRIVER}
        $<TARGET_FILE:ChangeInformationImageFilter>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplinedTranlatedScaledRotated.mhd
        1.05
        1.0 2.0 0.0
        10.0
)

if( DART_TESTING_TIMEOUT GREATER 10000 )
itk_add_test(NAME DeformableRegistration15Test1 COMMAND ${ITK_TEST_DRIVER}
    --compare DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
              ${TEMP}/DeformableRegistration15Test1.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  $<TARGET_FILE:DeformableRegistration15>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplinedTranlatedScaledRotated.mhd
        ${TEMP}/DeformableRegistration15Test1.mhd
        ${TEMP}/DeformableRegistration15Test1DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration15Test1DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration15Test1DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration15Test1FinalTransformParameters.txt
        5 100
        10.0 50
)
set_property(TEST DeformableRegistration15Test1 PROPERTY DEPENDS ChangeInformationTest1)
endif( DART_TESTING_TIMEOUT GREATER 10000 )

if( "${ITK_COMPUTER_MEMORY_SIZE}" GREATER 6 )

set(NUMBER_OF_BSPLINE_GRID_NODES 32)
itk_add_test(NAME DeformableRegistration8Test5 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test5.mhd
        ${TEMP}/DeformableRegistration8Test5DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test5DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test5DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration8Test5FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)
set_property(TEST DeformableRegistration8Test5 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration8Test6 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test6.mhd
        ${TEMP}/DeformableRegistration8Test6DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test6DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test6DeformationField.mhd
        0 1
        ${TEMP}/DeformableRegistration8Test6FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)
set_property(TEST DeformableRegistration8Test6 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration8Test7 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test7.mhd
        ${TEMP}/DeformableRegistration8Test7DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test7DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test7DeformationField.mhd
        1 0
        ${TEMP}/DeformableRegistration8Test7FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)
set_property(TEST DeformableRegistration8Test7 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration8Test8 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test8.mhd
        ${TEMP}/DeformableRegistration8Test8DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test8DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test8DeformationField.mhd
        1 1
        ${TEMP}/DeformableRegistration8Test8FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)
set_property(TEST DeformableRegistration8Test8 PROPERTY DEPENDS BSplineWarping2Test)

set(NUMBER_OF_BSPLINE_GRID_NODES 100)
itk_add_test(NAME DeformableRegistration8Test9 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test9.mhd
        ${TEMP}/DeformableRegistration8Test9DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test9DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test9DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration8Test9FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)
set_property(TEST DeformableRegistration8Test9 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration8Test10 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test10.mhd
        ${TEMP}/DeformableRegistration8Test10DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test10DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test10DeformationField.mhd
        0 1
        ${TEMP}/DeformableRegistration8Test10FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)
set_property(TEST DeformableRegistration8Test10 PROPERTY DEPENDS BSplineWarping2Test)

# Due to the large memory requirements this tests must be run one by one
set_tests_properties(
  DeformableRegistration8Test5
  DeformableRegistration8Test6
  DeformableRegistration8Test7
  DeformableRegistration8Test8
  DeformableRegistration8Test9
  DeformableRegistration8Test10
  PROPERTIES
  RUN_SERIAL 1
  )

if( "${ITK_COMPUTER_MEMORY_SIZE}" GREATER 16 )

itk_add_test(NAME DeformableRegistration8Test11 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test11.mhd
        ${TEMP}/DeformableRegistration8Test11DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test11DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test11DeformationField.mhd
        1 0
        ${TEMP}/DeformableRegistration8Test11FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)
set_property(TEST DeformableRegistration8Test11 PROPERTY DEPENDS BSplineWarping2Test)

itk_add_test(NAME DeformableRegistration8Test12 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:DeformableRegistration8>
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test12.mhd
        ${TEMP}/DeformableRegistration8Test12DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test12DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test12DeformationField.mhd
        1 1
        ${TEMP}/DeformableRegistration8Test12FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)
set_property(TEST DeformableRegistration8Test12 PROPERTY DEPENDS BSplineWarping2Test)

# Due to the large memory requirements this tests must be run one by one
  set_tests_properties(
    DeformableRegistration8Test11
    DeformableRegistration8Test12
    PROPERTIES
    RUN_SERIAL 1
    )
endif()

endif()

endif(ITK_USE_BRAINWEB_DATA)


if(ITK_MRI_UNC_DATABASE_DATA_ROOT)

if( DART_TESTING_TIMEOUT GREATER 10000 )
itk_add_test(NAME DeformableRegistration15Test2 COMMAND ${ITK_TEST_DRIVER}
        $<TARGET_FILE:DeformableRegistration15>
        ${ITK_MRI_UNC_DATABASE_DATA_ROOT}/b/I.hdr
        ${ITK_MRI_UNC_DATABASE_DATA_ROOT}/d/I.hdr
        ${TEMP}/DeformableRegistration15Test2.mhd
        ${TEMP}/DeformableRegistration15Test2DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration15Test2DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration15Test2DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration15Test2FinalTransformParameters.txt
        5 100
        10.0 50
)
endif( DART_TESTING_TIMEOUT GREATER 10000 )

endif(ITK_MRI_UNC_DATABASE_DATA_ROOT)


itk_add_test(NAME LandmarkWarping2Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/LandmarkWarping2Test.png
            ${TEMP}/LandmarkWarping2Test.png
  $<TARGET_FILE:LandmarkWarping2>
        ${ITK_SOURCE_DIR}/Examples/Data/LandmarkWarping2Landmarks1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${TEMP}/LandmarkWarping2Test.png
)
itk_add_test(NAME ImageRegistration12Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ImageRegistration12TestPixelCentered.png
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
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cake_easy.png  12 12
)


itk_add_test(NAME IterativeClosestPoint1Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:IterativeClosestPoint1>
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointFixedPoints.txt
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointMovingPoints.txt
)

itk_add_test(NAME IterativeClosestPoint2Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:IterativeClosestPoint2>
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointFixedPoints2.txt
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointMovingPoints2.txt
)

itk_add_test(NAME IterativeClosestPoint3Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:IterativeClosestPoint3>
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointFixedPoints.txt
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointMovingPoints.txt
)


endif(BUILD_TESTING)

endif( NOT ITK_DISABLE_CXX_TESTING )

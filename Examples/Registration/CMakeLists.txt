project(ImageRegistration)
include_regular_expression("^.*$")

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Registration)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

set(REGISTRATION_EXAMPLES ${CXX_TEST_PATH}/RegistrationExamples)
set(REGISTRATION_EXAMPLESO ${CXX_TEST_PATH}/RegistrationExamplesO)
set(REGISTRATION_EXAMPLESO2 ${CXX_TEST_PATH}/RegistrationExamplesO2)
set(REGISTRATION_EXAMPLES2 ${CXX_TEST_PATH}/RegistrationExamples2)
set(REGISTRATION_EXAMPLES3 ${CXX_TEST_PATH}/RegistrationExamples3)
set(REGISTRATION_EXAMPLES4 ${CXX_TEST_PATH}/RegistrationExamples4)
set(REGISTRATION_EXAMPLES5 ${CXX_TEST_PATH}/RegistrationExamples5)
set(REGISTRATION_EXAMPLES6 ${CXX_TEST_PATH}/RegistrationExamples6)
set(REGISTRATION_EXAMPLES7 ${CXX_TEST_PATH}/RegistrationExamples7)
set(REGISTRATION_EXAMPLES8 ${CXX_TEST_PATH}/RegistrationExamples8)
set(REGISTRATION_EXAMPLES9 ${CXX_TEST_PATH}/RegistrationExamples9)
set(REGISTRATION_EXAMPLES10 ${CXX_TEST_PATH}/RegistrationExamples10)
set(REGISTRATION_EXAMPLES11 ${CXX_TEST_PATH}/RegistrationExamples11)
set(REGISTRATION_EXAMPLES12 ${CXX_TEST_PATH}/RegistrationExamples12)
set(REGISTRATION_EXAMPLES13 ${CXX_TEST_PATH}/RegistrationExamples13)
set(REGISTRATION_EXAMPLES14 ${CXX_TEST_PATH}/RegistrationExamples14)

add_executable(ImageRegistration3 ImageRegistration3.cxx )
target_link_libraries(ImageRegistration3 ITKIO ITKNumerics)

add_executable(ImageRegistration4o ImageRegistration4o.cxx )
target_link_libraries(ImageRegistration4o ITKIO ITKNumerics)

add_executable(ImageRegistration5 ImageRegistration5.cxx )
target_link_libraries(ImageRegistration5 ITKIO ITKNumerics)

add_executable(ImageRegistration6 ImageRegistration6.cxx )
target_link_libraries(ImageRegistration6 ITKIO ITKNumerics)

add_executable(ImageRegistration8 ImageRegistration8.cxx )
target_link_libraries(ImageRegistration8 ITKIO ITKNumerics)

add_executable(ImageRegistration9 ImageRegistration9.cxx )
target_link_libraries(ImageRegistration9 ITKIO ITKNumerics)

add_executable(ImageRegistration10 ImageRegistration10.cxx )
target_link_libraries(ImageRegistration10 ITKIO ITKNumerics)

add_executable(DeformableRegistration4 DeformableRegistration4.cxx )
target_link_libraries(DeformableRegistration4 ITKNumerics ITKIO)

add_executable(ImageRegistration4 ImageRegistration4.cxx )
target_link_libraries(ImageRegistration4 ITKIO ITKNumerics)

add_executable(ImageRegistration7 ImageRegistration7.cxx )
target_link_libraries(ImageRegistration7 ITKIO ITKNumerics)

add_executable(ImageRegistration11 ImageRegistration11.cxx )
target_link_libraries(ImageRegistration11 ITKStatistics ITKIO ITKNumerics)

add_executable(ImageRegistration12 ImageRegistration12.cxx )
target_link_libraries(ImageRegistration12 ITKStatistics ITKIO ITKNumerics)

add_executable(ImageRegistration13 ImageRegistration13.cxx )
target_link_libraries(ImageRegistration13 ITKStatistics ITKIO ITKNumerics)

add_executable(ImageRegistration14 ImageRegistration14.cxx )
target_link_libraries(ImageRegistration14 ITKStatistics ITKIO ITKNumerics)

add_executable(ImageRegistration15 ImageRegistration15.cxx )
target_link_libraries(ImageRegistration15 ITKStatistics ITKIO ITKNumerics)

add_executable(ImageRegistration16 ImageRegistration16.cxx )
target_link_libraries(ImageRegistration16 ITKStatistics ITKIO ITKNumerics)

add_executable(ImageRegistration18 ImageRegistration18.cxx )
target_link_libraries(ImageRegistration18 ITKStatistics ITKIO ITKNumerics)

add_executable(ImageRegistration19 ImageRegistration19.cxx )
target_link_libraries(ImageRegistration19 ITKStatistics ITKIO ITKNumerics)

add_executable(ImageRegistration20 ImageRegistration20.cxx )
target_link_libraries(ImageRegistration20 ITKStatistics ITKIO ITKNumerics)

add_executable(DeformableRegistration6 DeformableRegistration6.cxx )
target_link_libraries(DeformableRegistration6 ITKIO ITKNumerics)

add_executable(DeformableRegistration7 DeformableRegistration7.cxx )
target_link_libraries(DeformableRegistration7 ITKIO ITKNumerics)

add_executable(DeformableRegistration8 DeformableRegistration8.cxx )
target_link_libraries(DeformableRegistration8 ITKIO ITKNumerics)

if( USE_FFTWD )
  add_executable(DeformableRegistration9 DeformableRegistration9.cxx )
  target_link_libraries(DeformableRegistration9
      ITKIO ITKAlgorithms ITKNumerics)

  add_executable(DeformableRegistration10 DeformableRegistration10.cxx )
  target_link_libraries(DeformableRegistration10
                        ITKIO ITKAlgorithms ITKNumerics)
endif( USE_FFTWD )

add_executable(MultiResImageRegistration1 MultiResImageRegistration1.cxx )
target_link_libraries(MultiResImageRegistration1 ITKIO ITKNumerics)

add_executable(MultiResImageRegistration2 MultiResImageRegistration2.cxx )
target_link_libraries(MultiResImageRegistration2 ITKIO ITKNumerics)

add_executable(MultiResImageRegistration3 MultiResImageRegistration3.cxx )
target_link_libraries(MultiResImageRegistration3 ITKIO ITKNumerics)

add_executable(RegistrationExamples7 RegistrationExamples7.cxx )
target_link_libraries(RegistrationExamples7 ITKFEM ITKIO ITKBasicFilters ITKNumerics)

add_executable(DeformableRegistration1 DeformableRegistration1.cxx )
target_link_libraries(DeformableRegistration1 ITKFEM ITKIO
                                              ITKStatistics ITKNumerics)

add_executable(DeformableRegistration2 DeformableRegistration2.cxx )
target_link_libraries(DeformableRegistration2 ITKIO ITKStatistics ITKNumerics)

add_executable(DeformableRegistration3 DeformableRegistration3.cxx )
target_link_libraries(DeformableRegistration3 ITKIO ITKStatistics ITKNumerics)

add_executable(DeformableRegistration5 DeformableRegistration5.cxx )
target_link_libraries(DeformableRegistration5 ITKIO ITKStatistics ITKNumerics)

add_executable(DeformableRegistration11 DeformableRegistration11.cxx )
target_link_libraries(DeformableRegistration11 ITKFEM ITKIO
                                              ITKStatistics ITKNumerics)

add_executable(DeformableRegistration12 DeformableRegistration12.cxx )
target_link_libraries(DeformableRegistration12 ITKIO ITKStatistics ITKNumerics)

add_executable(DeformableRegistration13 DeformableRegistration13.cxx )
target_link_libraries(DeformableRegistration13 ITKIO ITKStatistics ITKNumerics)

add_executable(DeformableRegistration14 DeformableRegistration14.cxx )
target_link_libraries(DeformableRegistration14 ITKIO ITKStatistics ITKNumerics)

add_executable(ChangeInformationImageFilter ChangeInformationImageFilter.cxx )
target_link_libraries(ChangeInformationImageFilter ITKIO )

add_executable(DeformableRegistration15 DeformableRegistration15.cxx )
target_link_libraries(DeformableRegistration15 ITKIO ITKStatistics ITKNumerics)

add_executable(DeformableRegistration16 DeformableRegistration16.cxx )
target_link_libraries(DeformableRegistration16 ITKIO ITKStatistics ITKNumerics)

add_executable(DeformableRegistration17 DeformableRegistration17.cxx )
target_link_libraries(DeformableRegistration17 ITKIO ITKStatistics ITKNumerics)

add_executable(DeformationFieldInitialization DeformationFieldInitialization.cxx )
target_link_libraries(DeformationFieldInitialization ITKIO)

add_executable(LandmarkWarping2 LandmarkWarping2.cxx )
target_link_libraries(LandmarkWarping2 ITKIO)

add_executable(BSplineWarping1 BSplineWarping1.cxx )
target_link_libraries(BSplineWarping1 ITKIO)

add_executable(BSplineWarping2 BSplineWarping2.cxx )
target_link_libraries(BSplineWarping2 ITKIO)

add_executable(ThinPlateSplineWarp ThinPlateSplineWarp.cxx )
target_link_libraries(ThinPlateSplineWarp ITKIO)

add_executable(ModelToImageRegistration1 ModelToImageRegistration1.cxx )
target_link_libraries(ModelToImageRegistration1 ITKStatistics ITKIO ITKNumerics)

add_executable(ModelToImageRegistration2 ModelToImageRegistration2.cxx )
target_link_libraries(ModelToImageRegistration2 ITKStatistics ITKIO ITKNumerics)


add_executable(ImageRegistration1 ImageRegistration1.cxx )
target_link_libraries(ImageRegistration1 ITKIO ITKNumerics)

add_executable(ImageRegistration2 ImageRegistration2.cxx )
target_link_libraries(ImageRegistration2 ITKIO ITKNumerics)

add_executable(ImageRegistration17 ImageRegistration17.cxx )
target_link_libraries(ImageRegistration17 ITKStatistics ITKIO ITKNumerics)

#TODO needs updating to new statistics  add_executable(ImageRegistrationHistogramPlotter ImageRegistrationHistogramPlotter.cxx )
#TODO needs updating to new statistics  target_link_libraries(ImageRegistrationHistogramPlotter ITKIO ITKStatistics ITKNumerics)

add_executable(MeanSquaresImageMetric1  MeanSquaresImageMetric1.cxx )
target_link_libraries(MeanSquaresImageMetric1 ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics)

add_executable(DeformationFieldJacobian DeformationFieldJacobian.cxx )
target_link_libraries(DeformationFieldJacobian ITKIO)

add_executable(IterativeClosestPoint1   IterativeClosestPoint1.cxx )
target_link_libraries(IterativeClosestPoint1 ITKAlgorithms ITKStatistics ITKIO ITKBasicFilters ITKNumerics)

add_executable(IterativeClosestPoint2   IterativeClosestPoint2.cxx )
target_link_libraries(IterativeClosestPoint2 ITKAlgorithms ITKStatistics ITKIO ITKBasicFilters ITKNumerics)

add_executable(IterativeClosestPoint3   IterativeClosestPoint3.cxx )
target_link_libraries(IterativeClosestPoint3 ITKAlgorithms ITKStatistics ITKIO ITKBasicFilters ITKNumerics)


if( NOT ITK_DISABLE_CXX_TESTING )

if(BUILD_TESTING)

add_test(ImageRegistration1oTest ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration1TestPixelCentered.png
            ${TEMP}/ImageRegistration1TestPixelCentered.png
  ImageRegistration1oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1TestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceAfterTestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceBeforeTestPixelCentered.png
)

add_test(ImageRegistration1od1Test ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration1TestPixelCentered.png
            ${TEMP}/ImageRegistration1od1TestPixelCentered.png
  ImageRegistration1oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20DirectionPlus30.mhd
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd
        ${TEMP}/ImageRegistration1od1TestPixelCentered.png
        ${TEMP}/ImageRegistration1od1DifferenceAfterTestPixelCentered.png
        ${TEMP}/ImageRegistration1od1DifferenceBeforeTestPixelCentered.png
)

add_test(ImageRegistration2oTest ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration2Test.png
            ${TEMP}/ImageRegistration2oTest.png
  ImageRegistration2oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration2oTest.png
)

add_test(ImageRegistration2od1Test ${REGISTRATION_EXAMPLESO}
  ImageRegistration2oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20DirectionPlus30.mhd
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd
        ${TEMP}/ImageRegistration2od1Test.mhd
)

add_test(ImageRegistration3oTest ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration3TestPixelCentered.png
            ${TEMP}/ImageRegistration3TestPixelCentered.png
  ImageRegistration3oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration3TestPixelCentered.png
)

add_test(ImageRegistration4oTest ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration4Test.png
            ${TEMP}/ImageRegistration4Test.png
  ImageRegistration4oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration4Test.png 0
        ${TEMP}/ImageRegistration4BeforeTest.png
        ${TEMP}/ImageRegistration4AfterTest.png
)

add_test(ImageRegistration5oTest1 ${REGISTRATION_EXAMPLESO2}
  --compare ${BASELINE}/ImageRegistration5oTest1PixelCentered.png
            ${TEMP}/ImageRegistration5oTest1PixelCentered.png
  ImageRegistration5oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceRotated10.png
        ${TEMP}/ImageRegistration5oTest1PixelCentered.png
)

add_test(ImageRegistration5oTest2 ${REGISTRATION_EXAMPLESO2}
  --compare ${BASELINE}/ImageRegistration5Test2PixelCentered.png
            ${TEMP}/ImageRegistration5Test2PixelCentered.png
  ImageRegistration5oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration5Test2PixelCentered.png
)

add_test(ImageRegistration6oTest ${REGISTRATION_EXAMPLESO2}
  --compare ${BASELINE}/ImageRegistration6TestPixelCentered.png
            ${TEMP}/ImageRegistration6oTestPixelCentered.png
  ImageRegistration6oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration6oTestPixelCentered.png
)

add_test(ImageRegistration1Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration1TestPixelCentered.png
            ${TEMP}/ImageRegistration1TestPixelCentered.png
  ImageRegistration1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1TestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceAfterTestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceBeforeTestPixelCentered.png
)

add_test(ImageRegistration2Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration2Test.png
            ${TEMP}/ImageRegistration2Test.png
  ImageRegistration2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration2Test.png
        ${TEMP}/ImageRegistration2BeforeTest.png
        ${TEMP}/ImageRegistration2AfterTest.png
)

add_test(ImageRegistration3Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration3TestPixelCentered.png
            ${TEMP}/ImageRegistration3TestPixelCentered.png
  ImageRegistration3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration3TestPixelCentered.png
)

add_test(ImageRegistration4Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration4Test.png
            ${TEMP}/ImageRegistration4Test.png
  ImageRegistration4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration4Test.png 0
        ${TEMP}/ImageRegistration4BeforeTest.png
        ${TEMP}/ImageRegistration4AfterTest.png
        24 10000 1
)

add_test(ImageRegistration4Test2 ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration4Test.png
            ${TEMP}/ImageRegistration4Test2.png
  ImageRegistration4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration4Test2.png 0
        ${TEMP}/ImageRegistration4BeforeTest.png
        ${TEMP}/ImageRegistration4AfterTest.png
        24 10000 0
)

add_test(ImageRegistration4Test3 ${REGISTRATION_EXAMPLES}
  ImageRegistration4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration4Test3.png 0
        ${TEMP}/ImageRegistration4BeforeTest3.png
        ${TEMP}/ImageRegistration4AfterTest3.png
        5 10000 0
)

add_test(ImageRegistration5Test1 ${REGISTRATION_EXAMPLES9}
  --compare ${BASELINE}/ImageRegistration5Test1PixelCentered.png
            ${TEMP}/ImageRegistration5Test1PixelCentered.png
  ImageRegistration5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceRotated10.png
        ${TEMP}/ImageRegistration5Test1PixelCentered.png
)

add_test(ImageRegistration5Test2 ${REGISTRATION_EXAMPLES9}
  --compare ${BASELINE}/ImageRegistration5Test2PixelCentered.png
            ${TEMP}/ImageRegistration5Test2PixelCentered.png
  ImageRegistration5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration5Test2PixelCentered.png
)

add_test(ImageRegistration6Test ${REGISTRATION_EXAMPLES9}
  --compare ${BASELINE}/ImageRegistration6TestPixelCentered.png
            ${TEMP}/ImageRegistration6TestPixelCentered.png
  ImageRegistration6Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration6TestPixelCentered.png
)

add_test(ImageRegistration7Test ${REGISTRATION_EXAMPLES9}
  --compare ${BASELINE}/ImageRegistration7TestPixelCentered.png
            ${TEMP}/ImageRegistration7TestPixelCentered.png
  ImageRegistration7Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17S12.png
        ${TEMP}/ImageRegistration7TestPixelCentered.png
        ${TEMP}/ImageRegistration7BeforeTestPixelCentered.png
        ${TEMP}/ImageRegistration7AfterTestPixelCentered.png
        1.0   1.0  0.0
)

if(ITK_BRAINWEB_DATA_ROOT)
add_test(ImageRegistration8Test ${REGISTRATION_EXAMPLES11}
  --compare ${BASELINE}/ImageRegistration8RegisteredSlice.png
            ${TEMP}/ImageRegistration8RegisteredSlice.png
  ImageRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20Rot10Tx15.mha
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
  add_test(ImageRegistration20Test
    ${CXX_TEST_PATH}/ImageRegistration20
      ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
      ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20Rot10Tx15.mha
      ${TEMP}/ImageRegistration20Output.mhd
      ${TEMP}/ImageRegistration20DifferenceBefore.mhd
      ${TEMP}/ImageRegistration20DifferenceAfter.mhd
      0.1  # Step length
      300  # maximum number of iterations
    )
  set_tests_properties(ImageRegistration20Test PROPERTIES RUN_SERIAL 1)
endif( ${CMAKE_BUILD_TYPE} MATCHES "Release")

endif(ITK_BRAINWEB_DATA_ROOT)

add_test(ImageRegistration9Test ${REGISTRATION_EXAMPLES11}
  --compare ${BASELINE}/ImageRegistration9TestPixelCentered.png
            ${TEMP}/ImageRegistration9TestPixelCentered.png
  ImageRegistration9Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration9TestPixelCentered.png
)

add_test(ImageRegistration10Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration10TestPixelCentered.png
            ${TEMP}/ImageRegistration10TestPixelCentered.png
  ImageRegistration10Test
        ${ITK_SOURCE_DIR}/Testing/Data/Baseline/Segmentation/WatershedSegmentation1Test1.png
        ${ITK_SOURCE_DIR}/Testing/Data/Baseline/Segmentation/WatershedSegmentation1Test2.png
        ${TEMP}/ImageRegistration10TestPixelCentered.png
        ${TEMP}/ImageRegistration10DifferenceTestPixelCentered.png
        -11 -13
)

add_test(ImageRegistration13Test ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13TestPixelCentered.png
  --compareNumberOfPixelsTolerance 5
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13TestPixelCentered.png
)

add_test(ImageRegistration13Test2 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test2PixelCentered.png
  --compareNumberOfPixelsTolerance 10
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test2PixelCentered.png
        0 0
)

add_test(ImageRegistration13Test3 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test3PixelCentered.png
  --compareNumberOfPixelsTolerance 10
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test3PixelCentered.png
        0 1
)

add_test(ImageRegistration13Test4 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test4PixelCentered.png
  --compareNumberOfPixelsTolerance 5
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test4PixelCentered.png
        1 0
)

add_test(ImageRegistration13Test5 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test5PixelCentered.png
  --compareNumberOfPixelsTolerance 5
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test5PixelCentered.png
        1 1
)

add_test(ImageRegistration14Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration14Test.png
            ${TEMP}/ImageRegistration14Test.png
  --compareNumberOfPixelsTolerance 2000
  ImageRegistration14Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration14Test.png 32 0.01 0.1  0.15  10.0  14.0
)

add_test(ImageRegistration15Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration15TestPixelCentered.png
            ${TEMP}/ImageRegistration15TestPixelCentered.png
  ImageRegistration15Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration15TestPixelCentered.png 32 0.1 0.05  0.0 0.0
)

add_test(ImageRegistration16Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration16TestPixelCentered.png
            ${TEMP}/ImageRegistration16TestPixelCentered.png
  ImageRegistration16Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration16TestPixelCentered.png 0.0 0.0
)

add_test(ImageRegistration16Test2 ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration16TestPixelCentered.png
            ${TEMP}/ImageRegistration16Test2PixelCentered.png
  ImageRegistration16Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration16Test2PixelCentered.png 0.0 0.0
        0
)

add_test(ImageRegistration17Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration17TestPixelCentered.png
            ${TEMP}/ImageRegistration17TestPixelCentered.png
  ImageRegistration17Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration17TestPixelCentered.png 0.0 0.0
)

add_test(ImageRegistration18Test
  ${CXX_TEST_PATH}/ImageRegistration18
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration18Test.png 0.0 0.0
)

add_test(ImageRegistration19Test
  ${CXX_TEST_PATH}/ImageRegistration19
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration19Test.png
        ${TEMP}/ImageRegistration19DifferenceTest.png
        2.0 2.0 # Initial translation parameters
)

add_test(MultiResImageRegistration1Test ${REGISTRATION_EXAMPLES2}
  --compare ${BASELINE}/MultiResImageRegistration1Test.png
            ${TEMP}/MultiResImageRegistration1Test.png
  --compareNumberOfPixelsTolerance 100
  MultiResImageRegistration1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration1Test.png 100
        ${TEMP}/MultiResImageRegistration1CheckerBoardBeforeTest.png
        ${TEMP}/MultiResImageRegistration1CheckerBoardAfterTest.png
)

add_test(MultiResImageRegistration1Test2 ${REGISTRATION_EXAMPLES2}
  --compare ${BASELINE}/MultiResImageRegistration1Test.png
            ${TEMP}/MultiResImageRegistration1Test2.png
  --compareNumberOfPixelsTolerance 100
  MultiResImageRegistration1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration1Test2.png 100
        ${TEMP}/MultiResImageRegistration1CheckerBoardBeforeTest2.png
        ${TEMP}/MultiResImageRegistration1CheckerBoardAfterTest2.png
        0
)


add_test(MultiResImageRegistration2Test ${REGISTRATION_EXAMPLES2}
  --compare ${BASELINE}/MultiResImageRegistration2Test.png
            ${TEMP}/MultiResImageRegistration2Test.png
  --compareNumberOfPixelsTolerance 10000
  MultiResImageRegistration2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration2Test.png  0
        ${TEMP}/MultiResImageRegistration2CheckerBoardBeforeTest.png
        ${TEMP}/MultiResImageRegistration2CheckerBoardAfterTest.png
)


add_test(MultiResImageRegistration2Test2 ${REGISTRATION_EXAMPLES2}
  --compare ${BASELINE}/MultiResImageRegistration2Test.png
            ${TEMP}/MultiResImageRegistration2Test2.png
  --compareNumberOfPixelsTolerance 10000
  MultiResImageRegistration2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration2Test2.png 0
        ${TEMP}/MultiResImageRegistration2CheckerBoardBeforeTest2.png
        ${TEMP}/MultiResImageRegistration2CheckerBoardAfterTest2.png
        0
)


add_test(DeformableRegistration2Test ${REGISTRATION_EXAMPLES5}
  --compare ${BASELINE}/DeformableRegistration2Test.png
            ${TEMP}/DeformableRegistration2Test.png
  DeformableRegistration2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration2Test.png
)

add_test(DeformableRegistration3Test ${REGISTRATION_EXAMPLES5}
  --compare ${BASELINE}/DeformableRegistration3Test.png
            ${TEMP}/DeformableRegistration3Test.png
  --compareNumberOfPixelsTolerance 100
  DeformableRegistration3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration3Test.png
)

add_test(DeformableRegistration4Test ${REGISTRATION_EXAMPLES11}
  --compare ${BASELINE}/DeformableRegistration4TestPixelCentered.png
            ${TEMP}/DeformableRegistration4TestPixelCentered.png
  DeformableRegistration4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration4TestPixelCentered.png
)

add_test(DeformableRegistration5Test ${REGISTRATION_EXAMPLES5}
  --compare ${BASELINE}/DeformableRegistration5Test.png
            ${TEMP}/DeformableRegistration5Test.png
  --compareNumberOfPixelsTolerance 5
  DeformableRegistration5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration5Test.png
)

add_test(DeformableRegistration6Test ${REGISTRATION_EXAMPLES12}
  --compare ${BASELINE}/DeformableRegistration6TestPixelCentered.png
            ${TEMP}/DeformableRegistration6TestPixelCentered.png
  DeformableRegistration6Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration6TestPixelCentered.png
)

add_test(BSplineWarping1Test ${REGISTRATION_EXAMPLES6}
  --compare ${BASELINE}/BSplineWarping1Test.png
            ${TEMP}/BSplineWarping1Test.png
  BSplineWarping1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${TEMP}/BSplineWarping1Test.png
        ${TEMP}/BSplineWarping1TestDeformationField.mhd
        ${TEMP}/BSplineWarping1TestTransform.txt
)

add_test(DeformableRegistration12Test1 ${REGISTRATION_EXAMPLES13}
  --compare ${BASELINE}/DeformableRegistration12Test.png
            ${TEMP}/DeformableRegistration12Test1.png
  --compareNumberOfPixelsTolerance 400
  DeformableRegistration12Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration12Test1.png
        ${TEMP}/DeformableRegistration12Test1DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test1DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test1DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration12Test1FinalTransformParameters.txt
)

add_test(DeformableRegistration12Test2 ${REGISTRATION_EXAMPLES13}
  --compare ${BASELINE}/DeformableRegistration12Test.png
            ${TEMP}/DeformableRegistration12Test2.png
  --compareNumberOfPixelsTolerance 400
  DeformableRegistration12Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration12Test2.png
        ${TEMP}/DeformableRegistration12Test2DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test2DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test2DeformationField.mhd
        0 1
        ${TEMP}/DeformableRegistration12Test2FinalTransformParameters.txt
)

add_test(DeformableRegistration12Test3 ${REGISTRATION_EXAMPLES13}
  --compare ${BASELINE}/DeformableRegistration12Test.png
            ${TEMP}/DeformableRegistration12Test3.png
  --compareNumberOfPixelsTolerance 400
  DeformableRegistration12Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration12Test3.png
        ${TEMP}/DeformableRegistration12Test3DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test3DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test3DeformationField.mhd
        1 0
        ${TEMP}/DeformableRegistration12Test3FinalTransformParameters.txt
)

add_test(DeformableRegistration12Test4 ${REGISTRATION_EXAMPLES13}
  --compare ${BASELINE}/DeformableRegistration12Test.png
            ${TEMP}/DeformableRegistration12Test4.png
  --compareNumberOfPixelsTolerance 400
  DeformableRegistration12Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration12Test4.png
        ${TEMP}/DeformableRegistration12Test4DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test4DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test4DeformationField.mhd
        1 1
        ${TEMP}/DeformableRegistration12Test4FinalTransformParameters.txt
)

add_test(DeformableRegistration13Test1 ${REGISTRATION_EXAMPLES13}
  --compare ${BASELINE}/DeformableRegistration13Test.png
            ${TEMP}/DeformableRegistration13Test1.png
  --compareNumberOfPixelsTolerance 400
  DeformableRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration13Test1.png
        ${TEMP}/DeformableRegistration13Test1DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test1DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test1DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration13Test1FinalTransformParameters.txt
)

add_test(DeformableRegistration13Test2 ${REGISTRATION_EXAMPLES13}
  --compare ${BASELINE}/DeformableRegistration13Test.png
            ${TEMP}/DeformableRegistration13Test2.png
  --compareNumberOfPixelsTolerance 400
  DeformableRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration13Test2.png
        ${TEMP}/DeformableRegistration13Test2DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test2DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test2DeformationField.mhd
        0 1
        ${TEMP}/DeformableRegistration13Test2FinalTransformParameters.txt
)

add_test(DeformableRegistration13Test3 ${REGISTRATION_EXAMPLES13}
  --compare ${BASELINE}/DeformableRegistration13Test.png
            ${TEMP}/DeformableRegistration13Test3.png
  --compareNumberOfPixelsTolerance 400
  DeformableRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration13Test3.png
        ${TEMP}/DeformableRegistration13Test3DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test3DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test3DeformationField.mhd
        1 0
        ${TEMP}/DeformableRegistration13Test3FinalTransformParameters.txt
)

add_test(DeformableRegistration13Test4 ${REGISTRATION_EXAMPLES13}
  --compare ${BASELINE}/DeformableRegistration13Test.png
            ${TEMP}/DeformableRegistration13Test4.png
  --compareNumberOfPixelsTolerance 400
  DeformableRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${BASELINE}/BSplineWarping1Test.png
        ${TEMP}/DeformableRegistration13Test4.png
        ${TEMP}/DeformableRegistration13Test4DifferenceAfter.png
        ${TEMP}/DeformableRegistration13Test4DifferenceBefore.png
        ${TEMP}/DeformableRegistration13Test4DeformationField.mhd
        1 1
        ${TEMP}/DeformableRegistration13Test4FinalTransformParameters.txt
)

if(ITK_BRAINWEB_DATA_ROOT)

add_test(BSplineWarping2Test ${REGISTRATION_EXAMPLES6}
  BSplineWarping2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements2.txt
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/BSplineWarping2TestDeformationField.mhd
        ${TEMP}/BSplineWarping2TestTransform.txt
)

set(NUMBER_OF_BSPLINE_GRID_NODES 5)

if( DART_TESTING_TIMEOUT GREATER 3000 )

add_test(DeformableRegistration8Test1 ${REGISTRATION_EXAMPLES12}
    --compare ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
              ${TEMP}/DeformableRegistration8Test1.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
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

add_test(DeformableRegistration8Test2 ${REGISTRATION_EXAMPLES12}
    --compare ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
              ${TEMP}/DeformableRegistration8Test2.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
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

add_test(DeformableRegistration8Test3 ${REGISTRATION_EXAMPLES12}
    --compare ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
              ${TEMP}/DeformableRegistration8Test3.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
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

add_test(DeformableRegistration8Test4 ${REGISTRATION_EXAMPLES12}
    --compare ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
              ${TEMP}/DeformableRegistration8Test4.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
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

add_test(DeformableRegistration14Test1 ${REGISTRATION_EXAMPLES12}
    --compare ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
              ${TEMP}/DeformableRegistration14Test1.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  DeformableRegistration14Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
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

add_test(DeformableRegistration14Test2 ${REGISTRATION_EXAMPLES12}
    --compare ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
              ${TEMP}/DeformableRegistration14Test2.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  DeformableRegistration14Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
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

add_test(DeformableRegistration14Test3 ${REGISTRATION_EXAMPLES12}
    --compare ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
              ${TEMP}/DeformableRegistration14Test3.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  DeformableRegistration14Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
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

add_test(DeformableRegistration14Test4 ${REGISTRATION_EXAMPLES12}
    --compare ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
              ${TEMP}/DeformableRegistration14Test4.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
  DeformableRegistration14Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
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
endif( DART_TESTING_TIMEOUT GREATER 3000 )

add_test(ChangeInformationTest1
        ${CXX_TEST_PATH}/ChangeInformationImageFilter
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplinedTranlatedScaledRotated.mhd
        1.05
        1.0 2.0 0.0
        10.0
)

if( DART_TESTING_TIMEOUT GREATER 10000 )
add_test(DeformableRegistration15Test1 ${REGISTRATION_EXAMPLES14}
    --compare ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
              ${TEMP}/DeformableRegistration15Test1.mhd
    --compareNumberOfPixelsTolerance 700000
    --compareIntensityTolerance 15
        DeformableRegistration15Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
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
endif( DART_TESTING_TIMEOUT GREATER 10000 )

if( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 6 )

set(NUMBER_OF_BSPLINE_GRID_NODES 32)
add_test(DeformableRegistration8Test5 ${REGISTRATION_EXAMPLES12}
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test5.mhd
        ${TEMP}/DeformableRegistration8Test5DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test5DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test5DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration8Test5FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)

add_test(DeformableRegistration8Test6 ${REGISTRATION_EXAMPLES12}
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test6.mhd
        ${TEMP}/DeformableRegistration8Test6DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test6DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test6DeformationField.mhd
        0 1
        ${TEMP}/DeformableRegistration8Test6FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)

add_test(DeformableRegistration8Test7 ${REGISTRATION_EXAMPLES12}
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test7.mhd
        ${TEMP}/DeformableRegistration8Test7DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test7DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test7DeformationField.mhd
        1 0
        ${TEMP}/DeformableRegistration8Test7FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)

add_test(DeformableRegistration8Test8 ${REGISTRATION_EXAMPLES12}
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test8.mhd
        ${TEMP}/DeformableRegistration8Test8DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test8DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test8DeformationField.mhd
        1 1
        ${TEMP}/DeformableRegistration8Test8FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)

set(NUMBER_OF_BSPLINE_GRID_NODES 100)
add_test(DeformableRegistration8Test9 ${REGISTRATION_EXAMPLES12}
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test9.mhd
        ${TEMP}/DeformableRegistration8Test9DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test9DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test9DeformationField.mhd
        0 0
        ${TEMP}/DeformableRegistration8Test9FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)

add_test(DeformableRegistration8Test10 ${REGISTRATION_EXAMPLES12}
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test10.mhd
        ${TEMP}/DeformableRegistration8Test10DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test10DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test10DeformationField.mhd
        0 1
        ${TEMP}/DeformableRegistration8Test10FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)

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

if( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 16 )

add_test(DeformableRegistration8Test11 ${REGISTRATION_EXAMPLES12}
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test11.mhd
        ${TEMP}/DeformableRegistration8Test11DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test11DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test11DeformationField.mhd
        1 0
        ${TEMP}/DeformableRegistration8Test11FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)

add_test(DeformableRegistration8Test12 ${REGISTRATION_EXAMPLES12}
  DeformableRegistration8Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/DeformableRegistration8Test12.mhd
        ${TEMP}/DeformableRegistration8Test12DifferenceAfter.mhd
        ${TEMP}/DeformableRegistration8Test12DifferenceBefore.mhd
        ${TEMP}/DeformableRegistration8Test12DeformationField.mhd
        1 1
        ${TEMP}/DeformableRegistration8Test12FinalTransformParameters.txt
        ${NUMBER_OF_BSPLINE_GRID_NODES}
)

# Due to the large memory requirements this tests must be run one by one
  set_tests_properties(
    DeformableRegistration8Test11
    DeformableRegistration8Test12
    PROPERTIES
    RUN_SERIAL 1
    )
endif( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 16 )

endif( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 6 )

endif(ITK_BRAINWEB_DATA_ROOT)


if(ITK_MRI_UNC_DATABASE_DATA_ROOT)

if( DART_TESTING_TIMEOUT GREATER 10000 )
add_test(DeformableRegistration15Test2 ${REGISTRATION_EXAMPLES14}
        DeformableRegistration15Test
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


add_test(LandmarkWarping2Test ${REGISTRATION_EXAMPLES6}
  --compare ${BASELINE}/LandmarkWarping2Test.png
            ${TEMP}/LandmarkWarping2Test.png
  LandmarkWarping2Test
        ${ITK_SOURCE_DIR}/Examples/Data/LandmarkWarping2Landmarks1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${TEMP}/LandmarkWarping2Test.png
)
add_test(ImageRegistration12Test ${REGISTRATION_EXAMPLES4}
  --compare ${BASELINE}/ImageRegistration12TestPixelCentered.png
            ${TEMP}/ImageRegistration12TestPixelCentered.png
  --compareNumberOfPixelsTolerance 2
  ImageRegistration12Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration12TestPixelCentered.png
)

add_test(ModelToImageRegistration2Test ${REGISTRATION_EXAMPLES10}
  ModelToImageRegistration2Test
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cake_easy.png  12 12
)


add_test(IterativeClosestPoint1Test ${REGISTRATION_EXAMPLES14}
  IterativeClosestPoint1Test
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointFixedPoints.txt
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointMovingPoints.txt
)

add_test(IterativeClosestPoint2Test ${REGISTRATION_EXAMPLES14}
  IterativeClosestPoint2Test
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointFixedPoints2.txt
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointMovingPoints2.txt
)

add_test(IterativeClosestPoint3Test ${REGISTRATION_EXAMPLES14}
  IterativeClosestPoint3Test
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointFixedPoints.txt
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointMovingPoints.txt
)


add_executable(RegistrationExamples RegistrationExamples.cxx )
target_link_libraries(RegistrationExamples ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamplesO RegistrationExamplesO.cxx )
target_link_libraries(RegistrationExamplesO ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamplesO2 RegistrationExamplesO2.cxx )
target_link_libraries(RegistrationExamplesO2 ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples2 RegistrationExamples2.cxx )
target_link_libraries(RegistrationExamples2 ITKAlgorithms ITKFEM ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples3 RegistrationExamples3.cxx )
target_link_libraries(RegistrationExamples3 ITKFEM ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples4 RegistrationExamples4.cxx )
target_link_libraries(RegistrationExamples4 ITKFEM ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples5 RegistrationExamples5.cxx )
target_link_libraries(RegistrationExamples5 ITKAlgorithms ITKFEM ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples6 RegistrationExamples6.cxx )
target_link_libraries(RegistrationExamples6 ITKFEM ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples8 RegistrationExamples8.cxx )
target_link_libraries(RegistrationExamples8 ITKAlgorithms ITKFEM ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples9 RegistrationExamples9.cxx )
target_link_libraries(RegistrationExamples9 ITKFEM ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples10 RegistrationExamples10.cxx )
target_link_libraries(RegistrationExamples10 ITKFEM ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples11 RegistrationExamples11.cxx )
target_link_libraries(RegistrationExamples11 ITKFEM ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples12 RegistrationExamples12.cxx )
target_link_libraries(RegistrationExamples12 ITKIO ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples13 RegistrationExamples13.cxx )
target_link_libraries(RegistrationExamples13 ITKIO ITKBasicFilters ITKNumerics)

add_executable(RegistrationExamples14 RegistrationExamples14.cxx )
target_link_libraries(RegistrationExamples14 ITKIO ITKBasicFilters ITKNumerics)

endif(BUILD_TESTING)

endif( NOT ITK_DISABLE_CXX_TESTING )

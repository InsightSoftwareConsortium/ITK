PROJECT(ImageRegistration)
INCLUDE_REGULAR_EXPRESSION("^.*$")

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Registration)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

SET(REGISTRATION_EXAMPLES ${CXX_TEST_PATH}/RegistrationExamples)
SET(REGISTRATION_EXAMPLESO ${CXX_TEST_PATH}/RegistrationExamplesO)
SET(REGISTRATION_EXAMPLESO2 ${CXX_TEST_PATH}/RegistrationExamplesO2)
SET(REGISTRATION_EXAMPLES2 ${CXX_TEST_PATH}/RegistrationExamples2)
SET(REGISTRATION_EXAMPLES3 ${CXX_TEST_PATH}/RegistrationExamples3)
SET(REGISTRATION_EXAMPLES4 ${CXX_TEST_PATH}/RegistrationExamples4)
SET(REGISTRATION_EXAMPLES5 ${CXX_TEST_PATH}/RegistrationExamples5)
SET(REGISTRATION_EXAMPLES6 ${CXX_TEST_PATH}/RegistrationExamples6)
SET(REGISTRATION_EXAMPLES7 ${CXX_TEST_PATH}/RegistrationExamples7)
SET(REGISTRATION_EXAMPLES8 ${CXX_TEST_PATH}/RegistrationExamples8)
SET(REGISTRATION_EXAMPLES9 ${CXX_TEST_PATH}/RegistrationExamples9)
SET(REGISTRATION_EXAMPLES10 ${CXX_TEST_PATH}/RegistrationExamples10)
SET(REGISTRATION_EXAMPLES11 ${CXX_TEST_PATH}/RegistrationExamples11)
SET(REGISTRATION_EXAMPLES12 ${CXX_TEST_PATH}/RegistrationExamples12)
SET(REGISTRATION_EXAMPLES13 ${CXX_TEST_PATH}/RegistrationExamples13)
SET(REGISTRATION_EXAMPLES14 ${CXX_TEST_PATH}/RegistrationExamples14)

ADD_EXECUTABLE(ImageRegistration3 ImageRegistration3.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration3 ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration4o ImageRegistration4o.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration4o ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration5 ImageRegistration5.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration5 ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration6 ImageRegistration6.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration6 ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration8 ImageRegistration8.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration8 ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration9 ImageRegistration9.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration9 ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration10 ImageRegistration10.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration10 ITKIO ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration4 DeformableRegistration4.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration4 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration4 ImageRegistration4.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration4 ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration7 ImageRegistration7.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration7 ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration11 ImageRegistration11.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration11 ITKStatistics ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration12 ImageRegistration12.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration12 ITKStatistics ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration13 ImageRegistration13.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration13 ITKStatistics ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration14 ImageRegistration14.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration14 ITKStatistics ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration15 ImageRegistration15.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration15 ITKStatistics ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration16 ImageRegistration16.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration16 ITKStatistics ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration18 ImageRegistration18.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration18 ITKStatistics ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration19 ImageRegistration19.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration19 ITKStatistics ITKIO ITKNumerics)

ADD_EXECUTABLE(ImageRegistration20 ImageRegistration20.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration20 ITKStatistics ITKIO ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration6 DeformableRegistration6.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration6 ITKIO ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration7 DeformableRegistration7.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration7 ITKIO ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration8 DeformableRegistration8.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration8 ITKIO ITKNumerics)

IF( USE_FFTWD )
  ADD_EXECUTABLE(DeformableRegistration9 DeformableRegistration9.cxx )
  TARGET_LINK_LIBRARIES(DeformableRegistration9
      ITKIO ITKAlgorithms ITKNumerics)

  ADD_EXECUTABLE(DeformableRegistration10 DeformableRegistration10.cxx )
  TARGET_LINK_LIBRARIES(DeformableRegistration10
                        ITKIO ITKAlgorithms ITKNumerics)
ENDIF( USE_FFTWD )

ADD_EXECUTABLE(MultiResImageRegistration1 MultiResImageRegistration1.cxx )
TARGET_LINK_LIBRARIES(MultiResImageRegistration1 ITKIO ITKNumerics)

ADD_EXECUTABLE(MultiResImageRegistration2 MultiResImageRegistration2.cxx )
TARGET_LINK_LIBRARIES(MultiResImageRegistration2 ITKIO ITKNumerics)

ADD_EXECUTABLE(MultiResImageRegistration3 MultiResImageRegistration3.cxx )
TARGET_LINK_LIBRARIES(MultiResImageRegistration3 ITKIO ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples7 RegistrationExamples7.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples7 ITKFEM ITKIO ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration1 DeformableRegistration1.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration1 ITKFEM ITKIO
                                              ITKStatistics ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration2 DeformableRegistration2.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration2 ITKIO ITKStatistics ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration3 DeformableRegistration3.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration3 ITKIO ITKStatistics ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration5 DeformableRegistration5.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration5 ITKIO ITKStatistics ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration11 DeformableRegistration11.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration11 ITKFEM ITKIO
                                              ITKStatistics ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration12 DeformableRegistration12.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration12 ITKIO ITKStatistics ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration13 DeformableRegistration13.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration13 ITKIO ITKStatistics ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration14 DeformableRegistration14.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration14 ITKIO ITKStatistics ITKNumerics)

ADD_EXECUTABLE(ChangeInformationImageFilter ChangeInformationImageFilter.cxx )
TARGET_LINK_LIBRARIES(ChangeInformationImageFilter ITKIO )

ADD_EXECUTABLE(DeformableRegistration15 DeformableRegistration15.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration15 ITKIO ITKStatistics ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration16 DeformableRegistration16.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration16 ITKIO ITKStatistics ITKNumerics)

ADD_EXECUTABLE(DeformableRegistration17 DeformableRegistration17.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration17 ITKIO ITKStatistics ITKNumerics)

ADD_EXECUTABLE(DeformationFieldInitialization DeformationFieldInitialization.cxx )
TARGET_LINK_LIBRARIES(DeformationFieldInitialization ITKIO)

ADD_EXECUTABLE(LandmarkWarping2 LandmarkWarping2.cxx )
TARGET_LINK_LIBRARIES(LandmarkWarping2 ITKIO)

ADD_EXECUTABLE(BSplineWarping1 BSplineWarping1.cxx )
TARGET_LINK_LIBRARIES(BSplineWarping1 ITKIO)

ADD_EXECUTABLE(BSplineWarping2 BSplineWarping2.cxx )
TARGET_LINK_LIBRARIES(BSplineWarping2 ITKIO)

ADD_EXECUTABLE(ThinPlateSplineWarp ThinPlateSplineWarp.cxx )
TARGET_LINK_LIBRARIES(ThinPlateSplineWarp ITKIO)

ADD_EXECUTABLE(ModelToImageRegistration1 ModelToImageRegistration1.cxx )
TARGET_LINK_LIBRARIES(ModelToImageRegistration1 ITKStatistics ITKIO ITKNumerics)

ADD_EXECUTABLE(ModelToImageRegistration2 ModelToImageRegistration2.cxx )
TARGET_LINK_LIBRARIES(ModelToImageRegistration2 ITKStatistics ITKIO ITKNumerics)

IF(NOT BORLAND)

  ADD_EXECUTABLE(ImageRegistration1 ImageRegistration1.cxx )
  TARGET_LINK_LIBRARIES(ImageRegistration1 ITKIO ITKNumerics)

  ADD_EXECUTABLE(ImageRegistration2 ImageRegistration2.cxx )
  TARGET_LINK_LIBRARIES(ImageRegistration2 ITKIO ITKNumerics)

  ADD_EXECUTABLE(ImageRegistration17 ImageRegistration17.cxx )
  TARGET_LINK_LIBRARIES(ImageRegistration17 ITKStatistics ITKIO ITKNumerics)

  IF (NOT ITK_USE_REVIEW_STATISTICS)
    ADD_EXECUTABLE(ImageRegistrationHistogramPlotter ImageRegistrationHistogramPlotter.cxx )
    TARGET_LINK_LIBRARIES(ImageRegistrationHistogramPlotter 
                          ITKIO ITKStatistics ITKNumerics)
  ENDIF (NOT ITK_USE_REVIEW_STATISTICS)

  ADD_EXECUTABLE(MeanSquaresImageMetric1  MeanSquaresImageMetric1.cxx )
  TARGET_LINK_LIBRARIES(MeanSquaresImageMetric1 ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics)

  ADD_EXECUTABLE(DeformationFieldJacobian DeformationFieldJacobian.cxx )
  TARGET_LINK_LIBRARIES(DeformationFieldJacobian ITKIO)

ENDIF(NOT BORLAND)


ADD_EXECUTABLE(IterativeClosestPoint1   IterativeClosestPoint1.cxx )
TARGET_LINK_LIBRARIES(IterativeClosestPoint1 ITKAlgorithms ITKStatistics ITKIO ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(IterativeClosestPoint2   IterativeClosestPoint2.cxx )
TARGET_LINK_LIBRARIES(IterativeClosestPoint2 ITKAlgorithms ITKStatistics ITKIO ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(IterativeClosestPoint3   IterativeClosestPoint3.cxx )
TARGET_LINK_LIBRARIES(IterativeClosestPoint3 ITKAlgorithms ITKStatistics ITKIO ITKBasicFilters ITKNumerics)


IF( NOT ITK_DISABLE_CXX_TESTING )

IF(BUILD_TESTING)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration1oTest ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration1TestPixelCentered.png
            ${TEMP}/ImageRegistration1TestPixelCentered.png
  ImageRegistration1oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1TestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceAfterTestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceBeforeTestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration1oTest ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration1Test.png
            ${TEMP}/ImageRegistration1Test.png
  ImageRegistration1oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1Test.png
        ${TEMP}/ImageRegistration1DifferenceAfterTest.png
        ${TEMP}/ImageRegistration1DifferenceBeforeTest.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration1od1Test ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration1TestPixelCentered.png
            ${TEMP}/ImageRegistration1od1TestPixelCentered.png
  ImageRegistration1oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20DirectionPlus30.mhd
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd
        ${TEMP}/ImageRegistration1od1TestPixelCentered.png
        ${TEMP}/ImageRegistration1od1DifferenceAfterTestPixelCentered.png
        ${TEMP}/ImageRegistration1od1DifferenceBeforeTestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration1od1Test ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration1Test.png
            ${TEMP}/ImageRegistration1od1Test.png
  ImageRegistration1oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20DirectionPlus30.mhd
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd
        ${TEMP}/ImageRegistration1od1Test.png
        ${TEMP}/ImageRegistration1od1DifferenceAfterTest.png
        ${TEMP}/ImageRegistration1od1DifferenceBeforeTest.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

ADD_TEST(ImageRegistration2oTest ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration2Test.png
            ${TEMP}/ImageRegistration2oTest.png
  ImageRegistration2oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration2oTest.png
)

ADD_TEST(ImageRegistration2od1Test ${REGISTRATION_EXAMPLESO}
  ImageRegistration2oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20DirectionPlus30.mhd
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd
        ${TEMP}/ImageRegistration2od1Test.mhd
)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration3oTest ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration3TestPixelCentered.png
            ${TEMP}/ImageRegistration3TestPixelCentered.png
  ImageRegistration3oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration3TestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration3oTest ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration3Test.png
            ${TEMP}/ImageRegistration3Test.png
  ImageRegistration3oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration3Test.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)


ADD_TEST(ImageRegistration4oTest ${REGISTRATION_EXAMPLESO}
  --compare ${BASELINE}/ImageRegistration4Test.png
            ${TEMP}/ImageRegistration4Test.png
  ImageRegistration4oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration4Test.png 0
        ${TEMP}/ImageRegistration4BeforeTest.png
        ${TEMP}/ImageRegistration4AfterTest.png
)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration5oTest1 ${REGISTRATION_EXAMPLESO2}
  --compare ${BASELINE}/ImageRegistration5oTest1PixelCentered.png
            ${TEMP}/ImageRegistration5oTest1PixelCentered.png
  ImageRegistration5oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceRotated10.png
        ${TEMP}/ImageRegistration5oTest1PixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration5oTest1 ${REGISTRATION_EXAMPLESO2}
  --compare ${BASELINE}/ImageRegistration5Test1.png
            ${TEMP}/ImageRegistration5oTest1.png
  ImageRegistration5oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceRotated10.png
        ${TEMP}/ImageRegistration5oTest1.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration5oTest2 ${REGISTRATION_EXAMPLESO2}
  --compare ${BASELINE}/ImageRegistration5Test2PixelCentered.png
            ${TEMP}/ImageRegistration5Test2PixelCentered.png
  ImageRegistration5oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration5Test2PixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration5oTest2 ${REGISTRATION_EXAMPLESO2}
  --compare ${BASELINE}/ImageRegistration5Test2.png
            ${TEMP}/ImageRegistration5Test2.png
  ImageRegistration5oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration5Test2.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration6oTest ${REGISTRATION_EXAMPLESO2}
  --compare ${BASELINE}/ImageRegistration6TestPixelCentered.png
            ${TEMP}/ImageRegistration6oTestPixelCentered.png
  ImageRegistration6oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration6oTestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration6oTest ${REGISTRATION_EXAMPLESO2}
  --compare ${BASELINE}/ImageRegistration6Test.png
            ${TEMP}/ImageRegistration6oTest.png
  ImageRegistration6oTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration6oTest.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration1Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration1TestPixelCentered.png
            ${TEMP}/ImageRegistration1TestPixelCentered.png
  ImageRegistration1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1TestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceAfterTestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceBeforeTestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration1Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration1Test.png
            ${TEMP}/ImageRegistration1Test.png
  ImageRegistration1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1Test.png
        ${TEMP}/ImageRegistration1DifferenceAfterTest.png
        ${TEMP}/ImageRegistration1DifferenceBeforeTest.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

ADD_TEST(ImageRegistration2Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration2Test.png
            ${TEMP}/ImageRegistration2Test.png
  ImageRegistration2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration2Test.png
        ${TEMP}/ImageRegistration2BeforeTest.png
        ${TEMP}/ImageRegistration2AfterTest.png
)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration3Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration3TestPixelCentered.png
            ${TEMP}/ImageRegistration3TestPixelCentered.png
  ImageRegistration3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration3TestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration3Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration3Test.png
            ${TEMP}/ImageRegistration3Test.png
  ImageRegistration3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration3Test.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

ADD_TEST(ImageRegistration4Test ${REGISTRATION_EXAMPLES}
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

ADD_TEST(ImageRegistration4Test2 ${REGISTRATION_EXAMPLES}
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

ADD_TEST(ImageRegistration4Test3 ${REGISTRATION_EXAMPLES}
  ImageRegistration4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration4Test3.png 0
        ${TEMP}/ImageRegistration4BeforeTest3.png
        ${TEMP}/ImageRegistration4AfterTest3.png
        5 10000 0
)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration5Test1 ${REGISTRATION_EXAMPLES9}
  --compare ${BASELINE}/ImageRegistration5Test1PixelCentered.png
            ${TEMP}/ImageRegistration5Test1PixelCentered.png
  ImageRegistration5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceRotated10.png
        ${TEMP}/ImageRegistration5Test1PixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration5Test1 ${REGISTRATION_EXAMPLES9}
  --compare ${BASELINE}/ImageRegistration5Test1.png
            ${TEMP}/ImageRegistration5Test1.png
  ImageRegistration5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceRotated10.png
        ${TEMP}/ImageRegistration5Test1.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration5Test2 ${REGISTRATION_EXAMPLES9}
  --compare ${BASELINE}/ImageRegistration5Test2PixelCentered.png
            ${TEMP}/ImageRegistration5Test2PixelCentered.png
  ImageRegistration5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration5Test2PixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration5Test2 ${REGISTRATION_EXAMPLES9}
  --compare ${BASELINE}/ImageRegistration5Test2.png
            ${TEMP}/ImageRegistration5Test2.png
  ImageRegistration5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration5Test2.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration6Test ${REGISTRATION_EXAMPLES9}
  --compare ${BASELINE}/ImageRegistration6TestPixelCentered.png
            ${TEMP}/ImageRegistration6TestPixelCentered.png
  ImageRegistration6Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration6TestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration6Test ${REGISTRATION_EXAMPLES9}
  --compare ${BASELINE}/ImageRegistration6Test.png
            ${TEMP}/ImageRegistration6Test.png
  ImageRegistration6Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration6Test.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration7Test ${REGISTRATION_EXAMPLES9}
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
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration7Test ${REGISTRATION_EXAMPLES9}
  --compare ${BASELINE}/ImageRegistration7Test.png
            ${TEMP}/ImageRegistration7Test.png
  ImageRegistration7Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17S12.png
        ${TEMP}/ImageRegistration7Test.png 
        ${TEMP}/ImageRegistration7BeforeTest.png
        ${TEMP}/ImageRegistration7AfterTest.png
        1.0   1.0  0.0
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_BRAINWEB_DATA_ROOT)
ADD_TEST(ImageRegistration8Test ${REGISTRATION_EXAMPLES11}
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

IF( ${CMAKE_BUILD_TYPE} MATCHES "Release")
  # This test is computationally demanding. 
  # Therefore we only run it in a Release build
  ADD_TEST(ImageRegistration20Test 
    ${CXX_TEST_PATH}/ImageRegistration20
      ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
      ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20Rot10Tx15.mha
      ${TEMP}/ImageRegistration20Output.mhd
      ${TEMP}/ImageRegistration20DifferenceBefore.mhd
      ${TEMP}/ImageRegistration20DifferenceAfter.mhd
      0.1  # Step length
      300  # maximum number of iterations
    )
  SET_TESTS_PROPERTIES(ImageRegistration20Test PROPERTIES RUN_SERIAL 1)
ENDIF( ${CMAKE_BUILD_TYPE} MATCHES "Release")

ENDIF(ITK_BRAINWEB_DATA_ROOT)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration9Test ${REGISTRATION_EXAMPLES11}
  --compare ${BASELINE}/ImageRegistration9TestPixelCentered.png
            ${TEMP}/ImageRegistration9TestPixelCentered.png
  ImageRegistration9Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration9TestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration9Test ${REGISTRATION_EXAMPLES11}
  --compare ${BASELINE}/ImageRegistration9Test.png
            ${TEMP}/ImageRegistration9Test.png
  ImageRegistration9Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration9Test.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration10Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration10TestPixelCentered.png
            ${TEMP}/ImageRegistration10TestPixelCentered.png
  ImageRegistration10Test
        ${ITK_SOURCE_DIR}/Testing/Data/Baseline/Segmentation/WatershedSegmentation1Test1.png
        ${ITK_SOURCE_DIR}/Testing/Data/Baseline/Segmentation/WatershedSegmentation1Test2.png
        ${TEMP}/ImageRegistration10TestPixelCentered.png
        ${TEMP}/ImageRegistration10DifferenceTestPixelCentered.png
        -11 -13
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration10Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration10Test.png
            ${TEMP}/ImageRegistration10Test.png
  ImageRegistration10Test
        ${ITK_SOURCE_DIR}/Testing/Data/Baseline/Segmentation/WatershedSegmentation1Test1.png
        ${ITK_SOURCE_DIR}/Testing/Data/Baseline/Segmentation/WatershedSegmentation1Test2.png
        ${TEMP}/ImageRegistration10Test.png
        ${TEMP}/ImageRegistration10DifferenceTest.png
        -11 -13
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration13Test ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13TestPixelCentered.png
  --compareNumberOfPixelsTolerance 5
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13TestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration13Test ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13Test.png
            ${TEMP}/ImageRegistration13Test.png
  --compareNumberOfPixelsTolerance 5
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration13Test2 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test2PixelCentered.png
  --compareNumberOfPixelsTolerance 10
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test2PixelCentered.png
        0 0
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration13Test2 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13Test.png
            ${TEMP}/ImageRegistration13Test2.png
  --compareNumberOfPixelsTolerance 10
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test2.png
        0 0
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration13Test3 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test3PixelCentered.png
  --compareNumberOfPixelsTolerance 10
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test3PixelCentered.png
        0 1
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration13Test3 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13Test.png
            ${TEMP}/ImageRegistration13Test3.png
  --compareNumberOfPixelsTolerance 10
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test3.png
        0 1
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration13Test4 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test4PixelCentered.png
  --compareNumberOfPixelsTolerance 5
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test4PixelCentered.png
        1 0
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration13Test4 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13Test.png
            ${TEMP}/ImageRegistration13Test4.png
  --compareNumberOfPixelsTolerance 5
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test4.png
        1 0
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration13Test5 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13TestPixelCentered.png
            ${TEMP}/ImageRegistration13Test5PixelCentered.png
  --compareNumberOfPixelsTolerance 5
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test5PixelCentered.png
        1 1
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration13Test5 ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13Test.png
            ${TEMP}/ImageRegistration13Test5.png
  --compareNumberOfPixelsTolerance 5
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test5.png
        1 1
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)


ADD_TEST(ImageRegistration14Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration14Test.png
            ${TEMP}/ImageRegistration14Test.png
  --compareNumberOfPixelsTolerance 2000
  ImageRegistration14Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration14Test.png 32 0.01 0.1  0.15  10.0  14.0
)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration15Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration15TestPixelCentered.png
            ${TEMP}/ImageRegistration15TestPixelCentered.png
  ImageRegistration15Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration15TestPixelCentered.png 32 0.1 0.05  0.0 0.0
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration15Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration15Test.png
            ${TEMP}/ImageRegistration15Test.png
  ImageRegistration15Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration15Test.png 32 0.1 0.05  0.0 0.0
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)


IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration16Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration16TestPixelCentered.png
            ${TEMP}/ImageRegistration16TestPixelCentered.png
  ImageRegistration16Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration16TestPixelCentered.png 0.0 0.0
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration16Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration16Test.png
            ${TEMP}/ImageRegistration16Test.png
  ImageRegistration16Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration16Test.png 0.0 0.0
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration16Test2 ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration16TestPixelCentered.png
            ${TEMP}/ImageRegistration16Test2PixelCentered.png
  ImageRegistration16Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration16Test2PixelCentered.png 0.0 0.0
        0
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration16Test2 ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration16Test.png
            ${TEMP}/ImageRegistration16Test2.png
  ImageRegistration16Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration16Test2.png 0.0 0.0
        0
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)



IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration17Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration17TestPixelCentered.png
            ${TEMP}/ImageRegistration17TestPixelCentered.png
  ImageRegistration17Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration17TestPixelCentered.png 0.0 0.0
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration17Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration17Test.png
            ${TEMP}/ImageRegistration17Test.png
  ImageRegistration17Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration17Test.png 0.0 0.0
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

ADD_TEST(ImageRegistration18Test 
  ${CXX_TEST_PATH}/ImageRegistration18
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration18Test.png 0.0 0.0
)

ADD_TEST(ImageRegistration19Test 
  ${CXX_TEST_PATH}/ImageRegistration19
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration19Test.png 
        ${TEMP}/ImageRegistration19DifferenceTest.png 
        2.0 2.0 # Initial translation parameters
)

ADD_TEST(MultiResImageRegistration1Test ${REGISTRATION_EXAMPLES2}
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

ADD_TEST(MultiResImageRegistration1Test2 ${REGISTRATION_EXAMPLES2}
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


ADD_TEST(MultiResImageRegistration2Test ${REGISTRATION_EXAMPLES2}
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


ADD_TEST(MultiResImageRegistration2Test2 ${REGISTRATION_EXAMPLES2}
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


ADD_TEST(DeformableRegistration2Test ${REGISTRATION_EXAMPLES5}
  --compare ${BASELINE}/DeformableRegistration2Test.png
            ${TEMP}/DeformableRegistration2Test.png
  DeformableRegistration2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration2Test.png
)

ADD_TEST(DeformableRegistration3Test ${REGISTRATION_EXAMPLES5}
  --compare ${BASELINE}/DeformableRegistration3Test.png
            ${TEMP}/DeformableRegistration3Test.png
  --compareNumberOfPixelsTolerance 100
  DeformableRegistration3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration3Test.png
)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(DeformableRegistration4Test ${REGISTRATION_EXAMPLES11}
  --compare ${BASELINE}/DeformableRegistration4TestPixelCentered.png
            ${TEMP}/DeformableRegistration4TestPixelCentered.png
  DeformableRegistration4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration4TestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(DeformableRegistration4Test ${REGISTRATION_EXAMPLES11}
  --compare ${BASELINE}/DeformableRegistration4Test.png
            ${TEMP}/DeformableRegistration4Test.png
  DeformableRegistration4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration4Test.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

ADD_TEST(DeformableRegistration5Test ${REGISTRATION_EXAMPLES5}
  --compare ${BASELINE}/DeformableRegistration5Test.png
            ${TEMP}/DeformableRegistration5Test.png
  --compareNumberOfPixelsTolerance 5
  DeformableRegistration5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration5Test.png
)

IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(DeformableRegistration6Test ${REGISTRATION_EXAMPLES12}
  --compare ${BASELINE}/DeformableRegistration6TestPixelCentered.png
            ${TEMP}/DeformableRegistration6TestPixelCentered.png
  DeformableRegistration6Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration6TestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(DeformableRegistration6Test ${REGISTRATION_EXAMPLES12}
  --compare ${BASELINE}/DeformableRegistration6Test.png
            ${TEMP}/DeformableRegistration6Test.png
  DeformableRegistration6Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration6Test.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)


ADD_TEST(BSplineWarping1Test ${REGISTRATION_EXAMPLES6}
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

ADD_TEST(DeformableRegistration12Test1 ${REGISTRATION_EXAMPLES13}
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

ADD_TEST(DeformableRegistration12Test2 ${REGISTRATION_EXAMPLES13}
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

ADD_TEST(DeformableRegistration12Test3 ${REGISTRATION_EXAMPLES13}
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

ADD_TEST(DeformableRegistration12Test4 ${REGISTRATION_EXAMPLES13}
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

ADD_TEST(DeformableRegistration13Test1 ${REGISTRATION_EXAMPLES13}
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

ADD_TEST(DeformableRegistration13Test2 ${REGISTRATION_EXAMPLES13}
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

ADD_TEST(DeformableRegistration13Test3 ${REGISTRATION_EXAMPLES13}
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

ADD_TEST(DeformableRegistration13Test4 ${REGISTRATION_EXAMPLES13}
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

IF(ITK_BRAINWEB_DATA_ROOT)

ADD_TEST(BSplineWarping2Test ${REGISTRATION_EXAMPLES6}
  BSplineWarping2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements2.txt
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplineWarping2Test.mhd
        ${TEMP}/BSplineWarping2TestDeformationField.mhd
        ${TEMP}/BSplineWarping2TestTransform.txt
)

SET(NUMBER_OF_BSPLINE_GRID_NODES 5)

IF( DART_TESTING_TIMEOUT GREATER 3000 )

ADD_TEST(DeformableRegistration8Test1 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration8Test2 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration8Test3 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration8Test4 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration14Test1 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration14Test2 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration14Test3 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration14Test4 ${REGISTRATION_EXAMPLES12}
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
ENDIF( DART_TESTING_TIMEOUT GREATER 3000 )

ADD_TEST(ChangeInformationTest1 
        ${CXX_TEST_PATH}/ChangeInformationImageFilter
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        ${TEMP}/BSplinedTranlatedScaledRotated.mhd
        1.05 
        1.0 2.0 0.0
        10.0
)

IF( DART_TESTING_TIMEOUT GREATER 10000 )
ADD_TEST(DeformableRegistration15Test1 ${REGISTRATION_EXAMPLES14}
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
ENDIF( DART_TESTING_TIMEOUT GREATER 10000 )

IF( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 6 )

SET(NUMBER_OF_BSPLINE_GRID_NODES 32)
ADD_TEST(DeformableRegistration8Test5 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration8Test6 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration8Test7 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration8Test8 ${REGISTRATION_EXAMPLES12}
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

SET(NUMBER_OF_BSPLINE_GRID_NODES 100)
ADD_TEST(DeformableRegistration8Test9 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration8Test10 ${REGISTRATION_EXAMPLES12}
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
SET_TESTS_PROPERTIES(
  DeformableRegistration8Test5
  DeformableRegistration8Test6
  DeformableRegistration8Test7
  DeformableRegistration8Test8
  DeformableRegistration8Test9
  DeformableRegistration8Test10    
  PROPERTIES
  RUN_SERIAL 1
  )

IF( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 16 )

ADD_TEST(DeformableRegistration8Test11 ${REGISTRATION_EXAMPLES12}
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

ADD_TEST(DeformableRegistration8Test12 ${REGISTRATION_EXAMPLES12}
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
  SET_TESTS_PROPERTIES(
    DeformableRegistration8Test11
    DeformableRegistration8Test12   
    PROPERTIES
    RUN_SERIAL 1
    )
ENDIF( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 16 )

ENDIF( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 6 )

ENDIF(ITK_BRAINWEB_DATA_ROOT)


IF(ITK_MRI_UNC_DATABASE_DATA_ROOT)

IF( DART_TESTING_TIMEOUT GREATER 10000 )
ADD_TEST(DeformableRegistration15Test2 ${REGISTRATION_EXAMPLES14}
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
ENDIF( DART_TESTING_TIMEOUT GREATER 10000 )

ENDIF(ITK_MRI_UNC_DATABASE_DATA_ROOT)


ADD_TEST(LandmarkWarping2Test ${REGISTRATION_EXAMPLES6}
  --compare ${BASELINE}/LandmarkWarping2Test.png
            ${TEMP}/LandmarkWarping2Test.png
  LandmarkWarping2Test
        ${ITK_SOURCE_DIR}/Examples/Data/LandmarkWarping2Landmarks1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${TEMP}/LandmarkWarping2Test.png
)
IF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration12Test ${REGISTRATION_EXAMPLES4}
  --compare ${BASELINE}/ImageRegistration12TestPixelCentered.png
            ${TEMP}/ImageRegistration12TestPixelCentered.png
  --compareNumberOfPixelsTolerance 2
  ImageRegistration12Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration12TestPixelCentered.png
)
ELSE(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)
ADD_TEST(ImageRegistration12Test ${REGISTRATION_EXAMPLES4}
  --compare ${BASELINE}/ImageRegistration12Test.png
            ${TEMP}/ImageRegistration12Test.png
  ImageRegistration12Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration12Test.png
)
ENDIF(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)




ADD_TEST(ModelToImageRegistration2Test ${REGISTRATION_EXAMPLES10}
  ModelToImageRegistration2Test
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cake_easy.png  12 12
)


ADD_TEST(IterativeClosestPoint1Test ${REGISTRATION_EXAMPLES14}
  IterativeClosestPoint1Test
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointFixedPoints.txt
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointMovingPoints.txt
)

ADD_TEST(IterativeClosestPoint2Test ${REGISTRATION_EXAMPLES14}
  IterativeClosestPoint2Test
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointFixedPoints2.txt
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointMovingPoints2.txt
)

ADD_TEST(IterativeClosestPoint3Test ${REGISTRATION_EXAMPLES14}
  IterativeClosestPoint3Test
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointFixedPoints.txt
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointMovingPoints.txt
)


ADD_EXECUTABLE(RegistrationExamples RegistrationExamples.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamplesO RegistrationExamplesO.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamplesO ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamplesO2 RegistrationExamplesO2.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamplesO2 ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples2 RegistrationExamples2.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples2 ITKAlgorithms ITKFEM ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples3 RegistrationExamples3.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples3 ITKFEM ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples4 RegistrationExamples4.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples4 ITKFEM ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples5 RegistrationExamples5.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples5 ITKAlgorithms ITKFEM ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples6 RegistrationExamples6.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples6 ITKFEM ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples8 RegistrationExamples8.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples8 ITKAlgorithms ITKFEM ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples9 RegistrationExamples9.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples9 ITKFEM ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples10 RegistrationExamples10.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples10 ITKFEM ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples11 RegistrationExamples11.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples11 ITKFEM ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples12 RegistrationExamples12.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples12 ITKIO ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples13 RegistrationExamples13.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples13 ITKIO ITKBasicFilters ITKNumerics)

ADD_EXECUTABLE(RegistrationExamples14 RegistrationExamples14.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples14 ITKIO ITKBasicFilters ITKNumerics)

ENDIF(BUILD_TESTING)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )

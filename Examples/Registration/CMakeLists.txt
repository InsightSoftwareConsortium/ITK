PROJECT(ImageRegistration)
INCLUDE_REGULAR_EXPRESSION("^.*$")

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Registration)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

SET(REGISTRATION_EXAMPLES ${CXX_TEST_PATH}/RegistrationExamples)
SET(REGISTRATION_EXAMPLES2 ${CXX_TEST_PATH}/RegistrationExamples2)
SET(REGISTRATION_EXAMPLES3 ${CXX_TEST_PATH}/RegistrationExamples3)
SET(REGISTRATION_EXAMPLES4 ${CXX_TEST_PATH}/RegistrationExamples4)
SET(REGISTRATION_EXAMPLES5 ${CXX_TEST_PATH}/RegistrationExamples5)
SET(REGISTRATION_EXAMPLES6 ${CXX_TEST_PATH}/RegistrationExamples6)
SET(REGISTRATION_EXAMPLES7 ${CXX_TEST_PATH}/RegistrationExamples7)
SET(REGISTRATION_EXAMPLES8 ${CXX_TEST_PATH}/RegistrationExamples8)

ADD_EXECUTABLE(ImageRegistration1 ImageRegistration1.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration1 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration2 ImageRegistration2.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration2 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration3 ImageRegistration3.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration3 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration4 ImageRegistration4.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration4 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration5 ImageRegistration5.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration5 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration6 ImageRegistration6.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration6 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration8 ImageRegistration8.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration8 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration9 ImageRegistration9.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration9 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration10 ImageRegistration10.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration10 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration11 ImageRegistration11.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration11 ITKStatistics ITKNumerics ITKIO)

IF(NOT BORLAND)
  ADD_EXECUTABLE(ImageRegistration7 ImageRegistration7.cxx )
  TARGET_LINK_LIBRARIES(ImageRegistration7 ITKNumerics ITKIO)

  ADD_EXECUTABLE(ImageRegistration12 ImageRegistration12.cxx )
  TARGET_LINK_LIBRARIES(ImageRegistration12 ITKStatistics ITKNumerics ITKIO)

  ADD_EXECUTABLE(ImageRegistration13 ImageRegistration13.cxx )
  TARGET_LINK_LIBRARIES(ImageRegistration13 ITKStatistics ITKNumerics ITKIO)

  ADD_EXECUTABLE(ImageRegistration14 ImageRegistration14.cxx )
  TARGET_LINK_LIBRARIES(ImageRegistration14 ITKStatistics ITKNumerics ITKIO)

  ADD_EXECUTABLE(ImageRegistration15 ImageRegistration15.cxx )
  TARGET_LINK_LIBRARIES(ImageRegistration15 ITKStatistics ITKNumerics ITKIO)

  ADD_EXECUTABLE(ImageRegistration16 ImageRegistration16.cxx )
  TARGET_LINK_LIBRARIES(ImageRegistration16 ITKStatistics ITKNumerics ITKIO)

  ADD_EXECUTABLE(ImageRegistration17 ImageRegistration17.cxx )
  TARGET_LINK_LIBRARIES(ImageRegistration17 ITKStatistics ITKNumerics ITKIO)

  ADD_EXECUTABLE(DeformableRegistration6 DeformableRegistration6.cxx )
  TARGET_LINK_LIBRARIES(DeformableRegistration6 ITKNumerics ITKIO)

  ADD_EXECUTABLE(DeformableRegistration7 DeformableRegistration7.cxx )
  TARGET_LINK_LIBRARIES(DeformableRegistration7 ITKNumerics ITKIO)

  ADD_EXECUTABLE(DeformableRegistration8 DeformableRegistration8.cxx )
  TARGET_LINK_LIBRARIES(DeformableRegistration8 ITKNumerics ITKIO)

  ADD_EXECUTABLE(MultiResImageRegistration1 MultiResImageRegistration1.cxx )
  TARGET_LINK_LIBRARIES(MultiResImageRegistration1 ITKNumerics ITKIO)

  ADD_EXECUTABLE(RegistrationExamples7 RegistrationExamples7.cxx )
  TARGET_LINK_LIBRARIES(RegistrationExamples7 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)

ENDIF(NOT BORLAND)

ADD_EXECUTABLE(MultiResImageRegistration2 MultiResImageRegistration2.cxx )
TARGET_LINK_LIBRARIES(MultiResImageRegistration2 ITKNumerics ITKIO)

ADD_EXECUTABLE(DeformableRegistration1 DeformableRegistration1.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration1 ITKNumerics ITKFEM ITKIO)

ADD_EXECUTABLE(DeformableRegistration2 DeformableRegistration2.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration2 ITKNumerics ITKIO)

ADD_EXECUTABLE(DeformableRegistration3 DeformableRegistration3.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration3 ITKNumerics ITKIO)

ADD_EXECUTABLE(DeformableRegistration4 DeformableRegistration4.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration4 ITKNumerics ITKIO)

ADD_EXECUTABLE(DeformableRegistration5 DeformableRegistration5.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration5 ITKNumerics ITKIO)

ADD_EXECUTABLE(DeformationFieldInitialization DeformationFieldInitialization.cxx )
TARGET_LINK_LIBRARIES(DeformationFieldInitialization ITKNumerics ITKIO)

ADD_EXECUTABLE(DeformationFieldJacobian DeformationFieldJacobian.cxx )
TARGET_LINK_LIBRARIES(DeformationFieldJacobian ITKNumerics ITKIO)

ADD_EXECUTABLE(LandmarkWarping2 LandmarkWarping2.cxx )
TARGET_LINK_LIBRARIES(LandmarkWarping2 ITKNumerics ITKIO)

ADD_EXECUTABLE(BSplineWarping1 BSplineWarping1.cxx )
TARGET_LINK_LIBRARIES(BSplineWarping1 ITKNumerics ITKIO)

ADD_EXECUTABLE(ModelToImageRegistration1 ModelToImageRegistration1.cxx )
TARGET_LINK_LIBRARIES(ModelToImageRegistration1 ITKStatistics ITKNumerics ITKIO)

ADD_EXECUTABLE(MeanSquaresImageMetric1  MeanSquaresImageMetric1.cxx )
TARGET_LINK_LIBRARIES(MeanSquaresImageMetric1 ITKAlgorithms ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(ImageRegistrationHistogramPlotter ImageRegistrationHistogramPlotter.cxx )
TARGET_LINK_LIBRARIES(ImageRegistrationHistogramPlotter ITKNumerics ITKIO ITKCommon)

IF( NOT ITK_DISABLE_CXX_TESTING )

ADD_TEST(ImageRegistration1Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration1Test.png
            ${TEMP}/ImageRegistration1Test.png
  ImageRegistration1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1Test.png
)

ADD_TEST(ImageRegistration2Test ${REGISTRATION_EXAMPLES}
  ImageRegistration2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration2Test.png
)

ADD_TEST(ImageRegistration3Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration3Test.png
            ${TEMP}/ImageRegistration3Test.png
  ImageRegistration3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration3Test.png
)

ADD_TEST(ImageRegistration4Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration4Test.png
            ${TEMP}/ImageRegistration4Test.png
  ImageRegistration4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration4Test.png 0
)

ADD_TEST(ImageRegistration5Test1 ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration5Test1.png
            ${TEMP}/ImageRegistration5Test1.png
  ImageRegistration5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceRotated10.png
        ${TEMP}/ImageRegistration5Test1.png
)

ADD_TEST(ImageRegistration5Test2 ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration5Test2.png
            ${TEMP}/ImageRegistration5Test2.png
  ImageRegistration5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration5Test2.png
)

ADD_TEST(ImageRegistration6Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration6Test.png
            ${TEMP}/ImageRegistration6Test.png
  ImageRegistration6Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration6Test.png
)

ADD_TEST(ImageRegistration9Test ${REGISTRATION_EXAMPLES3}
  --compare ${BASELINE}/ImageRegistration9Test.png
            ${TEMP}/ImageRegistration9Test.png
  ImageRegistration9Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration9Test.png
)

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

IF( NOT BORLAND )

ADD_TEST(ImageRegistration13Test ${REGISTRATION_EXAMPLES7}
  --compare ${BASELINE}/ImageRegistration13Test.png
            ${TEMP}/ImageRegistration13Test.png
  ImageRegistration13Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test.png
)

ADD_TEST(ImageRegistration14Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration14Test.png
            ${TEMP}/ImageRegistration14Test.png
  ImageRegistration14Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration14Test.png 32 0.01 0.1  0.15  10.0  14.0
)

ADD_TEST(ImageRegistration15Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration15Test.png
            ${TEMP}/ImageRegistration15Test.png
  ImageRegistration15Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration15Test.png 32 0.1 0.05  0.0 0.0
)

ADD_TEST(ImageRegistration16Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration16Test.png
            ${TEMP}/ImageRegistration16Test.png
  ImageRegistration16Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration16Test.png 0.0 0.0
)


ENDIF( NOT BORLAND )


ADD_TEST(ImageRegistration17Test ${REGISTRATION_EXAMPLES8}
  --compare ${BASELINE}/ImageRegistration17Test.png
            ${TEMP}/ImageRegistration17Test.png
  ImageRegistration17Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration17Test.png 0.0 0.0
)


ADD_TEST(MultiResImageRegistration1Test ${REGISTRATION_EXAMPLES2}
  --compare ${BASELINE}/MultiResImageRegistration1Test.png
            ${TEMP}/MultiResImageRegistration1Test.png
  MultiResImageRegistration1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration1Test.png
)

ADD_TEST(MultiResImageRegistration2Test ${REGISTRATION_EXAMPLES2}
  --compare ${BASELINE}/MultiResImageRegistration2Test.png
            ${TEMP}/MultiResImageRegistration2Test.png
  MultiResImageRegistration2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration2Test.png  0
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
  DeformableRegistration3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration3Test.png
)

ADD_TEST(DeformableRegistration4Test ${REGISTRATION_EXAMPLES6}
  --compare ${BASELINE}/DeformableRegistration4Test.png
            ${TEMP}/DeformableRegistration4Test.png
  DeformableRegistration4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration4Test.png
)

ADD_TEST(DeformableRegistration5Test ${REGISTRATION_EXAMPLES5}
  --compare ${BASELINE}/DeformableRegistration5Test.png
            ${TEMP}/DeformableRegistration5Test.png
  DeformableRegistration5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration5Test.png
)

ADD_TEST(DeformableRegistration6Test ${REGISTRATION_EXAMPLES6}
  --compare ${BASELINE}/DeformableRegistration6Test.png
            ${TEMP}/DeformableRegistration6Test.png
  DeformableRegistration6Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBSplined10.png
        ${TEMP}/DeformableRegistration6Test.png
)

ADD_TEST(BSplineWarping1Test ${REGISTRATION_EXAMPLES6}
  --compare ${BASELINE}/BSplineWarping1Test.png
            ${TEMP}/BSplineWarping1Test.png
  BSplineWarping1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${TEMP}/BSplineWarping1Test.png
)

ADD_TEST(LandmarkWarping2Test ${REGISTRATION_EXAMPLES6}
  --compare ${BASELINE}/LandmarkWarping2Test.png
            ${TEMP}/LandmarkWarping2Test.png
  LandmarkWarping2Test
        ${ITK_SOURCE_DIR}/Examples/Data/LandmarkWarping2Landmarks1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${TEMP}/LandmarkWarping2Test.png
)

ADD_TEST(ImageRegistration12Test ${REGISTRATION_EXAMPLES4}
  --compare ${BASELINE}/ImageRegistration12Test.png
            ${TEMP}/ImageRegistration12Test.png
  ImageRegistration12Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration12Test.png
)

ADD_EXECUTABLE(RegistrationExamples RegistrationExamples.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples ITKAlgorithms ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(RegistrationExamples2 RegistrationExamples2.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples2 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(RegistrationExamples3 RegistrationExamples3.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples3 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(RegistrationExamples4 RegistrationExamples4.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples4 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(RegistrationExamples5 RegistrationExamples5.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples5 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(RegistrationExamples6 RegistrationExamples6.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples6 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(RegistrationExamples8 RegistrationExamples8.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples8 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)


ENDIF( NOT ITK_DISABLE_CXX_TESTING )


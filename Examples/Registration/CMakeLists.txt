PROJECT(ImageRegistration)
INCLUDE_REGULAR_EXPRESSION("^.*$")

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

ADD_EXECUTABLE(ImageRegistration7 ImageRegistration7.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration7 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration8 ImageRegistration8.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration8 ITKNumerics ITKIO)

ADD_EXECUTABLE(ImageRegistration9 ImageRegistration9.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration9 ITKNumerics ITKIO)

ADD_EXECUTABLE(MultiResImageRegistration1 MultiResImageRegistration1.cxx )
TARGET_LINK_LIBRARIES(MultiResImageRegistration1 ITKNumerics ITKIO)

ADD_EXECUTABLE(MultiResImageRegistration2 MultiResImageRegistration2.cxx )
TARGET_LINK_LIBRARIES(MultiResImageRegistration2 ITKNumerics ITKIO)

ADD_EXECUTABLE(DeformableRegistration1 DeformableRegistration1.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration1 ITKNumerics ITKFEM ITKIO)

ADD_EXECUTABLE(DeformableRegistration2 DeformableRegistration2.cxx )
TARGET_LINK_LIBRARIES(DeformableRegistration2 ITKNumerics ITKIO)

ADD_EXECUTABLE(ModelToImageRegistration1 ModelToImageRegistration1.cxx )
TARGET_LINK_LIBRARIES(ModelToImageRegistration1 ITKStatistics ITKNumerics ITKIO)

SET(REGISTRATION_EXAMPLES ${CXX_TEST_PATH}/RegistrationExamples)
SET(REGISTRATION_EXAMPLES2 ${CXX_TEST_PATH}/RegistrationExamples2)

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Registration)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

ADD_TEST(ImageRegistration1Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration1Test.png
            ${TEMP}/ImageRegistration1Test.png
  ImageRegistration1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1Test.png
)

ADD_TEST(ImageRegistration2Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration2Test.png
            ${TEMP}/ImageRegistration2Test.png
  ImageRegistration2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration2Test.png
)

ADD_TEST(ImageRegistration3Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration3Test.png
            ${TEMP}/ImageRegistration3Test.png
  ImageRegistration3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration3Test.png
)

ADD_TEST(ImageRegistration4Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration4Test.png
            ${TEMP}/ImageRegistration4Test.png
  ImageRegistration4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration4Test.png
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

ADD_TEST(ImageRegistration9Test ${REGISTRATION_EXAMPLES}
  --compare ${BASELINE}/ImageRegistration9Test.png
            ${TEMP}/ImageRegistration9Test.png
  ImageRegistration9Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration9Test.png
)

ADD_EXECUTABLE(RegistrationExamples RegistrationExamples.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples ITKAlgorithms ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(RegistrationExamples2 RegistrationExamples2.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples2 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)


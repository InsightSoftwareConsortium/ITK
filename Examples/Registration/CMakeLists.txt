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

ADD_EXECUTABLE(RegistrationExamples RegistrationExamples.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples ITKAlgorithms ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(RegistrationExamples2 RegistrationExamples2.cxx )
TARGET_LINK_LIBRARIES(RegistrationExamples2 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)


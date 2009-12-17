PROJECT(IOExamples)

ADD_EXECUTABLE(ImageReadWrite ImageReadWrite.cxx )
TARGET_LINK_LIBRARIES(ImageReadWrite  ITKIO)

ADD_EXECUTABLE(ImageReadCastWrite ImageReadCastWrite.cxx )
TARGET_LINK_LIBRARIES(ImageReadCastWrite  ITKIO)

ADD_EXECUTABLE(ImageReadExtractFilterInsertWrite ImageReadExtractFilterInsertWrite.cxx )
TARGET_LINK_LIBRARIES(ImageReadExtractFilterInsertWrite  ITKIO)

ADD_EXECUTABLE(ImageReadRegionOfInterestWrite ImageReadRegionOfInterestWrite.cxx )
TARGET_LINK_LIBRARIES(ImageReadRegionOfInterestWrite  ITKIO)

ADD_EXECUTABLE(RGBImageReadWrite RGBImageReadWrite.cxx )
TARGET_LINK_LIBRARIES(RGBImageReadWrite  ITKIO)

ADD_EXECUTABLE(CovariantVectorImageRead CovariantVectorImageRead.cxx )
TARGET_LINK_LIBRARIES(CovariantVectorImageRead  ITKIO)

ADD_EXECUTABLE(CovariantVectorImageExtractComponent CovariantVectorImageExtractComponent.cxx )
TARGET_LINK_LIBRARIES(CovariantVectorImageExtractComponent  ITKIO)

ADD_EXECUTABLE(VectorImageReadWrite VectorImageReadWrite.cxx )
TARGET_LINK_LIBRARIES(VectorImageReadWrite  ITKIO)

ADD_EXECUTABLE(ComplexImageReadWrite ComplexImageReadWrite.cxx )
TARGET_LINK_LIBRARIES(ComplexImageReadWrite  ITKIO)

ADD_EXECUTABLE(ImageReadExportVTK ImageReadExportVTK.cxx )
TARGET_LINK_LIBRARIES(ImageReadExportVTK  ITKIO)

ADD_EXECUTABLE(RGBImageSeriesReadWrite RGBImageSeriesReadWrite.cxx )
TARGET_LINK_LIBRARIES(RGBImageSeriesReadWrite  ITKIO)

ADD_EXECUTABLE(ImageSeriesReadWrite ImageSeriesReadWrite.cxx )
TARGET_LINK_LIBRARIES(ImageSeriesReadWrite  ITKIO)

ADD_EXECUTABLE(ImageSeriesReadWrite2 ImageSeriesReadWrite2.cxx )
TARGET_LINK_LIBRARIES(ImageSeriesReadWrite2  ITKIO)

ADD_EXECUTABLE(ImageReadImageSeriesWrite ImageReadImageSeriesWrite.cxx )
TARGET_LINK_LIBRARIES(ImageReadImageSeriesWrite  ITKIO)

ADD_EXECUTABLE(DicomSeriesReadImageWrite DicomSeriesReadImageWrite.cxx )
TARGET_LINK_LIBRARIES(DicomSeriesReadImageWrite  ITKIO)

ADD_EXECUTABLE(DicomSeriesReadImageWrite2 DicomSeriesReadImageWrite2.cxx )
TARGET_LINK_LIBRARIES(DicomSeriesReadImageWrite2  ITKIO)

ADD_EXECUTABLE(DicomSeriesReadGaussianImageWrite DicomSeriesReadGaussianImageWrite.cxx )
TARGET_LINK_LIBRARIES(DicomSeriesReadGaussianImageWrite  ITKIO)

ADD_EXECUTABLE(DicomSeriesReadPrintTags DicomSeriesReadPrintTags.cxx )
TARGET_LINK_LIBRARIES(DicomSeriesReadPrintTags  ITKIO)

ADD_EXECUTABLE(DicomImageReadPrintTags DicomImageReadPrintTags.cxx )
TARGET_LINK_LIBRARIES(DicomImageReadPrintTags  ITKIO)

ADD_EXECUTABLE(DicomImageReadWrite DicomImageReadWrite.cxx )
TARGET_LINK_LIBRARIES(DicomImageReadWrite  ITKIO)

ADD_EXECUTABLE(ImageReadDicomSeriesWrite ImageReadDicomSeriesWrite.cxx )
TARGET_LINK_LIBRARIES(ImageReadDicomSeriesWrite  ITKIO)

ADD_EXECUTABLE(DicomImageReadChangeHeaderWrite DicomImageReadChangeHeaderWrite.cxx )
TARGET_LINK_LIBRARIES(DicomImageReadChangeHeaderWrite  ITKIO)

ADD_EXECUTABLE(DicomSeriesReadSeriesWrite DicomSeriesReadSeriesWrite.cxx )
TARGET_LINK_LIBRARIES(DicomSeriesReadSeriesWrite  ITKIO)

ADD_EXECUTABLE(DicomPrintPatientInformation DicomPrintPatientInformation.cxx )
TARGET_LINK_LIBRARIES(DicomPrintPatientInformation  ITKIO)

ADD_EXECUTABLE(TransformReadWrite TransformReadWrite.cxx )
TARGET_LINK_LIBRARIES(TransformReadWrite  ITKIO)

ADD_EXECUTABLE(VisibleHumanStreamReadWrite  VisibleHumanStreamReadWrite.cxx )
TARGET_LINK_LIBRARIES(VisibleHumanStreamReadWrite  ITKIO)

ADD_EXECUTABLE(VisibleHumanPasteWrite  VisibleHumanPasteWrite.cxx )
TARGET_LINK_LIBRARIES(VisibleHumanPasteWrite  ITKIO)


SET(IO_EXAMPLES ${CXX_TEST_PATH}/IOExamples)

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Registration)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

ADD_EXECUTABLE(IOExamples IOExamples.cxx )
TARGET_LINK_LIBRARIES(IOExamples ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics)

IF(ITK_VISIBLEHUMAN_DATA_ROOT)
  ADD_TEST(VisibleHumanStreamReadWriteTest 
    ${IO_EXAMPLES}
    VisibleHumanStreamReadWriteTest 
    ${ITK_VISIBLEHUMAN_DATA_ROOT}/Male/Fullcolor/fullbody/
    ${TEMP}/VisibleHumanStreamReadWrite.mha )
  ADD_TEST(VisibleHumanPasteWriteTest 
    ${IO_EXAMPLES}
    VisibleHumanPasteWriteTest
    ${TEMP}/VisibleHumanStreamReadWrite.mha
    ${TEMP}/VisibleHumanPasteWrite.mha)
  # this test must be run sequentially, be cause the output of the first is used by the second
  SET_TESTS_PROPERTIES(
    VisibleHumanStreamReadWriteTest
    VisibleHumanPasteWriteTest
    PROPERTIES
    RUN_SERIAL 1
    )
ENDIF(ITK_VISIBLEHUMAN_DATA_ROOT)

IF(NOT BORLAND)
  ADD_EXECUTABLE(CovariantVectorImageWrite CovariantVectorImageWrite.cxx )
  TARGET_LINK_LIBRARIES(CovariantVectorImageWrite  ITKIO)

  ADD_EXECUTABLE(ImageReadExtractWrite ImageReadExtractWrite.cxx )
  TARGET_LINK_LIBRARIES(ImageReadExtractWrite  ITKIO)
ENDIF(NOT BORLAND)


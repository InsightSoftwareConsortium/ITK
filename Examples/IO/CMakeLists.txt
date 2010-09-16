project(IOExamples)

add_executable(ImageReadWrite ImageReadWrite.cxx )
target_link_libraries(ImageReadWrite  ITKIO)

add_executable(ImageReadCastWrite ImageReadCastWrite.cxx )
target_link_libraries(ImageReadCastWrite  ITKIO)

add_executable(ImageReadExtractFilterInsertWrite ImageReadExtractFilterInsertWrite.cxx )
target_link_libraries(ImageReadExtractFilterInsertWrite  ITKIO)

add_executable(ImageReadRegionOfInterestWrite ImageReadRegionOfInterestWrite.cxx )
target_link_libraries(ImageReadRegionOfInterestWrite  ITKIO)

add_executable(RGBImageReadWrite RGBImageReadWrite.cxx )
target_link_libraries(RGBImageReadWrite  ITKIO)

add_executable(CovariantVectorImageRead CovariantVectorImageRead.cxx )
target_link_libraries(CovariantVectorImageRead  ITKIO)

add_executable(CovariantVectorImageExtractComponent CovariantVectorImageExtractComponent.cxx )
target_link_libraries(CovariantVectorImageExtractComponent  ITKIO)

add_executable(VectorImageReadWrite VectorImageReadWrite.cxx )
target_link_libraries(VectorImageReadWrite  ITKIO)

add_executable(ComplexImageReadWrite ComplexImageReadWrite.cxx )
target_link_libraries(ComplexImageReadWrite  ITKIO)

add_executable(ImageReadExportVTK ImageReadExportVTK.cxx )
target_link_libraries(ImageReadExportVTK  ITKIO)

add_executable(RGBImageSeriesReadWrite RGBImageSeriesReadWrite.cxx )
target_link_libraries(RGBImageSeriesReadWrite  ITKIO)

add_executable(ImageSeriesReadWrite ImageSeriesReadWrite.cxx )
target_link_libraries(ImageSeriesReadWrite  ITKIO)

add_executable(ImageSeriesReadWrite2 ImageSeriesReadWrite2.cxx )
target_link_libraries(ImageSeriesReadWrite2  ITKIO)

add_executable(ImageReadImageSeriesWrite ImageReadImageSeriesWrite.cxx )
target_link_libraries(ImageReadImageSeriesWrite  ITKIO)

add_executable(DicomSeriesReadImageWrite DicomSeriesReadImageWrite.cxx )
target_link_libraries(DicomSeriesReadImageWrite  ITKIO)

add_executable(DicomSeriesReadImageWrite2 DicomSeriesReadImageWrite2.cxx )
target_link_libraries(DicomSeriesReadImageWrite2  ITKIO)

add_executable(DicomSeriesReadGaussianImageWrite DicomSeriesReadGaussianImageWrite.cxx )
target_link_libraries(DicomSeriesReadGaussianImageWrite  ITKIO)

add_executable(DicomSeriesReadPrintTags DicomSeriesReadPrintTags.cxx )
target_link_libraries(DicomSeriesReadPrintTags  ITKIO)

add_executable(DicomImageReadPrintTags DicomImageReadPrintTags.cxx )
target_link_libraries(DicomImageReadPrintTags  ITKIO)

add_executable(DicomImageReadWrite DicomImageReadWrite.cxx )
target_link_libraries(DicomImageReadWrite  ITKIO)

add_executable(ImageReadDicomSeriesWrite ImageReadDicomSeriesWrite.cxx )
target_link_libraries(ImageReadDicomSeriesWrite  ITKIO)

add_executable(DicomImageReadChangeHeaderWrite DicomImageReadChangeHeaderWrite.cxx )
target_link_libraries(DicomImageReadChangeHeaderWrite  ITKIO)

add_executable(DicomSeriesReadSeriesWrite DicomSeriesReadSeriesWrite.cxx )
target_link_libraries(DicomSeriesReadSeriesWrite  ITKIO)

add_executable(DicomPrintPatientInformation DicomPrintPatientInformation.cxx )
target_link_libraries(DicomPrintPatientInformation  ITKIO)

add_executable(TransformReadWrite TransformReadWrite.cxx )
target_link_libraries(TransformReadWrite  ITKIO)

add_executable(VisibleHumanStreamReadWrite  VisibleHumanStreamReadWrite.cxx )
target_link_libraries(VisibleHumanStreamReadWrite  ITKIO)

add_executable(VisibleHumanPasteWrite  VisibleHumanPasteWrite.cxx )
target_link_libraries(VisibleHumanPasteWrite  ITKIO)


set(IO_EXAMPLES ${CXX_TEST_PATH}/IOExamples)

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Registration)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

add_executable(IOExamples IOExamples.cxx )
target_link_libraries(IOExamples ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics)

if(ITK_VISIBLEHUMAN_DATA_ROOT)
  add_test(VisibleHumanStreamReadWriteTest
    ${IO_EXAMPLES}
    VisibleHumanStreamReadWriteTest
    ${ITK_VISIBLEHUMAN_DATA_ROOT}/Male/Fullcolor/fullbody/
    ${TEMP}/VisibleHumanStreamReadWrite.mha )
  add_test(VisibleHumanPasteWriteTest
    ${IO_EXAMPLES}
    VisibleHumanPasteWriteTest
    ${TEMP}/VisibleHumanStreamReadWrite.mha
    ${TEMP}/VisibleHumanPasteWrite.mha)
  # this test must be run sequentially, be cause the output of the first is used by the second
  set_tests_properties(
    VisibleHumanStreamReadWriteTest
    VisibleHumanPasteWriteTest
    PROPERTIES
    RUN_SERIAL 1
    )
endif(ITK_VISIBLEHUMAN_DATA_ROOT)

add_executable(CovariantVectorImageWrite CovariantVectorImageWrite.cxx )
target_link_libraries(CovariantVectorImageWrite  ITKIO)

add_executable(ImageReadExtractWrite ImageReadExtractWrite.cxx )
target_link_libraries(ImageReadExtractWrite  ITKIO)


project(IOExamples)

add_executable(ImageReadWrite ImageReadWrite.cxx )
target_link_libraries(ImageReadWrite  ${ITK_LIBRARIES})

add_executable(ImageReadCastWrite ImageReadCastWrite.cxx )
target_link_libraries(ImageReadCastWrite  ${ITK_LIBRARIES})

add_executable(ImageReadExtractFilterInsertWrite ImageReadExtractFilterInsertWrite.cxx )
target_link_libraries(ImageReadExtractFilterInsertWrite  ${ITK_LIBRARIES})

add_executable(ImageReadRegionOfInterestWrite ImageReadRegionOfInterestWrite.cxx )
target_link_libraries(ImageReadRegionOfInterestWrite  ${ITK_LIBRARIES})

add_executable(RGBImageReadWrite RGBImageReadWrite.cxx )
target_link_libraries(RGBImageReadWrite  ${ITK_LIBRARIES})

add_executable(CovariantVectorImageRead CovariantVectorImageRead.cxx )
target_link_libraries(CovariantVectorImageRead  ${ITK_LIBRARIES})

add_executable(CovariantVectorImageExtractComponent CovariantVectorImageExtractComponent.cxx )
target_link_libraries(CovariantVectorImageExtractComponent  ${ITK_LIBRARIES})

add_executable(VectorImageReadWrite VectorImageReadWrite.cxx )
target_link_libraries(VectorImageReadWrite  ${ITK_LIBRARIES})

add_executable(ComplexImageReadWrite ComplexImageReadWrite.cxx )
target_link_libraries(ComplexImageReadWrite  ${ITK_LIBRARIES})

add_executable(ImageReadExportVTK ImageReadExportVTK.cxx )
target_link_libraries(ImageReadExportVTK  ${ITK_LIBRARIES})

add_executable(RGBImageSeriesReadWrite RGBImageSeriesReadWrite.cxx )
target_link_libraries(RGBImageSeriesReadWrite  ${ITK_LIBRARIES})

add_executable(ImageSeriesReadWrite ImageSeriesReadWrite.cxx )
target_link_libraries(ImageSeriesReadWrite  ${ITK_LIBRARIES})

add_executable(ImageSeriesReadWrite2 ImageSeriesReadWrite2.cxx )
target_link_libraries(ImageSeriesReadWrite2  ${ITK_LIBRARIES})

add_executable(ImageReadImageSeriesWrite ImageReadImageSeriesWrite.cxx )
target_link_libraries(ImageReadImageSeriesWrite  ${ITK_LIBRARIES})

if(NOT ITK_USE_MODULAR_BUILD)
  add_executable(DicomSeriesReadImageWrite DicomSeriesReadImageWrite.cxx )
  target_link_libraries(DicomSeriesReadImageWrite  ${ITK_LIBRARIES})
endif()

add_executable(DicomSeriesReadImageWrite2 DicomSeriesReadImageWrite2.cxx )
target_link_libraries(DicomSeriesReadImageWrite2  ${ITK_LIBRARIES})

add_executable(DicomSeriesReadGaussianImageWrite DicomSeriesReadGaussianImageWrite.cxx )
target_link_libraries(DicomSeriesReadGaussianImageWrite  ${ITK_LIBRARIES})

add_executable(DicomSeriesReadPrintTags DicomSeriesReadPrintTags.cxx )
target_link_libraries(DicomSeriesReadPrintTags  ${ITK_LIBRARIES})

add_executable(DicomImageReadPrintTags DicomImageReadPrintTags.cxx )
target_link_libraries(DicomImageReadPrintTags  ${ITK_LIBRARIES})

add_executable(DicomImageReadWrite DicomImageReadWrite.cxx )
target_link_libraries(DicomImageReadWrite  ${ITK_LIBRARIES})

add_executable(ImageReadDicomSeriesWrite ImageReadDicomSeriesWrite.cxx )
target_link_libraries(ImageReadDicomSeriesWrite  ${ITK_LIBRARIES})

add_executable(DicomImageReadChangeHeaderWrite DicomImageReadChangeHeaderWrite.cxx )
target_link_libraries(DicomImageReadChangeHeaderWrite  ${ITK_LIBRARIES})

add_executable(DicomSeriesReadSeriesWrite DicomSeriesReadSeriesWrite.cxx )
target_link_libraries(DicomSeriesReadSeriesWrite  ${ITK_LIBRARIES})

add_executable(DicomPrintPatientInformation DicomPrintPatientInformation.cxx )
target_link_libraries(DicomPrintPatientInformation  ${ITK_LIBRARIES})

add_executable(TransformReadWrite TransformReadWrite.cxx )
target_link_libraries(TransformReadWrite  ${ITK_LIBRARIES})

add_executable(VisibleHumanStreamReadWrite  VisibleHumanStreamReadWrite.cxx )
target_link_libraries(VisibleHumanStreamReadWrite  ${ITK_LIBRARIES})

add_executable(VisibleHumanPasteWrite  VisibleHumanPasteWrite.cxx )
target_link_libraries(VisibleHumanPasteWrite  ${ITK_LIBRARIES})


set(IO_EXAMPLES ${CXX_TEST_PATH}/IOExamples)

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Registration)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

add_executable(IOExamples IOExamples.cxx )
target_link_libraries(IOExamples ${ITK_LIBRARIES})

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
    PROPERTIES
    RUN_SERIAL 1
    )
  set_tests_properties(
    VisibleHumanPasteWriteTest
    PROPERTIES
    DEPENDS VisibleHumanStreamReadWriteTest
    )
endif(ITK_VISIBLEHUMAN_DATA_ROOT)

add_executable(CovariantVectorImageWrite CovariantVectorImageWrite.cxx )
target_link_libraries(CovariantVectorImageWrite  ${ITK_LIBRARIES})

add_executable(ImageReadExtractWrite ImageReadExtractWrite.cxx )
target_link_libraries(ImageReadExtractWrite  ${ITK_LIBRARIES})


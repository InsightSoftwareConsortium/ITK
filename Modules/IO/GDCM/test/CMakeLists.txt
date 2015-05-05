itk_module_test()
set(ITKIOGDCMTests
itkGDCMImageIOTest.cxx
itkGDCMImageIOTest2.cxx
itkGDCMImageIONoCrashTest.cxx
itkGDCMImageReadSeriesWriteTest.cxx
itkGDCMSeriesReadImageWriteTest.cxx
itkGDCMSeriesMissingDicomTagTest.cxx
itkGDCMSeriesStreamReadImageWriteTest.cxx
itkGDCMImagePositionPatientTest.cxx
itkGDCMImageIOOrthoDirTest.cxx
itkGDCMImageOrientationPatientTest.cxx
itkGDCMLoadImageSpacingTest.cxx
)

CreateTestDriver(ITKIOGDCM  "${ITKIOGDCM-Test_LIBRARIES}" "${ITKIOGDCMTests}")

itk_add_test(NAME itkGDCMImageIOTest1
  COMMAND ITKIOGDCMTestDriver
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.mha
    DATA{Baseline/itkGDCMImageIOTestBaseline.mha}
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTestRescaled.mha
    DATA{Baseline/itkGDCMImageIOTestRescaledBaseline.mha}
  itkGDCMImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.mha
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTestRescaled.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTestRescaled.mha
  )
itk_add_test(NAME itkGDCMImageIOTest2
  COMMAND ITKIOGDCMTestDriver
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.mha
    DATA{Baseline/itkGDCMImageIOTest2Baseline.mha}
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2Rescaled.mha
    DATA{Baseline/itkGDCMImageIOTest2RescaledBaseline.mha}
  itkGDCMImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.mha
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2Rescaled.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2Rescaled.mha
  )
itk_add_test(NAME itkGDCMImageIOTest3
  COMMAND ITKIOGDCMTestDriver
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.mha
    DATA{Baseline/itkGDCMImageIOTest3Baseline.mha}
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3Rescaled.mha
    DATA{Baseline/itkGDCMImageIOTest3RescaledBaseline.mha}
  itkGDCMImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest3.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.mha
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3Rescaled.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3Rescaled.mha
  )
itk_add_test(NAME itkGDCMImageIOTest4
  COMMAND ITKIOGDCMTestDriver
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.mha
    DATA{Baseline/itkGDCMImageIOTest4Baseline.mha}
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4Rescaled.mha
    DATA{Baseline/itkGDCMImageIOTest4RescaledBaseline.mha}
  itkGDCMImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/012345.002.050}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.mha
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4Rescaled.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4Rescaled.mha
  )
itk_add_test(NAME itkGDCMImageIOTest5
      COMMAND ITKIOGDCMTestDriver itkGDCMImageIOTest2
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest5)

itk_add_test(NAME itkGDCMSeriesReadImageWriteTest
      COMMAND ITKIOGDCMTestDriver itkGDCMSeriesReadImageWriteTest
              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
              ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesReadImageWriteTest.vtk ${ITK_TEST_OUTPUT_DIR})

set_property(TEST itkGDCMSeriesReadImageWriteTest APPEND PROPERTY DEPENDS ITKData)

itk_add_test(NAME itkGDCMSeriesStreamReadImageWriteTest1
      COMMAND ITKIOGDCMTestDriver itkGDCMSeriesStreamReadImageWriteTest
              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
              ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWriteTest1.mhd 0.859375 0.85939 1.60016 0)

set_property(TEST itkGDCMSeriesStreamReadImageWriteTest1 APPEND PROPERTY DEPENDS ITKData)

itk_add_test(NAME itkGDCMSeriesStreamReadImageWriteTest2
      COMMAND ITKIOGDCMTestDriver itkGDCMSeriesStreamReadImageWriteTest
              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
              ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWriteTest2.mhd 0.859375 0.85939 1.60016 1)

set_property(TEST itkGDCMSeriesStreamReadImageWriteTest2 APPEND PROPERTY DEPENDS ITKData)

itk_add_test(NAME itkGDCMImagePositionPatientTest
      COMMAND ITKIOGDCMTestDriver itkGDCMImagePositionPatientTest
              ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkGDCMImageReadSeriesWriteTest
      COMMAND ITKIOGDCMTestDriver
      --compare DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha}
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageReadSeriesWriteTest.mha
      itkGDCMImageReadSeriesWriteTest
        DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha}
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageReadSeriesWriteTest
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageReadSeriesWriteTest.mha )

itk_add_test(NAME itkGDCMSeriesMissingDicomTagTest
  COMMAND ITKIOGDCMTestDriver itkGDCMSeriesMissingDicomTagTest
  DATA{${ITK_DATA_ROOT}/Input/DicomSeries2/,Image0075.dcm,Image0076-missingTag.dcm})

itk_add_test(NAME itkGDCMImageIOOrthoDirTest
  COMMAND ITKIOGDCMTestDriver itkGDCMImageIOOrthoDirTest
  DATA{${ITK_DATA_ROOT}/Input/OrthogonalDirectionsTest.dcm})

itk_add_test(NAME itkGDCMImageOrientationPatientTest
      COMMAND ITKIOGDCMTestDriver itkGDCMImageOrientationPatientTest
              ${ITK_TEST_OUTPUT_DIR})

set_property(TEST itkGDCMSeriesMissingDicomTagTest APPEND PROPERTY DEPENDS ITKData)

itk_add_test(NAME itkGDCMImageIONoCrashTest
             COMMAND ITKIOGDCMTestDriver itkGDCMImageIONoCrashTest DATA{${ITK_DATA_ROOT}/Input/OT-PAL-8-face.dcm})

itk_add_test(NAME itkGDCMLoadImageSpacingTest
  COMMAND ITKIOGDCMTestDriver itkGDCMLoadImageSpacingTest
    DATA{Input/gdcmSpacingTest.dcm}
    0.178038
    0.174924
  )

itk_add_test(NAME itkGDCMLoadImageNoSpacingTest
  COMMAND ITKIOGDCMTestDriver itkGDCMLoadImageSpacingTest
  DATA{Input/gdcmNoSpacingTest.dcm}
    1.0
    1.0
  )

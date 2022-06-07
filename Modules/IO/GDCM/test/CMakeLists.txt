itk_module_test()
set(ITKIOGDCMTests
itkGDCMImageIOTest.cxx
itkGDCMImageIOTest2.cxx
itkGDCMImageIONoCrashTest.cxx
itkGDCMImageReadSeriesWriteTest.cxx
itkGDCMImageReadWriteTest.cxx
itkGDCMSeriesReadImageWriteTest.cxx
itkGDCMSeriesMissingDicomTagTest.cxx
itkGDCMSeriesStreamReadImageWriteTest.cxx
itkGDCMImagePositionPatientTest.cxx
itkGDCMImageIOOrthoDirTest.cxx
itkGDCMImageOrientationPatientTest.cxx
itkGDCMLoadImageSpacingTest.cxx
itkGDCMLegacyMultiFrameTest.cxx
itkGDCMImageIONoPreambleTest.cxx
)

CreateTestDriver(ITKIOGDCM  "${ITKIOGDCM-Test_LIBRARIES}" "${ITKIOGDCMTests}")

itk_add_test(NAME itkGDCMImageIOTest1
  COMMAND ITKIOGDCMTestDriver
  --compare
    DATA{Baseline/itkGDCMImageIOTestBaseline.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.mha
  --compare
    DATA{Baseline/itkGDCMImageIOTestRescaledBaseline.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTestRescaled.mha
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
    DATA{Baseline/itkGDCMImageIOTest2Baseline.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.mha
  --compare
    DATA{Baseline/itkGDCMImageIOTest2RescaledBaseline.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2Rescaled.mha
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
    DATA{Baseline/itkGDCMImageIOTest3Baseline.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.mha
  --compare
    DATA{Baseline/itkGDCMImageIOTest3RescaledBaseline.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3Rescaled.mha
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
    DATA{Baseline/itkGDCMImageIOTest4Baseline.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.mha
  --compare
    DATA{Baseline/itkGDCMImageIOTest4RescaledBaseline.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4Rescaled.mha
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
              ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWriteTest1.mhd 0.859375 0.85939 1.60016 0 1 0 0 0)

set_property(TEST itkGDCMSeriesStreamReadImageWriteTest1 APPEND PROPERTY DEPENDS ITKData)

itk_add_test(NAME itkGDCMSeriesStreamReadImageWriteTest2
      COMMAND ITKIOGDCMTestDriver itkGDCMSeriesStreamReadImageWriteTest
              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
              ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWriteTest2.mhd 0.859375 0.85939 1.60016 0 1 0 0 1)

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
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageReadSeriesWriteTest.mha)

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

itk_add_test(NAME itkGDCMLegacyMultiFrameTest
  COMMAND ITKIOGDCMTestDriver
    --compare
      DATA{Baseline/itkGDCMLegacyMultiFrameTest.mha}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCMLegacyMultiFrameTest.mha
    itkGDCMLegacyMultiFrameTest
      # This dataset was derived from
      # https://www.dropbox.com/s/8m7ugu4cmw83fvd/dicoms-anon.zip?dl=0
      # per the discussion here
      # https://discourse.slicer.org/t/dicom-multiframe-support/4806/9
      DATA{Input/LegacyMultiFrame.dcm}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCMLegacyMultiFrameTest.mha
  )

itk_add_test(NAME itkGDCMImageIONoPreambleTest
  COMMAND ITKIOGDCMTestDriver itkGDCMImageIONoPreambleTest
  DATA{Input/NoPreambleDicomTest.dcm}
  )

itk_add_test(NAME itkGDCMImageReadWriteTest_RGB
  COMMAND ITKIOGDCMTestDriver
    --compare
      DATA{Baseline/itkGDCMImageReadWriteTest_RGB.mha}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageReadWriteTest_RGB.mha
    itkGDCMImageReadWriteTest
      DATA{${ITK_DATA_ROOT}/Input/RGBDicomTest.dcm}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageReadWriteTest_RGB.mha
      rgb)

itk_add_test(NAME itkGDCM_ComplianceTestRGB_JPEG2000ICT
  COMMAND ITKIOGDCMTestDriver
    --compare DATA{Baseline/US1_J2KI.mha}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCM_ComplianceTestRGB_JPEG2000ICT.mha
    itkGDCMImageReadWriteTest
      DATA{Input/US1_J2KI.dcm}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCM_ComplianceTestRGB_JPEG2000ICT.mha
      rgb)

itk_add_test(NAME itkGDCM_ComplianceTestRGB_JPEG2000RCT
  COMMAND ITKIOGDCMTestDriver
    --compare DATA{Baseline/US1_J2KR.mha}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCM_ComplianceTestRGB_JPEG2000RCT.mha
    itkGDCMImageReadWriteTest
      DATA{Input/US1_J2KR.dcm}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCM_ComplianceTestRGB_JPEG2000RCT.mha
      rgb)

itk_add_test(NAME itkGDCMImageReadWriteTest_JPEGBaseline1
  COMMAND ITKIOGDCMTestDriver
    --compareIntensityTolerance 5
    --compareNumberOfPixelsTolerance 71
    --compare DATA{Baseline/itkGDCMImageReadWriteTest_RGB.mha}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageReadWriteTest_JPEGBaseline1.mha
    itkGDCMImageReadWriteTest
      DATA{Input/JPEGBaseline1DicomTest.dcm}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageReadWriteTest_JPEGBaseline1.mha
      rgb)

itk_add_test(NAME itkGDCMImageIOTest3_mono1
  COMMAND ITKIOGDCMTestDriver
    --ignoreInputInformation
    --compare DATA{Baseline/itkGDCMImageIOTest3Baseline.mha}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3_mono1.mha
    itkGDCMImageReadWriteTest
      DATA{Input/itkGDCMImageIOTest3_mono1.dcm}
      ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3_mono1.mha
      scalar
  )

itk_add_test(NAME itkGDCM_ComplianceTest_singlebit
  COMMAND ITKIOGDCMTestDriver
    --compare DATA{Baseline/single-bit.mha}
      ${ITK_TEST_OUTPUT_DIR}/single-bit.mha
    itkGDCMImageReadWriteTest
      DATA{Input/single-bit.dcm}
      ${ITK_TEST_OUTPUT_DIR}/single-bit.mha
      scalar)

function(AddComplianceTest fileName)
  itk_add_test(NAME itkGDCM_ComplianceTestRGB_${fileName}
    COMMAND ITKIOGDCMTestDriver
      --compareIntensityTolerance 3
      --compareCoordinateTolerance 0.001
      --compare DATA{Baseline/Lily.png}
        ${ITK_TEST_OUTPUT_DIR}/itkGDCM_ComplianceTestRGB_${fileName}.mha
      itkGDCMImageReadWriteTest
        DATA{Input/Lily/${fileName}.dcm}
        ${ITK_TEST_OUTPUT_DIR}/itkGDCM_ComplianceTestRGB_${fileName}.mha
        rgb)
endfunction()

AddComplianceTest(JPEG2000-YBR_RCT)
AddComplianceTest(JPEGLS-RGB)
AddComplianceTest(losslessJPEG-RGB)
AddComplianceTest(lossyJPEG-YBR_FULL_422)
AddComplianceTest(raw-RGB)
AddComplianceTest(raw-YBR_FULL)
AddComplianceTest(raw-YBR_FULL_422)
AddComplianceTest(RLE-RGB)

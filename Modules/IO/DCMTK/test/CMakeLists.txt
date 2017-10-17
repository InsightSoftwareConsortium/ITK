itk_module_test()
set(ITKIODCMTKTests
  itkDCMTKImageIOTest.cxx
  itkDCMTKRGBImageIOTest.cxx
  itkDCMTKSeriesReadImageWrite.cxx
  itkDCMTKSeriesStreamReadImageWrite.cxx
  itkDCMTKImageIOOrthoDirTest.cxx
  itkDCMTKMultiFrame4DTest.cxx
  itkDCMTKLoggerTest.cxx
  itkDCMTKImageIOSlopeInterceptTest.cxx
  itkDCMTKImageIOMultiFrameImageTest.cxx
)

CreateTestDriver(ITKIODCMTK "${ITKIODCMTK-Test_LIBRARIES}" "${ITKIODCMTKTests}")

itk_add_test(NAME itkDCMTKImageIOTest1
  COMMAND ITKIODCMTKTestDriver itkDCMTKImageIOTest
  DATA{${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest.dcm
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest.png
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKRescaleImageIOTest.dcm)

itk_add_test(NAME itkDCMTKImageIOTest2
  COMMAND ITKIODCMTKTestDriver itkDCMTKImageIOTest
  DATA{${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest.dcm}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest2.dcm
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest2.png
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKRescaleImageIOTest2.dcm)

itk_add_test(NAME itkDCMTKImageIOTest3
  COMMAND ITKIODCMTKTestDriver itkDCMTKImageIOTest
  DATA{${ITK_DATA_ROOT}/Input/012345.002.050}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest3.dcm
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest3.png
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKRescaleImageIOTest3.dcm)

itk_add_test(NAME itkDCMTKImageIOTest4 COMMAND ITKIODCMTKTestDriver
  itkDCMTKImageIOTest
  DATA{${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest3.dcm}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest4.dcm
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest4.png
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKRescaleImageIOTest4.dcm)

itk_add_test(NAME itkDCMTKSeriesReadImageWrite
  COMMAND ITKIODCMTKTestDriver itkDCMTKSeriesReadImageWrite
  DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesReadImageWrite.vtk)

itk_add_test(NAME itkDCMTKDirCosinesTest
  COMMAND ITKIODCMTKTestDriver itkDCMTKSeriesReadImageWrite
  DATA{${ITK_DATA_ROOT}/Input/DicomDirectionsTest/,REGEX:1020_[0-9]+.dcm}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesReadImageWrite.vtk)

set_property(TEST itkDCMTKSeriesReadImageWrite APPEND PROPERTY DEPENDS ITKData)

itk_add_test(NAME itkDCMTKSeriesStreamReadImageWrite1
  COMMAND ITKIODCMTKTestDriver itkDCMTKSeriesStreamReadImageWrite
  DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesStreamReadImageWrite1.mhd
  0.859375 0.85939 1.60016 0)

itk_add_test(NAME itkDCMTKSeriesImageOrientationWrite1
  COMMAND ITKIODCMTKTestDriver
  --compare DATA{${ITK_DATA_ROOT}/Baseline/itkDCMTKSeriesImageOrientationWrite1.mha}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesImageOrientationWrite1.mha
  itkDCMTKSeriesReadImageWrite
  DATA{${ITK_DATA_ROOT}/Input/DicomImageOrientationTest/,REGEX:ImageOrientation.[0-9]+.dcm}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesImageOrientationWrite1.mha)

itk_add_test(NAME itkDCMTKImageIOOrthoDirTest
  COMMAND ITKIODCMTKTestDriver itkDCMTKImageIOOrthoDirTest
  DATA{${ITK_DATA_ROOT}/Input/OrthogonalDirectionsTest.dcm})

itk_add_test(NAME itkDCMTKMultiFrame4DTest
  COMMAND ITKIODCMTKTestDriver itkDCMTKMultiFrame4DTest
  DATA{${ITK_DATA_ROOT}/Input/Philips_4D_DICOM.dcm}
  ${ITK_TEST_OUTPUT_DIR}/Philips_4D_DICOM.nrrd )

itk_add_test(NAME itkDCMTKImageIONonSquareTest
  COMMAND ITKIODCMTKTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/itkDCMTKNonSquare.nii.gz}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKNonSquare.nii.gz
  itkDCMTKImageIOTest
  DATA{${ITK_DATA_ROOT}/Input/itkDCMTKNonSquare.dcm}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKNonSquare.nii.gz
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKNonSquare.nii.gz
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKNonSquareRescale.dcm)

itk_add_test(NAME itkDCMTKRGBImageIOColorImageTest
  COMMAND ITKIODCMTKTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/itkDCMTKColorImage.png}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKColorImage.png
  itkDCMTKRGBImageIOTest
  DATA{${ITK_DATA_ROOT}/Input/RGBDicomTest.dcm}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKColorImage.png)

itk_add_test(NAME ITKDCMTKLoggerTest
  COMMAND ITKIODCMTKTestDriver itkDCMTKLoggerTest
  )

itk_add_test(NAME itkDCMTKImageIOSlopeInterceptTest
  COMMAND ITKIODCMTKTestDriver itkDCMTKImageIOSlopeInterceptTest
  DATA{Input/slopeInterceptOrig.dcm}
  DATA{Input/slopeIntercept.dcm}
  )

itk_add_test(NAME itkDCMTKImageIOMultiFrameImageTest
  COMMAND ITKIODCMTKTestDriver itkDCMTKImageIOMultiFrameImageTest
  DATA{Input/MultiFrameDicomTest.dcm}
  )

itk_module_test()
set(ITK-IO-GDCMTests
itkIOGDCMHeaderTest.cxx
itkGDCMImageIOTest.cxx
itkGDCMImageIOTest2.cxx
itkGDCMSeriesReadImageWrite.cxx
itkGDCMSeriesStreamReadImageWrite.cxx
)

CreateTestDriver(ITK-IO-GDCM  "${ITK-IO-GDCM-Test_LIBRARIES}" "${ITK-IO-GDCMTests}")

itk_add_test(NAME itkIOGDCMHeaderTest
      COMMAND ITK-IO-GDCMTestDriver itkIOGDCMHeaderTest)
itk_add_test(NAME itkGDCMImageIOTest1
      COMMAND ITK-IO-GDCMTestDriver itkGDCMImageIOTest
              ${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest.dcm)
itk_add_test(NAME itkGDCMImageIOTest2
      COMMAND ITK-IO-GDCMTestDriver itkGDCMImageIOTest
              ${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest2.dcm)
itk_add_test(NAME itkGDCMImageIOTest3
      COMMAND ITK-IO-GDCMTestDriver itkGDCMImageIOTest
              ${ITK_DATA_ROOT}/Input/012345.002.050 ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest3.dcm)
itk_add_test(NAME itkGDCMImageIOTest4
      COMMAND ITK-IO-GDCMTestDriver itkGDCMImageIOTest
              ${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest3.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest4.dcm)
itk_add_test(NAME itkGDCMImageIOTest5
      COMMAND ITK-IO-GDCMTestDriver itkGDCMImageIOTest2
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest5)
itk_add_test(NAME itkGDCMSeriesReadImageWrite
      COMMAND ITK-IO-GDCMTestDriver itkGDCMSeriesReadImageWrite
              ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesReadImageWrite.vtk ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkGDCMSeriesStreamReadImageWrite1
      COMMAND ITK-IO-GDCMTestDriver itkGDCMSeriesStreamReadImageWrite
              ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWrite1.mhd 0.859375 0.85939 1.60016 0)
itk_add_test(NAME itkGDCMSeriesStreamReadImageWrite2
      COMMAND ITK-IO-GDCMTestDriver itkGDCMSeriesStreamReadImageWrite
              ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWrite2.mhd 0.859375 0.85939 1.60016 1)

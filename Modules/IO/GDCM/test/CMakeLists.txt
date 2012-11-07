itk_module_test()
set(ITKIOGDCMTests
itkGDCMImageIOTest.cxx
itkGDCMImageIOTest2.cxx
itkGDCMSeriesReadImageWrite.cxx
itkGDCMSeriesStreamReadImageWrite.cxx
itkGDCMImagePositionPatientTest.cxx
)

CreateTestDriver(ITKIOGDCM  "${ITKIOGDCM-Test_LIBRARIES}" "${ITKIOGDCMTests}")

itk_add_test(NAME itkGDCMImageIOTest1
      COMMAND ITKIOGDCMTestDriver itkGDCMImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm} ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest.dcm)
itk_add_test(NAME itkGDCMImageIOTest2
      COMMAND ITKIOGDCMTestDriver itkGDCMImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest.dcm} ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest2.dcm)
itk_add_test(NAME itkGDCMImageIOTest3
      COMMAND ITKIOGDCMTestDriver itkGDCMImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/012345.002.050} ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest3.dcm)
itk_add_test(NAME itkGDCMImageIOTest4
      COMMAND ITKIOGDCMTestDriver itkGDCMImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest3.dcm} ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest4.dcm)
itk_add_test(NAME itkGDCMImageIOTest5
      COMMAND ITKIOGDCMTestDriver itkGDCMImageIOTest2
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest5)

## Construct the DICOMSERIES_DIR from external data, and set up the proper dependancies.
ExternalData_expand_arguments(ITKData VAR_IMAGE0075 DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0075.dcm})
ExternalData_expand_arguments(ITKData VAR_IMAGE0076 DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm})
ExternalData_expand_arguments(ITKData VAR_IMAGE0077 DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0077.dcm})
get_filename_component(DICOMSERIES_DIR ${VAR_IMAGE0075} PATH)

itk_add_test(NAME itkGDCMSeriesReadImageWrite
      COMMAND ITKIOGDCMTestDriver itkGDCMSeriesReadImageWrite
              ${DICOMSERIES_DIR} ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesReadImageWrite.vtk ${ITK_TEST_OUTPUT_DIR})
set_property(TEST itkGDCMSeriesReadImageWrite        APPEND PROPERTY DEPENDS ITKData)

itk_add_test(NAME itkGDCMSeriesStreamReadImageWrite1
      COMMAND ITKIOGDCMTestDriver itkGDCMSeriesStreamReadImageWrite
              ${DICOMSERIES_DIR} ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWrite1.mhd 0.859375 0.85939 1.60016 0)
set_property(TEST itkGDCMSeriesStreamReadImageWrite1 APPEND PROPERTY DEPENDS ITKData)

itk_add_test(NAME itkGDCMSeriesStreamReadImageWrite2
      COMMAND ITKIOGDCMTestDriver itkGDCMSeriesStreamReadImageWrite
              ${DICOMSERIES_DIR} ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWrite2.mhd 0.859375 0.85939 1.60016 1)
set_property(TEST itkGDCMSeriesStreamReadImageWrite2 APPEND PROPERTY DEPENDS ITKData)

itk_add_test(NAME itkGDCMImagePositionPatientTest
      COMMAND ITKIOGDCMTestDriver itkGDCMImagePositionPatientTest
              ${ITK_TEST_OUTPUT_DIR})

itk_module_test()
set(ITKIODCMTKTests
  itkDCMTKImageIOTest.cxx
  itkDCMTKSeriesReadImageWrite.cxx
  itkDCMTKSeriesStreamReadImageWrite.cxx
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



## Construct the DICOMSERIES_DIR from external data, and set up the proper dependancies.
ExternalData_expand_arguments(ITKData VAR_IMAGE0075 DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0075.dcm})
ExternalData_expand_arguments(ITKData VAR_IMAGE0076 DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm})
ExternalData_expand_arguments(ITKData VAR_IMAGE0077 DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0077.dcm})
get_filename_component(DICOMSERIES_DIR ${VAR_IMAGE0075} PATH)

itk_add_test(NAME itkDCMTKSeriesReadImageWrite
  COMMAND ITKIODCMTKTestDriver itkDCMTKSeriesReadImageWrite
  ${DICOMSERIES_DIR}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesReadImageWrite.vtk ${ITK_TEST_OUTPUT_DIR})

set_property(TEST itkDCMTKSeriesReadImageWrite APPEND PROPERTY DEPENDS ITKData)

itk_add_test(NAME itkDCMTKSeriesStreamReadImageWrite1
  COMMAND ITKIODCMTKTestDriver itkDCMTKSeriesStreamReadImageWrite
  ${DICOMSERIES_DIR}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesStreamReadImageWrite1.mhd
  0.859375 0.85939 1.60016 0)

#
# this test depends on being able to get the ExternalData infrastructure
# to unpack tar files into dicom directories.
if(DONT_USE)
itk_add_test(NAME itkDCMTKGeSignaHDx COMMAND ITKIODCMTKTestDriver
  itkDCMTKSeriesReadImageWrite
  DATA{${ITK_DATA_ROOT}/Input/GeSignaHDx.tar.gz}
  ${ITK_TEST_OUTPUT_DIR}/itkDCMTKGeSignaHDx.nrrd)
endif()

SET(IO_TESTS ${CXX_TEST_PATH}/itkIOTests)
SET(IO_HEADER_TEST ${CXX_TEST_PATH}/itkIOHeaderTest)

SET(IOTest_SRCS
itkIOPrintTest.cxx
itkAnalyzeImageIOTest.cxx
itkConvertBufferTest.cxx
itkDicomImageIOTest.cxx
itkDICOMImageIO2Test.cxx
itkDICOMSeriesFileNamesTest.cxx
itkDICOMImageSeriesTest.cxx
itkGEImageIOTest.cxx
itkGiplImageIOTest.cxx
itkMetaImageIOTest.cxx
itkNumericSeriesFileNamesTest.cxx
itkPNGImageIOTest.cxx
itkRawImageIOTest.cxx
itkRawImageIOTest2.cxx
itkRawImageIOTest3.cxx
itkRawImageIOTest4.cxx
itkRegularExpressionSeriesFileNamesTest.cxx
itkVOLImageIOTest.cxx
itkVTKImageIOTest.cxx
testMetaUtils.cxx
testMetaObject.cxx
testMetaImage.cxx
testMetaTube.cxx
testMetaBlob.cxx
testMetaSurface.cxx
testMetaLandmark.cxx
testMetaLine.cxx
testMetaScene.cxx
)

SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

ADD_TEST(itkIOPrintTest ${IO_TESTS} itkIOPrintTest)
ADD_TEST(itkConvertBufferTest ${IO_TESTS} itkConvertBufferTest)
ADD_TEST(testMetaUtils ${IO_TESTS} testMetaUtils)
ADD_TEST(testMetaObject ${IO_TESTS} testMetaObject)
ADD_TEST(testMetaImage ${IO_TESTS} testMetaImage)
ADD_TEST(testMetaTube ${IO_TESTS} testMetaTube)
ADD_TEST(testMetaBlob ${IO_TESTS} testMetaBlob)
ADD_TEST(testMetaSurface ${IO_TESTS} testMetaSurface)
ADD_TEST(testMetaLine ${IO_TESTS} testMetaLine)
ADD_TEST(testMetaLandmark ${IO_TESTS} testMetaLandmark)
ADD_TEST(testMetaScene ${IO_TESTS} testMetaScene)
ADD_TEST(itkVTKImageIOTest ${IO_TESTS} itkVTKImageIOTest
                           ${TEMP}/test1.vtk ${TEMP}/test2.vtk)
ADD_TEST(itkRawImageIOTest ${IO_TESTS} itkRawImageIOTest
                           ${TEMP}/raw1.raw ${TEMP}/raw2.raw
)
ADD_TEST(itkRawImageIOTest3 ${IO_TESTS} itkRawImageIOTest3
                           ${TEMP}/raw1.raw ${TEMP}/raw2.raw
)
ADD_TEST(itkRawImageIOTest4 ${IO_TESTS} itkRawImageIOTest4
                           ${TEMP}/bigendian.raw ${TEMP}/littleendian.raw
)

ADD_TEST(itkIOHeaderTest ${IO_HEADER_TEST})
ADD_EXECUTABLE(itkIOHeaderTest itkIOHeaderTest.cxx)
ADD_EXECUTABLE(itkIOTests itkIOTests.cxx ${IOTest_SRCS})
TARGET_LINK_LIBRARIES(itkIOHeaderTest ITKIO)
TARGET_LINK_LIBRARIES(itkIOTests ITKIO)


##Add tests that depend on finding data
FIND_PATH(VW_RAW_DATA_PATH avfCROP.1023c.raw 
          ${ITKTesting_SOURCE_DIR}/../../InsightData/VisibleWomanHead/CROPraw)
MARK_AS_ADVANCED(VW_RAW_DATA_PATH)

IF(VW_RAW_DATA_PATH)
  ADD_TEST(itkRawImageIOTest2 ${IO_TESTS} itkRawImageIOTest2
           ${VW_RAW_DATA_PATH}/avfCROP.1023c.raw)
ENDIF(VW_RAW_DATA_PATH)

IF(ITK_DATA_ROOT)
ADD_TEST(itkMetaImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mhd
  itkMetaImageIOTest
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mhd)
ADD_TEST(itkGiplImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd
            ${ITK_TEST_OUTPUT_DIR}/ramp.mhd
  itkGiplImageIOTest 
            ${ITK_DATA_ROOT}/Input/ramp.gipl
            ${ITK_TEST_OUTPUT_DIR}/ramp.mhd)
ADD_TEST(itkDicomImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/itkDicomImageIOTest.png
            ${ITK_TEST_OUTPUT_DIR}/itkDicomImageIOTest.png
  itkDicomImageIOTest
            ${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm
            ${ITK_TEST_OUTPUT_DIR}/itkDicomImageIOTest.png)
ADD_TEST(itkDICOMImageIO2Test ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/itkDICOMImageIO2Test.png
            ${ITK_TEST_OUTPUT_DIR}/itkDICOMImageIO2Test.png
  itkDICOMImageIO2Test
            ${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm
            ${ITK_TEST_OUTPUT_DIR}/itkDICOMImageIO2Test.png)

ADD_TEST(itkDICOMImageSeriesTest ${IO_TESTS} 
  itkDICOMImageSeriesTest
            ${ITK_DATA_ROOT}/Input/DicomSeries 0)
ADD_TEST(itkDICOMSeriesFileNamesTest ${IO_TESTS} 
  itkDICOMSeriesFileNamesTest
            ${ITK_DATA_ROOT}/Input/DicomSeries)

ADD_TEST(itkNumericSeriesFileNamesTest ${IO_TESTS} 
  itkNumericSeriesFileNamesTest)

  ADD_TEST(itkDicomDifficultFile ${IO_TESTS} itkDicomImageIOTest
           ${ITK_DATA_ROOT}/Input/012345.002.050
           ${ITK_TEST_OUTPUT_DIR}/itkDicomDifficultFile.png)
  ADD_TEST(itkPNGImageIOTest1 ${IO_TESTS} itkPNGImageIOTest
           ${ITK_DATA_ROOT}/Input/cthead1.png)
  ADD_TEST(itkPNGImageIOTest2 ${IO_TESTS} itkPNGImageIOTest
           ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png)
  ADD_TEST(itkAnalyzeImageIOTest ${IO_TESTS}
    itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR})
  ADD_TEST(itkAnalyzeIOBigEndianCompressed.hdr ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.hdr)
  ADD_TEST(itkAnalyzeIOBigEndianCompressed.img ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img)
  ADD_TEST(itkAnalyzeIOBigEndianCompressed.img.gz ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img.gz)
  ADD_TEST(itkAnalyzeIOBigEndian ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndian.hdr)
  ADD_TEST(itkAnalyzeIOLittleEndianCompressed ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/LittleEndianCompressed.hdr)
  ADD_TEST(itkAnalyzeIOLittleEndian ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
  ADD_TEST(itkAnalyzeIOInternalTests ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR} )
  ADD_TEST(itkAnalyzeIOShouldFail ${IO_TESTS} itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                  false ${ITK_DATA_ROOT}/Input/ramp.gipl)
  ADD_TEST(itkAnalyzeIOShouldSucceed ${IO_TESTS} itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                  true ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
  ADD_TEST(itkGE4 ${IO_TESTS} itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true 
		  GE4 ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001)
  ADD_TEST(itkGE5 ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} true 
		  GE5 ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001)
  ADD_TEST(itkGEAdw ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} true 
		  GEAdw ${ITK_DATA_ROOT}/Input/test_geadw/I.001)
  ADD_TEST(itkSiemens ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} true 
		  Siemens ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima)

  ADD_TEST(itkGE4Fail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false 
		  GE4 
		  ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001)
  ADD_TEST(itkGE5Fail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false 
		  GE5 
		  ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001)
  ADD_TEST(itkGEAdwFail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false 
		  GEAdw 
		  ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima)
  ADD_TEST(itkSiemensFail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false 
		  Siemens 
		  ${ITK_DATA_ROOT}/Input/test_geadw/I.001)

ENDIF(ITK_DATA_ROOT)

IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-IO ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/IO)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

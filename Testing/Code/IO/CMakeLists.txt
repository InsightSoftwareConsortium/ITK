SET(IO_TESTS ${CXX_TEST_PATH}/itkIOTests)
SET(IO_HEADER_TEST ${CXX_TEST_PATH}/itkIOHeaderTest)

SET(IOTest_SRCS
itkAnalyzeImageIOTest.cxx
itkConvertBufferTest.cxx
itkDicomImageIOTest.cxx
itkGiplImageIOTest.cxx
itkImageSeriesIOTest.cxx
itkMetaImageIOTest.cxx
itkPNGImageIOTest.cxx
itkRawImageIOTest.cxx
itkRawImageIOTest2.cxx
itkRawImageIOTest3.cxx
itkRawImageIOTest4.cxx
itkVOLImageIOTest.cxx
itkVTKImageIOTest.cxx
)

ADD_TEST(itkConvertBufferTest ${IO_TESTS} itkConvertBufferTest)
ADD_TEST(itkAnalyzeImageIOTest ${IO_TESTS} itkAnalyzeImageIOTest)
ADD_TEST(itkVOLImageIOTest ${IO_TESTS} itkVOLImageIOTest)
ADD_TEST(itkVTKImageIOTest ${IO_TESTS} itkVTKImageIOTest)
ADD_TEST(itkRawImageIOTest ${IO_TESTS} itkRawImageIOTest)
ADD_TEST(itkRawImageIOTest3 ${IO_TESTS} itkRawImageIOTest3)
ADD_TEST(itkRawImageIOTest4 ${IO_TESTS} itkRawImageIOTest4)

ADD_TEST(itkIOHeaderTest ${IO_HEADER_TEST})
ADD_EXECUTABLE(itkIOHeaderTest itkIOHeaderTest.cxx)
ADD_EXECUTABLE(itkIOTests itkIOTests.cxx ${IOTest_SRCS})
TARGET_LINK_LIBRARIES(itkIOHeaderTest ITKIO)
TARGET_LINK_LIBRARIES(itkIOTests ITKIO)


##Add tests that depend on finding data
FIND_PATH(VW_RAW_DATA_PATH avfCROP.1023c.raw 
          ${ITKTesting_SOURCE_DIR}/../../InsightData/VisibleWomanHead/CROPraw)

IF(VW_RAW_DATA_PATH)
  ADD_TEST(itkRawImageIOTest2 ${IO_TESTS} itkRawImageIOTest2
           ${VW_RAW_DATA_PATH}/avfCROP.1023c.raw)
ENDIF(VW_RAW_DATA_PATH)

IF(ITK_DATA_ROOT)
ADD_TEST(itkMetaImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_BINARY_DIR}/Testing/Temporary/HeadMRVolume.mhd
  itkMetaImageIOTest
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mdh
            ${ITK_BINARY_DIR}/Testing/Temporary/HeadMRVolume.mhd)
ADD_TEST(itkGiplImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.gpil
            ${ITK_BINARY_DIR}/Testing/Temporary/ramp.gpil
  itkGiplImageIOTest 
            ${ITK_DATA_ROOT}/Input/ramp.gpil
            ${ITK_BINARY_DIR}/Testing/Temporary/ramp.gpil)
ADD_TEST(itkDicomImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/itkDicomImageIOTest.png
            ${ITK_BINARY_DIR}/Testing/Temporary/itkDicomImageIOTest.png
  itkDicomImageIOTest
            ${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm
            ${ITK_BINARY_DIR}/Testing/Temporary/itkDicomImageIOTest.png)
  ADD_TEST(itkDicomDifficultFile ${IO_TESTS} itkDicomImageIOTest
           ${ITK_DATA_ROOT}/Input/012345.002.050
           ${ITK_BINARY_DIR}/Testing/Temporary/itkDicomDifficultFile.png)
  ADD_TEST(itkPNGImageIOTest1 ${IO_TESTS} itkPNGImageIOTest
           ${ITK_DATA_ROOT}/Input/cthead1.png)
  ADD_TEST(itkPNGImageIOTest2 ${IO_TESTS} itkPNGImageIOTest
           ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png)
  ADD_TEST(itkAnalyzeIOBigEndianCompressed.hdr ${IO_TESTS} itkAnalyzeImageIOTest
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.hdr)
  ADD_TEST(itkAnalyzeIOBigEndianCompressed.img ${IO_TESTS} itkAnalyzeImageIOTest
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img)
  ADD_TEST(itkAnalyzeIOBigEndianCompressed.img.gz ${IO_TESTS} itkAnalyzeImageIOTest
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img.gz)
  ADD_TEST(itkAnalyzeIOBigEndian ${IO_TESTS} itkAnalyzeImageIOTest
           ${ITK_DATA_ROOT}/Input/BigEndian.hdr)
  ADD_TEST(itkAnalyzeIOLittleEndianCompressed ${IO_TESTS} itkAnalyzeImageIOTest
           ${ITK_DATA_ROOT}/Input/LittleEndianCompressed.hdr)
  ADD_TEST(itkAnalyzeIOLittleEndian ${IO_TESTS} itkAnalyzeImageIOTest
           ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
  ADD_TEST(itkAnalyzeIOInternalTests ${IO_TESTS} itkAnalyzeImageIOTest)
  ADD_TEST(itkAnalyzeIOShouldFail ${IO_TESTS} itkAnalyzeImageIOTest2
                                  false ${ITK_DATA_ROOT}/Input/ramp.gipl)
  ADD_TEST(itkAnalyzeIOShouldSucceed ${IO_TESTS} itkAnalyzeImageIOTest2
                                  true ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
ENDIF(ITK_DATA_ROOT)

IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-IO ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/IO)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

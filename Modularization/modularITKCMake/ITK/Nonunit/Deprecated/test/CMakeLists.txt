itk_module_test()
set(ITK-DeprecatedTests
itk2DDeformableTest.cxx
itkDeformableTest.cxx
itkDICOMImageIO2Test.cxx
itkDicomImageIOTest.cxx
itkDICOMImageSeriesTest.cxx
itkDICOMImageSeriesTest2.cxx
itkDICOMSeriesFileNamesTest.cxx
itkFEMLandmarkLoadImplementationTest.cxx
itkFFTTests.cxx
itkImageReadDICOMSeriesWriteTest.cxx
itkNonThreadedShrinkImageTest.cxx
itkNonUniformBSplineTest.cxx
itkQuaternionOrientationAdapterTest.cxx
itkReflectImageFilterTest.cxx
# itkScalarTest.cxx (itkScalar.h is missing)
itkShiftScaleInPlaceImageFilterTest.cxx
itkTwoOutputExampleImageFilterTest.cxx
#itkVOLImageIOTest.cxx (itkVOLImageIOFactory.h is missing)
)


CreateTestDriver_SupportBuildInIOFactories(ITK-Deprecated  "${ITK-Deprecated-Test_LIBRARIES}" "${ITK-DeprecatedTests}")


create_test_sourcelist(AnalyzeIOTests  AnalyzeIOTestDriver.cxx
        itkAnalyzeImageIODirectionsTest.cxx
        itkAnalyzeImageIOTest.cxx
        EXTRA_INCLUDE itkTestDriverIncludeAnalyzeIOFactory.h
        FUNCTION  ProcessArgumentsAndRegisterAnalyzeIOFactory
)
add_executable(AnalyzeIOTestDriver AnalyzeIOTestDriver.cxx  ${AnalyzeIOTests})
target_link_libraries(AnalyzeIOTestDriver ${ITK-Deprecated-Test_LIBRARIES})

##Analyze Test Suite
add_test(NAME itkAnalyzeIOBigEndianCompressed.hdr
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/BigEndianCompressed.hdr)
add_test(NAME itkAnalyzeIOBigEndianCompressed.img
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img)
add_test(NAME itkAnalyzeIOBigEndianCompressed.img.gz
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img.gz)
add_test(NAME itkAnalyzeIOBigEndian
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/BigEndian.hdr)
add_test(NAME itkAnalyzeIOLittleEndianCompressed
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/LittleEndianCompressed.hdr)
add_test(NAME itkAnalyzeIOLittleEndian
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
add_test(NAME itkAnalyzeIOInternalTests
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR} )
add_test(NAME itkAnalyzeIOShouldFail
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                false ${ITK_DATA_ROOT}/Input/ramp.gipl)
add_test(NAME itkAnalyzeIOShouldSucceed
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                true ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
add_test(NAME itkAnalyzeIOShouldSucceedZ
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                true ${ITK_DATA_ROOT}/Input/LittleEndianZ.hdr)
add_test(NAME itkAnalyzeImageIODirectionsTest
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIODirectionsTest
                                ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkAnalyzeImageIOBadHeader
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIOBadHeader
                                ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkAnalyzeImageIORGBImageTest
      COMMAND AnalyzeIOTestDriver itkAnalyzeImageIORGBImageTest
                                       ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkAnalyzeDimensionLimitsTest
      COMMAND AnalyzeIOTestDriver itkNiftiImageIOTest11
                                ${ITK_TEST_OUTPUT_DIR} SizeFailure.img.gz )

add_test(NAME itk2DDeformableTest
      COMMAND ITK-DeprecatedTestDriver itk2DDeformableTest)
add_test(NAME itkDeformableTest
      COMMAND ITK-DeprecatedTestDriver itkDeformableTest)
add_test(NAME itkNonThreadedShrinkImageTest
      COMMAND ITK-DeprecatedTestDriver itkNonThreadedShrinkImageTest)
add_test(NAME itkNonUniformBSplineTest
      COMMAND ITK-DeprecatedTestDriver itkNonUniformBSplineTest)
add_test(NAME itkReflectImageFilterTest
      COMMAND ITK-DeprecatedTestDriver itkReflectImageFilterTest)
add_test(NAME itkShiftScaleInPlaceImageFilterTest
      COMMAND ITK-DeprecatedTestDriver itkShiftScaleInPlaceImageFilterTest)
add_test(NAME itkTwoOutputExampleImageFilterTest
      COMMAND ITK-DeprecatedTestDriver itkTwoOutputExampleImageFilterTest)

add_test(NAME itkDICOMImageSeriesTest
     COMMAND ITK-DeprecatedTestDriver itkDICOMImageSeriesTest
              ${ITK_DATA_ROOT}/Input/DicomSeries 0)
add_test(NAME itkDICOMImageSeriesTest2
       COMMAND ITK-DeprecatedTestDriver  itkDICOMImageSeriesTest2
            ${ITK_DATA_ROOT}/Input/DicomSeries )
add_test(NAME itkDICOMSeriesFileNamesTest
       COMMAND ITK-DeprecatedTestDriver itkDICOMSeriesFileNamesTest ${ITK_DATA_ROOT}/Input/DicomSeries)
add_test(NAME itkDicomDifficultFile
     COMMAND ITK-DeprecatedTestDriver
    itkDicomImageIOTest ${ITK_DATA_ROOT}/Input/012345.002.050
         ${ITK_TEST_OUTPUT_DIR}/itkDicomDifficultFile.png)
add_test(NAME itkImageReadDICOMSeriesWriteTest
     COMMAND ITK-DeprecatedTestDriver
      itkImageReadDICOMSeriesWriteTest ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha
  ${ITK_TEST_OUTPUT_DIR}/DICOMSeriesOutputHeadMRVolume)
add_test(NAME itkImageReadDICOMSeriesWriteWithDirection002Test
       COMMAND ITK-DeprecatedTestDriver
      itkImageReadDICOMSeriesWriteTest ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection002.mhd
  ${ITK_TEST_OUTPUT_DIR}/DICOMSeriesOutputHeadMRVolumeWithDirection002)
add_test(NAME itkImageReadDICOMSeriesWriteWithDirection003Test
       COMMAND ITK-DeprecatedTestDriver
      itkImageReadDICOMSeriesWriteTest ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection003.mhd
  ${ITK_TEST_OUTPUT_DIR}/DICOMSeriesOutputHeadMRVolumeWithDirection003)

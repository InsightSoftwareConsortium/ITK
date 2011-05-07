itk_module_test()
set(ITK-DeprecatedTests
itk2DDeformableTest.cxx
itkDeformableTest.cxx
itkNonThreadedShrinkImageTest.cxx
itkNonUniformBSplineTest.cxx
itkReflectImageFilterTest.cxx
itkShiftScaleInPlaceImageFilterTest.cxx
itkTwoOutputExampleImageFilterTest.cxx
itkGradientToMagnitudeImageFilterTest.cxx
)


CreateTestDriver_SupportBuildInIOFactories(ITK-Deprecated  "${ITK-Deprecated-Test_LIBRARIES}" "${ITK-DeprecatedTests}")

itk_add_test(NAME itk2DDeformableTest
      COMMAND ITK-DeprecatedTestDriver itk2DDeformableTest)
itk_add_test(NAME itkDeformableTest
      COMMAND ITK-DeprecatedTestDriver itkDeformableTest)
itk_add_test(NAME itkNonThreadedShrinkImageTest
      COMMAND ITK-DeprecatedTestDriver itkNonThreadedShrinkImageTest)
itk_add_test(NAME itkNonUniformBSplineTest
      COMMAND ITK-DeprecatedTestDriver itkNonUniformBSplineTest)
itk_add_test(NAME itkReflectImageFilterTest
      COMMAND ITK-DeprecatedTestDriver itkReflectImageFilterTest)
itk_add_test(NAME itkShiftScaleInPlaceImageFilterTest
      COMMAND ITK-DeprecatedTestDriver itkShiftScaleInPlaceImageFilterTest)
itk_add_test(NAME itkTwoOutputExampleImageFilterTest
      COMMAND ITK-DeprecatedTestDriver itkTwoOutputExampleImageFilterTest)
itk_add_test(NAME itkGradientToMagnitudeImageFilterTest
      COMMAND ITK-DeprecatedTestDriver itkGradientToMagnitudeImageFilterTest)

create_test_sourcelist(DeprecatedIOTests  DeprecatedIOTestDriver.cxx
        itkDICOMImageIO2Test.cxx
        itkDicomImageIOTest.cxx
        itkDicomImageIODirection2DTest.cxx
        itkDICOMImageSeriesTest.cxx
        itkDICOMImageSeriesTest2.cxx
        itkDICOMSeriesFileNamesTest.cxx
        itkImageReadDICOMSeriesWriteTest.cxx
        itkAnalyzeImageIODirectionsTest.cxx
        itkAnalyzeImageIOTest.cxx
        itkAnalyzeImageIOTest2
        itkAnalyzeImageIOBadHeader
        itkAnalyzeImageIORGBImageTest

        itkAnalyzeImageIODirectionsTest

        EXTRA_INCLUDE itkTestDriverIncludeDeprecatedIOFactories.h
        FUNCTION  ProcessArgumentsAndRegisterDeprecatedIOFactories
)
add_library(ITK-AnalyzeIO-TestHelper itkAnalyzeImageIOTestHelper.cxx)
itk_module_target_label(ITK-AnalyzeIO-TestHelper)
target_link_libraries(ITK-AnalyzeIO-TestHelper  ${ITK-Deprecated_LIBRARIES})

list(APPEND ITK-Deprecated-Test_LIBRARIES ITK-AnalyzeIO-TestHelper)
add_executable(DeprecatedIOTestDriver DeprecatedIOTestDriver.cxx  ${DeprecatedIOTests})
itk_module_target_label(DeprecatedIOTestDriver)
target_link_libraries(DeprecatedIOTestDriver ${ITK-Deprecated-Test_LIBRARIES})


##Analyze Test Suite
itk_add_test(NAME itkAnalyzeIOBigEndianCompressed.hdr
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/BigEndianCompressed.hdr)
itk_add_test(NAME itkAnalyzeIOBigEndianCompressed.img
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img)
itk_add_test(NAME itkAnalyzeIOBigEndianCompressed.img.gz
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img.gz)
itk_add_test(NAME itkAnalyzeIOBigEndian
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/BigEndian.hdr)
itk_add_test(NAME itkAnalyzeIOLittleEndianCompressed
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/LittleEndianCompressed.hdr)
itk_add_test(NAME itkAnalyzeIOLittleEndian
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
         ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
itk_add_test(NAME itkAnalyzeIOInternalTests
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR} )
itk_add_test(NAME itkAnalyzeIOShouldFail
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                false ${ITK_DATA_ROOT}/Input/ramp.gipl)
itk_add_test(NAME itkAnalyzeIOShouldSucceed
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                true ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
itk_add_test(NAME itkAnalyzeIOShouldSucceedZ
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                true ${ITK_DATA_ROOT}/Input/LittleEndianZ.hdr)
itk_add_test(NAME itkAnalyzeImageIODirectionsTest
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIODirectionsTest
                                ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkAnalyzeImageIOBadHeader
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIOBadHeader
                                ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkAnalyzeImageIORGBImageTest
      COMMAND DeprecatedIOTestDriver itkAnalyzeImageIORGBImageTest
                                       ${ITK_TEST_OUTPUT_DIR})
# DICOM
itk_add_test(NAME itkDICOMImageSeriesTest
     COMMAND DeprecatedIOTestDriver itkDICOMImageSeriesTest
              ${ITK_DATA_ROOT}/Input/DicomSeries 0)
itk_add_test(NAME itkDICOMImageSeriesTest2
       COMMAND DeprecatedIOTestDriver  itkDICOMImageSeriesTest2
            ${ITK_DATA_ROOT}/Input/DicomSeries )
itk_add_test(NAME itkDICOMSeriesFileNamesTest
       COMMAND DeprecatedIOTestDriver itkDICOMSeriesFileNamesTest ${ITK_DATA_ROOT}/Input/DicomSeries)
itk_add_test(NAME itkDicomDifficultFile
     COMMAND DeprecatedIOTestDriver
    itkDicomImageIOTest ${ITK_DATA_ROOT}/Input/012345.002.050
         ${ITK_TEST_OUTPUT_DIR}/itkDicomDifficultFile.png)
itk_add_test(NAME itkDicomImageIODirection2DTest
      COMMAND DeprecatedIOTestDriver itkDicomImageIODirection2DTest
              ${ITK_TEST_OUTPUT_DIR}/itkDicomImage3DIODirection2DTest.dcm ${ITK_TEST_OUTPUT_DIR}/itkDicomImage3DIODirection2DTest.mhd ${ITK_TEST_OUTPUT_DIR}/itkDicomImage2DIODirection2DTest.mhd)
itk_add_test(NAME itkImageReadDICOMSeriesWriteTest
     COMMAND DeprecatedIOTestDriver
      itkImageReadDICOMSeriesWriteTest ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha
  ${ITK_TEST_OUTPUT_DIR}/DICOMSeriesOutputHeadMRVolume)
itk_add_test(NAME itkImageReadDICOMSeriesWriteWithDirection002Test
       COMMAND DeprecatedIOTestDriver
      itkImageReadDICOMSeriesWriteTest ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection002.mhd
  ${ITK_TEST_OUTPUT_DIR}/DICOMSeriesOutputHeadMRVolumeWithDirection002)
itk_add_test(NAME itkImageReadDICOMSeriesWriteWithDirection003Test
       COMMAND DeprecatedIOTestDriver
      itkImageReadDICOMSeriesWriteTest ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection003.mhd
  ${ITK_TEST_OUTPUT_DIR}/DICOMSeriesOutputHeadMRVolumeWithDirection003)

itk_module_test()
set(ITKDeprecatedTests
itk2DDeformableTest.cxx
itkDeformableTest.cxx
itkNonThreadedShrinkImageTest.cxx
itkNonUniformBSplineTest.cxx
itkReflectImageFilterTest.cxx
itkShiftScaleInPlaceImageFilterTest.cxx
itkTwoOutputExampleImageFilterTest.cxx
itkGradientToMagnitudeImageFilterTest.cxx
itkAddConstantToImageFilterTest.cxx
itkDivideByConstantImageFilterTest.cxx
itkMultiplyByConstantImageFilterTest.cxx
itkSubtractConstantFromImageFilterTest.cxx
)


CreateTestDriver_SupportBuildInIOFactories(ITKDeprecated  "${ITKDeprecated-Test_LIBRARIES}" "${ITKDeprecatedTests}")

itk_add_test(NAME itk2DDeformableTest
      COMMAND ITKDeprecatedTestDriver itk2DDeformableTest)
itk_add_test(NAME itkDeformableTest
      COMMAND ITKDeprecatedTestDriver itkDeformableTest)
itk_add_test(NAME itkNonThreadedShrinkImageTest
      COMMAND ITKDeprecatedTestDriver itkNonThreadedShrinkImageTest)
itk_add_test(NAME itkNonUniformBSplineTest
      COMMAND ITKDeprecatedTestDriver itkNonUniformBSplineTest)
itk_add_test(NAME itkReflectImageFilterTest
      COMMAND ITKDeprecatedTestDriver itkReflectImageFilterTest)
itk_add_test(NAME itkShiftScaleInPlaceImageFilterTest
      COMMAND ITKDeprecatedTestDriver itkShiftScaleInPlaceImageFilterTest)
itk_add_test(NAME itkTwoOutputExampleImageFilterTest
      COMMAND ITKDeprecatedTestDriver itkTwoOutputExampleImageFilterTest)
itk_add_test(NAME itkGradientToMagnitudeImageFilterTest
      COMMAND ITKDeprecatedTestDriver itkGradientToMagnitudeImageFilterTest)

itk_add_test(NAME DeprecatedAddConstantToImageFilterTest
  COMMAND ITKDeprecatedTestDriver itkAddConstantToImageFilterTest)
itk_add_test(NAME DeprecatedSubtractConstantFromaImageFilterTest
  COMMAND ITKDeprecatedTestDriver itkSubtractConstantFromImageFilterTest)
itk_add_test(NAME DeprecatedMultiplyByConstantImageFilterTest
  COMMAND ITKDeprecatedTestDriver itkMultiplyByConstantImageFilterTest)
itk_add_test(NAME DeprecatedDivideByConstantImageFilterTest
  COMMAND ITKDeprecatedTestDriver itkDivideByConstantImageFilterTest)

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
add_library(ITKAnalyzeIO-TestHelper itkAnalyzeImageIOTestHelper.cxx)
itk_module_target_label(ITKAnalyzeIO-TestHelper)
target_link_libraries(ITKAnalyzeIO-TestHelper  ${ITKDeprecated_LIBRARIES})

list(APPEND ITKDeprecated-Test_LIBRARIES ITKAnalyzeIO-TestHelper)
add_executable(DeprecatedIOTestDriver DeprecatedIOTestDriver.cxx  ${DeprecatedIOTests})
itk_module_target_label(DeprecatedIOTestDriver)
target_link_libraries(DeprecatedIOTestDriver ${ITKDeprecated-Test_LIBRARIES})


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

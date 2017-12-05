itk_module_test()
set(ITKIONIFTITests
itkNiftiImageIOTest.cxx
itkNiftiImageIOTest2.cxx
itkNiftiImageIOTest3.cxx
itkNiftiImageIOTest4.cxx
itkNiftiImageIOTest5.cxx
itkNiftiImageIOTest6.cxx
itkNiftiImageIOTest7.cxx
itkNiftiImageIOTest8.cxx
itkNiftiImageIOTest9.cxx
itkNiftiImageIOTest10.cxx
itkNiftiImageIOTest11.cxx
itkNiftiImageIOTest12.cxx
itkNiftiReadAnalyzeTest.cxx
itkExtractSlice.cxx
)

# For itkNiftiImageIOTest.h.
include_directories( ${ITKIONIFTI_SOURCE_DIR}/test )

add_library(ITKIONIFTI-TestSupport  itkNiftiImageIOTestHelper.cxx)
itk_module_target_label(ITKIONIFTI-TestSupport)
target_link_libraries(ITKIONIFTI-TestSupport LINK_PUBLIC ${ITKIONIFTI-Test_LIBRARIES})

list(APPEND ITKIONIFTI-Test_LIBRARIES ITKIONIFTI-TestSupport)

CreateTestDriver(ITKIONIFTI  "${ITKIONIFTI-Test_LIBRARIES}" "${ITKIONIFTITests}")

itk_add_test(NAME itkNiftiIOBigEndianCompressed.hdr
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndianCompressed.hdr DATA{${ITK_DATA_ROOT}/Input/BigEndianCompressed.hdr})
itk_add_test(NAME itkNiftiIOBigEndianCompressed.img
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndianCompressed.img DATA{${ITK_DATA_ROOT}/Input/BigEndianCompressed.img})
itk_add_test(NAME itkNiftiIOBigEndianCompressed.img.gz
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndianCompressed.img.gz DATA{${ITK_DATA_ROOT}/Input/BigEndianCompressed.img.gz})
itk_add_test(NAME itkNiftiIOBigEndian
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndian DATA{${ITK_DATA_ROOT}/Input/BigEndian.hdr,BigEndian.mhd,BigEndian.img})
itk_add_test(NAME itkNiftiIOLittleEndianCompressed
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiImageIOTest DATA{${ITK_DATA_ROOT}/Input/LittleEndianCompressed.hdr,LittleEndianCompressed.img.gz})
itk_add_test(NAME itkNiftiIOLittleEndian
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOLittleEndian DATA{${ITK_DATA_ROOT}/Input/LittleEndian.hdr,LittleEndian.mhd,LittleEndian.img})
itk_add_test(NAME itkNiftiIOInternalTests
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkNiftiIOShouldFail
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest2
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOShouldFail false DATA{${ITK_DATA_ROOT}/Input/ramp.gipl})
itk_add_test(NAME itkNiftiIOShouldSucceed
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest2
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOShouldSucceed true DATA{${ITK_DATA_ROOT}/Input/LittleEndian.hdr,LittleEndian.mhd,LittleEndian.img})
itk_add_test(NAME itkDirCosinesTest
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest4 ${ITK_TEST_OUTPUT_DIR} )
itk_add_test(NAME itkNiftiSlopeInterceptTest
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest5 ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkNiftiVectorImageTest2
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest6 ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkNiftiDiffusionTensor3dImageTest
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest7 ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkNiftiSymmetricSecondRankTensorImageTest
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest8 ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkNiftiRGBImageTest
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest9 ${ITK_TEST_OUTPUT_DIR} RGBImage.nii.gz)
itk_add_test(NAME itkNiftiRGBAImageTest
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest10 ${ITK_TEST_OUTPUT_DIR} RGBAImage.nii.gz)
itk_add_test(NAME itkNiftiVectorImageTest
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest3 ${ITK_TEST_OUTPUT_DIR} )
itk_add_test(NAME itkNiftiDimensionLimitsTest
      COMMAND ITKIONIFTITestDriver itkNiftiImageIOTest11 ${ITK_TEST_OUTPUT_DIR} SizeFailure.nii.gz )
itk_add_test(NAME itkNiftiReadAnalyzeTest
      COMMAND ITKIONIFTITestDriver itkNiftiReadAnalyzeTest ${ITK_TEST_OUTPUT_DIR} )
itk_add_test(NAME itkExtractSliceSlopeInterceptUCHAR
      COMMAND ITKIONIFTITestDriver --compare DATA{Baseline/SlopeInterceptUCHAR-midSlice.nrrd} ${ITK_TEST_OUTPUT_DIR}/SlopeInterceptUCHAR-midSlice.nrrd
              itkExtractSlice DATA{Input/SlopeInterceptUCHAR.nii.gz} ${ITK_TEST_OUTPUT_DIR}/SlopeInterceptUCHAR-midSlice.nrrd)

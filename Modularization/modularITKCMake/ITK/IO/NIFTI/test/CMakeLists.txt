itk_module_test()
set(ITK-IO-NIFTITests
itkNiftiImageIOTest.cxx
itkIONIFTIHeaderTest.cxx
itkNiftiImageIOTest2.cxx
)

CreateTestDriver(ITK-IO-NIFTI  "${ITK-IO-NIFTI-Test_LIBRARIES}" "${ITK-IO-NIFTITests}")

add_test(NAME itkIONIFTIHeaderTest
      COMMAND ITK-IO-NIFTITestDriver itkIONIFTIHeaderTest)
add_test(NAME itkNiftiIOBigEndianCompressed.hdr
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndianCompressed.hdr ${ITK_DATA_ROOT}/Input/BigEndianCompressed.hdr)
add_test(NAME itkNiftiIOBigEndianCompressed.img
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndianCompressed.img ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img)
add_test(NAME itkNiftiIOBigEndianCompressed.img.gz
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndianCompressed.img.gz ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img.gz)
add_test(NAME itkNiftiIOBigEndian
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndian ${ITK_DATA_ROOT}/Input/BigEndian.hdr)
add_test(NAME itkNiftiIOLittleEndianCompressed
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiImageIOTest ${ITK_DATA_ROOT}/Input/LittleEndianCompressed.hdr)
add_test(NAME itkNiftiIOLittleEndian
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOLittleEndian ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
add_test(NAME itkNiftiIOInternalTests
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkNiftiIOShouldFail
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest2
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOShouldFail false ${ITK_DATA_ROOT}/Input/ramp.gipl)
add_test(NAME itkNiftiIOShouldSucceed
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest2
              ${ITK_TEST_OUTPUT_DIR} itkNiftiIOShouldSucceed true ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
add_test(NAME itkDirCosinesTest
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest4 ${ITK_TEST_OUTPUT_DIR} )
add_test(NAME itkNiftiSlopeInterceptTest
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest5 ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkNiftiVectorImageTest2
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest6 ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkNiftiDiffusionTensor3dImageTest
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest7 ${ITK_TEST_OUTPUT_DIR})
add_test(itkNiftiSymmetricSecondRankTensorImageTest
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest8 ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkNiftiRGBImageTest
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest9 ${ITK_TEST_OUTPUT_DIR} RGBImage.nii.gz)
add_test(NAME itkNiftiRGBAImageTest
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest10 ${ITK_TEST_OUTPUT_DIR} RGBAImage.nii.gz)
add_test(NAME itkNiftiVectorImageTest
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest3 ${ITK_TEST_OUTPUT_DIR} )
add_test(NAME itkNiftiDimensionLimitsTest
      COMMAND ITK-IO-NIFTITestDriver itkNiftiImageIOTest11 ${ITK_TEST_OUTPUT_DIR} SizeFailure.nii.gz )

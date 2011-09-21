itk_module_test()
set(ITKIORAWTests
itkRawImageIOTest.cxx
itkRawImageIOTest2.cxx
itkRawImageIOTest3.cxx
itkRawImageIOTest4.cxx
itkRawImageIOTest5.cxx
)

CreateTestDriver(ITKIORAW  "${ITKIORAW-Test_LIBRARIES}" "${ITKIORAWTests}")

itk_add_test(NAME itkRawImageIOTest
      COMMAND ITKIORAWTestDriver itkRawImageIOTest
              ${ITK_TEST_OUTPUT_DIR}/raw1.raw ${ITK_TEST_OUTPUT_DIR}/raw2.raw)
itk_add_test(NAME itkRawImageIOTest2
      COMMAND ITKIORAWTestDriver itkRawImageIOTest2
              DATA{${ITK_DATA_ROOT}/Input/VHFColor.raw})
itk_add_test(NAME itkRawImageIOTest3
      COMMAND ITKIORAWTestDriver itkRawImageIOTest3
              ${ITK_TEST_OUTPUT_DIR}/raw3.raw ${ITK_TEST_OUTPUT_DIR}/raw4.raw)
itk_add_test(NAME itkRawImageIOTest4
      COMMAND ITKIORAWTestDriver itkRawImageIOTest4
              ${ITK_TEST_OUTPUT_DIR}/bigendian.raw ${ITK_TEST_OUTPUT_DIR}/littleendian.raw)
itk_add_test(NAME itkRawImageIOTest5
      COMMAND ITKIORAWTestDriver itkRawImageIOTest5
              ${ITK_TEST_OUTPUT_DIR})

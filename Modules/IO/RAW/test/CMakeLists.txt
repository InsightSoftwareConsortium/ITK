itk_module_test()
set(ITK-IO-RAWTests
itkRawImageIOTest.cxx
itkRawImageIOTest2.cxx
itkRawImageIOTest3.cxx
itkRawImageIOTest4.cxx
itkRawImageIOTest5.cxx
itkIORAWHeaderTest.cxx
)

CreateTestDriver(ITK-IO-RAW  "${ITK-IO-RAW-Test_LIBRARIES}" "${ITK-IO-RAWTests}")

itk_add_test(NAME itkIORAWHeaderTest
      COMMAND ITK-IO-RAWTestDriver itkIORAWHeaderTest)
itk_add_test(NAME itkRawImageIOTest
      COMMAND ITK-IO-RAWTestDriver itkRawImageIOTest
              ${ITK_TEST_OUTPUT_DIR}/raw1.raw ${ITK_TEST_OUTPUT_DIR}/raw2.raw)
itk_add_test(NAME itkRawImageIOTest2
      COMMAND ITK-IO-RAWTestDriver itkRawImageIOTest2
              ${ITK_DATA_ROOT}/Input/VHFColor.raw)
itk_add_test(NAME itkRawImageIOTest3
      COMMAND ITK-IO-RAWTestDriver itkRawImageIOTest3
              ${ITK_TEST_OUTPUT_DIR}/raw3.raw ${ITK_TEST_OUTPUT_DIR}/raw4.raw)
itk_add_test(NAME itkRawImageIOTest4
      COMMAND ITK-IO-RAWTestDriver itkRawImageIOTest4
              ${ITK_TEST_OUTPUT_DIR}/bigendian.raw ${ITK_TEST_OUTPUT_DIR}/littleendian.raw)
itk_add_test(NAME itkRawImageIOTest5
      COMMAND ITK-IO-RAWTestDriver itkRawImageIOTest5
              ${ITK_TEST_OUTPUT_DIR})

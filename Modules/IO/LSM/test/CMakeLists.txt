itk_module_test()
set(ITK-IO-LSMTests
itkIOLSMHeaderTest.cxx
itkLSMImageIOTest.cxx
)

CreateTestDriver(ITK-IO-LSM  "${ITK-IO-LSM-Test_LIBRARIES}" "${ITK-IO-LSMTests}")

itk_add_test(NAME itkIOLSMHeaderTest
      COMMAND ITK-IO-LSMTestDriver itkIOLSMHeaderTest)
itk_add_test(NAME itkLSMImageIOTest
      COMMAND ITK-IO-LSMTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif
              ${ITK_TEST_OUTPUT_DIR}/cthead1.tif
    itkLSMImageIOTest ${ITK_DATA_ROOT}/Input/cthead1.lsm ${ITK_TEST_OUTPUT_DIR}/cthead1.tif)

itk_module_test()
set(ITKIOLSMTests
itkLSMImageIOTest.cxx
)

CreateTestDriver(ITKIOLSM  "${ITKIOLSM-Test_LIBRARIES}" "${ITKIOLSMTests}")

itk_add_test(NAME itkLSMImageIOTest
      COMMAND ITKIOLSMTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif}
              ${ITK_TEST_OUTPUT_DIR}/cthead1.tif
    itkLSMImageIOTest DATA{${ITK_DATA_ROOT}/Input/cthead1.lsm} ${ITK_TEST_OUTPUT_DIR}/cthead1.tif)

itk_module_test()
set(ITKIOPNGTests
itkPNGImageIOTest.cxx
)

CreateTestDriver(ITKIOPNG  "${ITKIOPNG-Test_LIBRARIES}" "${ITKIOPNGTests}")

itk_add_test(NAME itkPNGImageIOTest1
      COMMAND ITKIOPNGTestDriver itkPNGImageIOTest
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest1.png)
itk_add_test(NAME itkPNGImageIOTest2
      COMMAND ITKIOPNGTestDriver itkPNGImageIOTest
              ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest2.png)

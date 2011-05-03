itk_module_test()
set(ITK-IO-PNGTests
itkPNGImageIOTest.cxx
itkIOPNGHeaderTest.cxx
)

CreateTestDriver(ITK-IO-PNG  "${ITK-IO-PNG-Test_LIBRARIES}" "${ITK-IO-PNGTests}")

itk_add_test(NAME itkIOPNGHeaderTest
      COMMAND ITK-IO-PNGTestDriver itkIOPNGHeaderTest)
itk_add_test(NAME itkPNGImageIOTest1
      COMMAND ITK-IO-PNGTestDriver itkPNGImageIOTest
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest1.png)
itk_add_test(NAME itkPNGImageIOTest2
      COMMAND ITK-IO-PNGTestDriver itkPNGImageIOTest
              ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest2.png)

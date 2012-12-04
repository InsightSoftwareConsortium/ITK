itk_module_test()
set(ITKIOPNGTests
itkPNGImageIOTest.cxx
itkPNGImageIOTest2.cxx
)

CreateTestDriver(ITKIOPNG  "${ITKIOPNG-Test_LIBRARIES}" "${ITKIOPNGTests}")

itk_add_test(NAME itkPNGImageIOTest1
      COMMAND ITKIOPNGTestDriver itkPNGImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest1.png)
itk_add_test(NAME itkPNGImageIOTest2
      COMMAND ITKIOPNGTestDriver itkPNGImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest2.png)
itk_add_test(NAME itkPNGImageIOTest3
      COMMAND ITKIOPNGTestDriver
    --compare DATA{Baseline/itkPNGImageIOTest3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3.png
         itkPNGImageIOTest2
           DATA{Input/GrayAlpha.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3.png
)

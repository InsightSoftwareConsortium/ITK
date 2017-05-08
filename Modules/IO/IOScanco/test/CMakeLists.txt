itk_module_test()
set(IOScancoTests itkScancoImageIOTest.cxx)

createtestdriver(IOScanco  "${IOScanco-Test_LIBRARIES}" "${IOScancoTests}")

itk_add_test(
  NAME itkScancoImageIOTest
  COMMAND
    IOScancoTestDriver
    itkScancoImageIOTest
    /home/matt/data/BPBPHumerus/00004180-phantom-2014.0620/C0003665.ISQ
    ${ITK_TEST_OUTPUT_DIR}/C0003665.mha
)

itk_module_test()
set(IOScancoTests itkScancoImageIOTest.cxx)

createtestdriver(IOScanco  "${IOScanco-Test_LIBRARIES}" "${IOScancoTests}")

itk_add_test(
  NAME itkScancoImageIOTest
  COMMAND
    IOScancoTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd,HeadMRVolume.raw}
    ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mhd
    itkScancoImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw}
    ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mhd
)

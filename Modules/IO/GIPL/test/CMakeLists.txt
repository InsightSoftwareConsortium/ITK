itk_module_test()
set(ITKIOGIPLTests
itkGiplImageIOTest.cxx
)

CreateTestDriver(ITKIOGIPL  "${ITKIOGIPL-Test_LIBRARIES}" "${ITKIOGIPLTests}")

itk_add_test(NAME itkGiplImageIOTest
      COMMAND ITKIOGIPLTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd
              ${ITK_TEST_OUTPUT_DIR}/ramp.mhd
    itkGiplImageIOTest ${ITK_DATA_ROOT}/Input/ramp.gipl ${ITK_TEST_OUTPUT_DIR}/ramp.mhd)
itk_add_test(NAME itkGiplImageIOGzTest
      COMMAND ITKIOGIPLTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd
              ${ITK_TEST_OUTPUT_DIR}/ramp-gz.mhd
    itkGiplImageIOTest ${ITK_DATA_ROOT}/Input/ramp.gipl.gz ${ITK_TEST_OUTPUT_DIR}/ramp-gz.mhd)
itk_add_test(NAME itkGiplImageIOTest2
      COMMAND ITKIOGIPLTestDriver itkGiplImageIOTest
              ${ITK_DATA_ROOT}/Input/ramp.gipl ${ITK_TEST_OUTPUT_DIR}/ramp.gipl)

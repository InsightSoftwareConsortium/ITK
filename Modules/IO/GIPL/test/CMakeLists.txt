itk_module_test()
set(ITKIOGIPLTests
itkGiplImageIOTest.cxx
)

CreateTestDriver(ITKIOGIPL  "${ITKIOGIPL-Test_LIBRARIES}" "${ITKIOGIPLTests}")

itk_add_test(NAME itkGiplImageIOTest
      COMMAND ITKIOGIPLTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd,ramp.raw}
              ${ITK_TEST_OUTPUT_DIR}/ramp.mhd
    itkGiplImageIOTest DATA{${ITK_DATA_ROOT}/Input/ramp.gipl} ${ITK_TEST_OUTPUT_DIR}/ramp.mhd)
itk_add_test(NAME itkGiplImageIOGzTest
      COMMAND ITKIOGIPLTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd,ramp.raw}
              ${ITK_TEST_OUTPUT_DIR}/ramp-gz.mhd
    itkGiplImageIOTest DATA{${ITK_DATA_ROOT}/Input/ramp.gipl.gz} ${ITK_TEST_OUTPUT_DIR}/ramp-gz.mhd)
itk_add_test(NAME itkGiplImageIOTest2
      COMMAND ITKIOGIPLTestDriver itkGiplImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/ramp.gipl} ${ITK_TEST_OUTPUT_DIR}/ramp.gipl)

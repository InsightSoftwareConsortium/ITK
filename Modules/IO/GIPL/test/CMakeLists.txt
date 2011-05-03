itk_module_test()
set(ITK-IO-GIPLTests
itkIOGIPLHeaderTest.cxx
itkGiplImageIOTest.cxx
)

CreateTestDriver(ITK-IO-GIPL  "${ITK-IO-GIPL-Test_LIBRARIES}" "${ITK-IO-GIPLTests}")

itk_add_test(NAME itkIOGIPLHeaderTest
      COMMAND ITK-IO-GIPLTestDriver itkIOGIPLHeaderTest)
itk_add_test(NAME itkGiplImageIOTest
      COMMAND ITK-IO-GIPLTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd
              ${ITK_TEST_OUTPUT_DIR}/ramp.mhd
    itkGiplImageIOTest ${ITK_DATA_ROOT}/Input/ramp.gipl ${ITK_TEST_OUTPUT_DIR}/ramp.mhd)
itk_add_test(NAME itkGiplImageIOGzTest
      COMMAND ITK-IO-GIPLTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd
              ${ITK_TEST_OUTPUT_DIR}/ramp-gz.mhd
    itkGiplImageIOTest ${ITK_DATA_ROOT}/Input/ramp.gipl.gz ${ITK_TEST_OUTPUT_DIR}/ramp-gz.mhd)
itk_add_test(NAME itkGiplImageIOTest2
      COMMAND ITK-IO-GIPLTestDriver itkGiplImageIOTest
              ${ITK_DATA_ROOT}/Input/ramp.gipl ${ITK_TEST_OUTPUT_DIR}/ramp.gipl)

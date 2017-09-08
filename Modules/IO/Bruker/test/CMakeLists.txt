itk_module_test()
set(ITKIOBrukerTests
    itkBrukerImageTest.cxx
)

CreateTestDriver(ITKIOBruker "${ITKIOBruker-Test_LIBRARIES}" "${ITKIOBrukerTests}")

ExternalData_Expand_Arguments( ITKData
    ExpandedData # Ignore output variable
    DATA{Input/,RECURSE:,REGEX:.*} )

itk_add_test( NAME itkBruker2dseq_PV6.0_INT16
  COMMAND ITKIOBrukerTestDriver
  --compare
    ${ITK_TEST_OUTPUT_DIR}/PV6.0_FLASH_INT16.mha
    DATA{Baseline/PV6.0_FLASH_INT16.mha}
  itkBrukerImageTest
    DATA{Input/PV6.0_FLASH/pdata/1/2dseq}
    ${ITK_TEST_OUTPUT_DIR}/PV6.0_FLASH_INT16.mha )

itk_add_test( NAME itkBruker2dseq_PV6.0_UINT8
  COMMAND ITKIOBrukerTestDriver
  --compare
    ${ITK_TEST_OUTPUT_DIR}/PV6.0_FLASH_UINT8.mha
    DATA{Baseline/PV6.0_FLASH_UINT8.mha}
  itkBrukerImageTest
    DATA{Input/PV6.0_FLASH/pdata/2/2dseq}
    ${ITK_TEST_OUTPUT_DIR}/PV6.0_FLASH_UINT8.mha )

itk_add_test( NAME itkBruker2dseq_PV6.0_FLOAT32
  COMMAND ITKIOBrukerTestDriver
  --compare
    ${ITK_TEST_OUTPUT_DIR}/PV6.0_FLASH_FLOAT32.mha
    DATA{Baseline/PV6.0_FLASH_FLOAT32.mha}
  itkBrukerImageTest
    DATA{Input/PV6.0_FLASH/pdata/2/2dseq}
    ${ITK_TEST_OUTPUT_DIR}/PV6.0_FLASH_FLOAT32.mha )

itk_add_test( NAME itkBruker2dseq_PV5.1_FSE_INT16
  COMMAND ITKIOBrukerTestDriver
  --compare
    ${ITK_TEST_OUTPUT_DIR}/PV5.1_FSE_INT16.mha
    DATA{Baseline/PV5.1_FSE_INT16.mha}
  itkBrukerImageTest
    DATA{Input/PV5.1_FSE/pdata/1/2dseq}
    ${ITK_TEST_OUTPUT_DIR}/PV5.1_FSE_INT16.mha )

itk_module_test()

set(GrowCutTests
  itkFastGrowCutTest.cxx
  )

CreateTestDriver(GrowCut "${GrowCut-Test_LIBRARIES}" "${GrowCutTests}")

itk_add_test(NAME itkFastGrowCutTest
  COMMAND GrowCutTestDriver
    --compare
      DATA{Baseline/NoisyResult.seg.nrrd}
      ${ITK_TEST_OUTPUT_DIR}/itkFastGrowCutTestNoisy-label.nrrd
    --compare
      DATA{Baseline/MedianResult.seg.nrrd}
      ${ITK_TEST_OUTPUT_DIR}/itkFastGrowCutTestMedian-label.nrrd
  itkFastGrowCutTest
    DATA{Input/C0004255.mha}
    DATA{Input/Seeds.seg.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/itkFastGrowCutTestNoisy-label.nrrd
    ${ITK_TEST_OUTPUT_DIR}/itkFastGrowCutTestMedian-label.nrrd
  )

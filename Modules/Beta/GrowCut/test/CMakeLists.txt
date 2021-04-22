itk_module_test()

set(GrowCutTests
  itkFastGrowCutTest.cxx
  )

CreateTestDriver(GrowCut "${GrowCut-Test_LIBRARIES}" "${GrowCutTests}")

itk_add_test(NAME itkFastGrowCutTestDzZ_L1
  COMMAND GrowCutTestDriver
    --compare
      DATA{Baseline/DzZ_L1-label.nrrd}
      ${ITK_TEST_OUTPUT_DIR}/itkFastGrowCutTestDzZ_L1-label.nrrd
  itkFastGrowCutTest
    DATA{Input/DzZ_L1.nrrd}
    DATA{Input/DzZ_L1_seeds-label.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/itkFastGrowCutTestDzZ_L1-label.nrrd
  )

itk_add_test(NAME itkFastGrowCutTestDzZ_T1
  COMMAND GrowCutTestDriver
    --compare
      DATA{Baseline/DzZ_T1.seg.nrrd}
      ${ITK_TEST_OUTPUT_DIR}/itkFastGrowCutTestDzZ_T1-label.nrrd
  itkFastGrowCutTest
    DATA{Input/DzZ_T1_orig.nhdr,DzZ_T1_orig.raw.gz}
    DATA{Input/DzZ_Seeds.seg.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/itkFastGrowCutTestDzZ_T1-label.nrrd
  )

itk_add_test(NAME itkFastGrowCutTestLarge
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

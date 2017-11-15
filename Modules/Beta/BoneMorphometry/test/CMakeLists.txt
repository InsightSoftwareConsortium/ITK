itk_module_test()

set(BoneMorphometryTests
    BoneMorphometryFeaturesFilterInstantiationTest.cxx
    BoneMorphometryFeaturesImageFilterInstantiationTest.cxx
    ReplaceFeatureMapNanInfImageFilterInstantiationTest.cxx
  )

CreateTestDriver(BoneMorphometry "${BoneMorphometry-Test_LIBRARIES}" "${BoneMorphometryTests}")

itk_add_test(NAME BoneMorphometryFeaturesFilterInstantiationTest
  COMMAND BoneMorphometryTestDriver
  BoneMorphometryFeaturesFilterInstantiationTest
  DATA{Input/Scan_CBCT_13R.nrrd} DATA{Input/SegmC_CBCT_13R.nrrd})


itk_add_test(NAME BoneMorphometryFeaturesImageFilterInstantiationTest
  COMMAND BoneMorphometryTestDriver
  --compare DATA{Baseline/resultTestFilterInstensiation.nrrd}
            ${ITK_TEST_OUTPUT_DIR}/resultTestFilterInstensiation.nrrd
  BoneMorphometryFeaturesImageFilterInstantiationTest
  DATA{Input/Scan_CBCT_13R.nrrd}
  DATA{Input/SegmC_CBCT_13R.nrrd}
  ${ITK_TEST_OUTPUT_DIR}/resultTestFilterInstensiation.nrrd)

itk_add_test(NAME ReplaceFeatureMapNanInfImageFilterInstantiationTest
  COMMAND BoneMorphometryTestDriver
  --compare DATA{Baseline/resultTestFilterInstensiation.nrrd}
            ${ITK_TEST_OUTPUT_DIR}/resultTestReplaceFeatureMapNanInfImageFilter.nrrd
  ReplaceFeatureMapNanInfImageFilterInstantiationTest
  DATA{Input/Scan_CBCT_13R.nrrd}
  DATA{Input/SegmC_CBCT_13R.nrrd}
  ${ITK_TEST_OUTPUT_DIR}/resultTestReplaceFeatureMapNanInfImageFilter.nrrd)

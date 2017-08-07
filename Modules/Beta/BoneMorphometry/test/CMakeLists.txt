itk_module_test()

set(BoneMorphometryTests BoneMorphometryImageFilterInstantiationTest.cxx
  )

CreateTestDriver(BoneMorphometry "${BoneMorphometry-Test_LIBRARIES}" "${BoneMorphometryTests}")

itk_add_test(NAME BoneMorphometryImageFilterInstantiationTest
  COMMAND BoneMorphometryTestDriver
  BoneMorphometryImageFilterInstantiationTest
  DATA{Input/Scan_CBCT_13R.nrrd} DATA{Input/SegmC_CBCT_13R.nrrd})

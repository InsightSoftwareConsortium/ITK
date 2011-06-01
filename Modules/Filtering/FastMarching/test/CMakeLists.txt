itk_module_test()

set(ITK-FastMarchingTests
itkFastMarchingHeaderTest.cxx
itkFastMarchingExtensionImageFilterTest.cxx
itkFastMarchingTest.cxx
itkFastMarchingTest2.cxx
itkFastMarchingUpwindGradientTest.cxx
# New files
itkFastMarchingBaseTest.cxx
itkFastMarchingImageFilterBaseTest.cxx
itkFastMarchingImageFilterRealTest1.cxx
itkFastMarchingImageFilterRealTest2.cxx
itkFastMarchingImageTopologicalTest.cxx
itkFastMarchingQuadEdgeMeshFilterBaseTest2.cxx
itkFastMarchingQuadEdgeMeshFilterBaseTest3.cxx
itkFastMarchingQuadEdgeMeshFilterBaseTest.cxx
itkFastMarchingStoppingCriterionBaseTest.cxx
itkFastMarchingThresholdStoppingCriterionTest.cxx
itkFastMarchingUpwindGradientBaseTest.cxx
)

CreateTestDriver(ITK-FastMarching "${ITK-FastMarching-Test_LIBRARIES}" "${ITK-FastMarchingTests}")

itk_add_test(NAME itkFastMarchingHeaderTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingHeaderTest)
itk_add_test(NAME itkFastMarchingExtensionImageFilterTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingExtensionImageFilterTest)
itk_add_test(NAME itkFastMarchingTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingTest)
itk_add_test(NAME itkFastMarchingTest2
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingTest2)
itk_add_test(NAME itkFastMarchingUpwindGradientTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingUpwindGradientTest)

itk_add_test(NAME itkFastMarchingBaseTest0
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingBaseTest 0 )
itk_add_test(NAME itkFastMarchingBaseTest1
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingBaseTest 1 )

itk_add_test(NAME itkFastMarchingImageFilterBaseTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingImageFilterBaseTest )

itk_add_test(NAME itkFastMarchingImageFilterRealTest1
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingImageFilterRealTest1)

itk_add_test(NAME itkFastMarchingImageFilterRealTest2
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingImageFilterRealTest2)

itk_add_test(NAME itkFastMarchingUpwindGradientBaseTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingUpwindGradientBaseTest )

itk_add_test(NAME itkFastMarchingQuadEdgeMeshFilterBaseTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingQuadEdgeMeshFilterBaseTest )

itk_add_test(NAME itkFastMarchingQuadEdgeMeshFilterBaseTest2
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingQuadEdgeMeshFilterBaseTest2 )

itk_add_test(NAME itkFastMarchingQuadEdgeMeshFilterBaseTest3
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingQuadEdgeMeshFilterBaseTest3 )

itk_add_test(NAME itkFastMarchingStoppingCriterionBaseTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingStoppingCriterionBaseTest )

itk_add_test(NAME itkFastMarchingThresholdStoppingCriterionTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingThresholdStoppingCriterionTest )

# -------------------------------------------------------------------------
# Topology constrained front propagation
# -------------------------------------------------------------------------

# ************************************************************************
# 2D

itk_add_test(NAME itkFastMarchingImageFilterTest_Brain2D_SingleSeed_NoTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    2
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/Baseline/Filtering/test_Brain2D_singleSeed_NoTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_singleSeed.nii.gz
    150
    0
)

itk_add_test(NAME itkFastMarchingImageFilterTest_Brain2D_SingleSeed_StrictTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    2
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/Baseline/Filtering/test_Brain2D_singleSeed_StrictTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_singleSeed.nii.gz
    150
    1
)

itk_add_test(NAME itkFastMarchingImageFilterTest_Brain2D_SingleSeed_NoHandlesTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    2
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
  test_Brain2D_singleSeed_NoHandlesTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_singleSeed.nii.gz
    150
    2
)

itk_add_test(NAME itkFastMarchingImageFilterTest_Brain2D_multipleSeeds_NoTopo
      COMMAND ITK-FastMarchingTestDriver
  itkFastMarchingImageTopologicalTest 2
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
  ${ITK_TEST_OUTPUT_DIR}/test_Brain2D_multipleSeeds_NoTopo.nii.gz
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_multipleSeeds.nii.gz
  150
  0
)

itk_add_test(NAME itkFastMarchingImageFilterTest_Brain2D_multipleSeeds_StrictTopo
      COMMAND ITK-FastMarchingTestDriver
  itkFastMarchingImageTopologicalTest
  2
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
  ${ITK_TEST_OUTPUT_DIR}/test_Brain2D_multipleSeeds_StrictTopo.nii.gz
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_multipleSeeds.nii.gz
  150
  1
)

itk_add_test( NAME itkFastMarchingImageFilterTest_Brain2D_multipleSeeds_NoHandlesTopo
      COMMAND ITK-FastMarchingTestDriver
  itkFastMarchingImageTopologicalTest
  2
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
  ${ITK_TEST_OUTPUT_DIR}/test_Brain2D_multipleSeeds_NoHandlesTopo.nii.gz
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_multipleSeeds.nii.gz
  150
  2
)

# ************************************************************************
# 3D
itk_add_test( NAME itkFastMarchingImageFilterTest_torus_multipleSeeds_NoTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_torus_multipleSeeds_NoTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus_multipleSeeds.nii.gz
    150
    0
)

itk_add_test( NAME itkFastMarchingImageFilterTest_torus_multipleSeeds_StrictTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_torus_multipleSeeds_StrictTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus_multipleSeeds.nii.gz
    150
    1
)

itk_add_test( NAME itkFastMarchingImageFilterTest_torus_multipleSeeds_NoHandlesTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_torus_multipleSeeds_NoHandlesTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus_multipleSeeds.nii.gz
    150
    2
)

itk_add_test( NAME itkFastMarchingImageFilterTest_wm_multipleSeeds_NoTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_wm_multipleSeeds_NoTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm_multipleSeeds.nii.gz
    150
    0
)

itk_add_test( NAME itkFastMarchingImageFilterTest_wm_multipleSeeds_StrictTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_wm_multipleSeeds_StrictTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm_multipleSeeds.nii.gz
    150
    1
)

itk_add_test( NAME itkFastMarchingImageFilterTest_wm_multipleSeeds_NoHandlesTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_wm_multipleSeeds_NoHandlesTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm_multipleSeeds.nii.gz
    150
    2
)

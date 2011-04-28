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

add_test(NAME itkFastMarchingHeaderTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingHeaderTest)
add_test(NAME itkFastMarchingExtensionImageFilterTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingExtensionImageFilterTest)
add_test(NAME itkFastMarchingTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingTest)
add_test(NAME itkFastMarchingTest2
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingTest2)
add_test(NAME itkFastMarchingUpwindGradientTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingUpwindGradientTest)

add_test(NAME itkFastMarchingBaseTest0
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingBaseTest 0 )
add_test(NAME itkFastMarchingBaseTest1
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingBaseTest 1 )

add_test(NAME itkFastMarchingImageFilterBaseTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingImageFilterBaseTest )

add_test(NAME itkFastMarchingImageFilterRealTest1
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingImageFilterRealTest1)

add_test(NAME itkFastMarchingImageFilterRealTest2
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingImageFilterRealTest2)

add_test(NAME itkFastMarchingUpwindGradientBaseTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingUpwindGradientBaseTest )

add_test(NAME itkFastMarchingQuadEdgeMeshFilterBaseTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingQuadEdgeMeshFilterBaseTest )

add_test(NAME itkFastMarchingQuadEdgeMeshFilterBaseTest2
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingQuadEdgeMeshFilterBaseTest2 )

add_test(NAME itkFastMarchingQuadEdgeMeshFilterBaseTest3
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingQuadEdgeMeshFilterBaseTest3 )

add_test(NAME itkFastMarchingStoppingCriterionBaseTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingStoppingCriterionBaseTest )

add_test(NAME itkFastMarchingThresholdStoppingCriterionTest
      COMMAND ITK-FastMarchingTestDriver itkFastMarchingThresholdStoppingCriterionTest )

# -------------------------------------------------------------------------
# Topology constrained front propagation
# -------------------------------------------------------------------------

# ************************************************************************
# 2D

add_test(NAME itkFastMarchingImageFilterTest_Brain2D_SingleSeed_NoTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    2
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/Baseline/Filtering/test_Brain2D_singleSeed_NoTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_singleSeed.nii.gz
    150
    0
)

add_test(NAME itkFastMarchingImageFilterTest_Brain2D_SingleSeed_StrictTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    2
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/Baseline/Filtering/test_Brain2D_singleSeed_StrictTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_singleSeed.nii.gz
    150
    1
)

add_test(NAME itkFastMarchingImageFilterTest_Brain2D_SingleSeed_NoHandlesTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    2
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
  test_Brain2D_singleSeed_NoHandlesTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_singleSeed.nii.gz
    150
    2
)

add_test(NAME itkFastMarchingImageFilterTest_Brain2D_multipleSeeds_NoTopo
      COMMAND ITK-FastMarchingTestDriver
  itkFastMarchingImageTopologicalTest 2
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
  ${ITK_TEST_OUTPUT_DIR}/test_Brain2D_multipleSeeds_NoTopo.nii.gz
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_multipleSeeds.nii.gz
  150
  0
)

add_test(NAME itkFastMarchingImageFilterTest_Brain2D_multipleSeeds_StrictTopo
      COMMAND ITK-FastMarchingTestDriver
  itkFastMarchingImageTopologicalTest
  2
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
  ${ITK_TEST_OUTPUT_DIR}/test_Brain2D_multipleSeeds_StrictTopo.nii.gz
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_multipleSeeds.nii.gz
  150
  1
)

add_test( NAME itkFastMarchingImageFilterTest_Brain2D_multipleSeeds_NoHandlesTopo
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
add_test( NAME itkFastMarchingImageFilterTest_torus_multipleSeeds_NoTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_torus_multipleSeeds_NoTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus_multipleSeeds.nii.gz
    150
    0
)

add_test( NAME itkFastMarchingImageFilterTest_torus_multipleSeeds_StrictTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_torus_multipleSeeds_StrictTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus_multipleSeeds.nii.gz
    150
    1
)

add_test( NAME itkFastMarchingImageFilterTest_torus_multipleSeeds_NoHandlesTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_torus_multipleSeeds_NoHandlesTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus_multipleSeeds.nii.gz
    150
    2
)

add_test( NAME itkFastMarchingImageFilterTest_wm_multipleSeeds_NoTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_wm_multipleSeeds_NoTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm_multipleSeeds.nii.gz
    150
    0
)

add_test( NAME itkFastMarchingImageFilterTest_wm_multipleSeeds_StrictTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_wm_multipleSeeds_StrictTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm_multipleSeeds.nii.gz
    150
    1
)

add_test( NAME itkFastMarchingImageFilterTest_wm_multipleSeeds_NoHandlesTopo
      COMMAND ITK-FastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_wm_multipleSeeds_NoHandlesTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm_multipleSeeds.nii.gz
    150
    2
)

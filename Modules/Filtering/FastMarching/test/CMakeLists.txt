itk_module_test()

set(ITKFastMarchingTests
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
itkFastMarchingQuadEdgeMeshFilterBaseTest4.cxx
itkFastMarchingQuadEdgeMeshFilterBaseTest.cxx
itkFastMarchingStoppingCriterionBaseTest.cxx
itkFastMarchingThresholdStoppingCriterionTest.cxx
itkFastMarchingUpwindGradientBaseTest.cxx
)

CreateTestDriver(ITKFastMarching "${ITKFastMarching-Test_LIBRARIES}" "${ITKFastMarchingTests}")

itk_add_test(NAME itkFastMarchingExtensionImageFilterTest
      COMMAND ITKFastMarchingTestDriver itkFastMarchingExtensionImageFilterTest)
itk_add_test(NAME itkFastMarchingTest
      COMMAND ITKFastMarchingTestDriver itkFastMarchingTest)
itk_add_test(NAME itkFastMarchingTest2
      COMMAND ITKFastMarchingTestDriver itkFastMarchingTest2)
itk_add_test(NAME itkFastMarchingUpwindGradientTest
      COMMAND ITKFastMarchingTestDriver itkFastMarchingUpwindGradientTest)

itk_add_test(NAME itkFastMarchingBaseTest0
      COMMAND ITKFastMarchingTestDriver itkFastMarchingBaseTest 0 )
itk_add_test(NAME itkFastMarchingBaseTest1
      COMMAND ITKFastMarchingTestDriver itkFastMarchingBaseTest 1 )

itk_add_test(NAME itkFastMarchingImageFilterBaseTest
      COMMAND ITKFastMarchingTestDriver itkFastMarchingImageFilterBaseTest )

itk_add_test(NAME itkFastMarchingImageFilterRealTest1
      COMMAND ITKFastMarchingTestDriver itkFastMarchingImageFilterRealTest1)

itk_add_test(NAME itkFastMarchingImageFilterRealTest2
      COMMAND ITKFastMarchingTestDriver itkFastMarchingImageFilterRealTest2)

itk_add_test(NAME itkFastMarchingUpwindGradientBaseTest
      COMMAND ITKFastMarchingTestDriver itkFastMarchingUpwindGradientBaseTest )

itk_add_test(NAME itkFastMarchingQuadEdgeMeshFilterBaseTest
      COMMAND ITKFastMarchingTestDriver itkFastMarchingQuadEdgeMeshFilterBaseTest )

itk_add_test(NAME itkFastMarchingQuadEdgeMeshFilterBaseTest2
      COMMAND ITKFastMarchingTestDriver itkFastMarchingQuadEdgeMeshFilterBaseTest2 )

itk_add_test(NAME itkFastMarchingQuadEdgeMeshFilterBaseTest3
      COMMAND ITKFastMarchingTestDriver itkFastMarchingQuadEdgeMeshFilterBaseTest3 )

itk_add_test(NAME itkFastMarchingQuadEdgeMeshFilterBaseTest4
      COMMAND ITKFastMarchingTestDriver itkFastMarchingQuadEdgeMeshFilterBaseTest4 )

itk_add_test(NAME itkFastMarchingStoppingCriterionBaseTest
      COMMAND ITKFastMarchingTestDriver itkFastMarchingStoppingCriterionBaseTest )

itk_add_test(NAME itkFastMarchingThresholdStoppingCriterionTest
      COMMAND ITKFastMarchingTestDriver itkFastMarchingThresholdStoppingCriterionTest )

# -------------------------------------------------------------------------
# Topology constrained front propagation
# -------------------------------------------------------------------------

# ************************************************************************
# 2D

itk_add_test(NAME itkFastMarchingImageFilterTest_Brain2D_SingleSeed_NoTopo
      COMMAND ITKFastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    2
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/Baseline/Filtering/test_Brain2D_singleSeed_NoTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_singleSeed.nii.gz
    150
    0
)

itk_add_test(NAME itkFastMarchingImageFilterTest_Brain2D_SingleSeed_StrictTopo
      COMMAND ITKFastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    2
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/Baseline/Filtering/test_Brain2D_singleSeed_StrictTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_singleSeed.nii.gz
    150
    1
)

itk_add_test(NAME itkFastMarchingImageFilterTest_Brain2D_SingleSeed_NoHandlesTopo
      COMMAND ITKFastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    2
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
  test_Brain2D_singleSeed_NoHandlesTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_singleSeed.nii.gz
    150
    2
)

itk_add_test(NAME itkFastMarchingImageFilterTest_Brain2D_multipleSeeds_NoTopo
      COMMAND ITKFastMarchingTestDriver
  itkFastMarchingImageTopologicalTest 2
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
  ${ITK_TEST_OUTPUT_DIR}/test_Brain2D_multipleSeeds_NoTopo.nii.gz
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_multipleSeeds.nii.gz
  150
  0
)

itk_add_test(NAME itkFastMarchingImageFilterTest_Brain2D_multipleSeeds_StrictTopo
      COMMAND ITKFastMarchingTestDriver
  itkFastMarchingImageTopologicalTest
  2
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_speed.nii.gz
  ${ITK_TEST_OUTPUT_DIR}/test_Brain2D_multipleSeeds_StrictTopo.nii.gz
  ${ITK_DATA_ROOT}/Baseline/Filtering/BrainProtonDensitySlice_multipleSeeds.nii.gz
  150
  1
)

itk_add_test( NAME itkFastMarchingImageFilterTest_Brain2D_multipleSeeds_NoHandlesTopo
      COMMAND ITKFastMarchingTestDriver
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
      COMMAND ITKFastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_torus_multipleSeeds_NoTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus_multipleSeeds.nii.gz
    150
    0
)

itk_add_test( NAME itkFastMarchingImageFilterTest_torus_multipleSeeds_StrictTopo
      COMMAND ITKFastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_torus_multipleSeeds_StrictTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus_multipleSeeds.nii.gz
    150
    1
)

itk_add_test( NAME itkFastMarchingImageFilterTest_torus_multipleSeeds_NoHandlesTopo
      COMMAND ITKFastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_torus_multipleSeeds_NoHandlesTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/torus_multipleSeeds.nii.gz
    150
    2
)

itk_add_test( NAME itkFastMarchingImageFilterTest_wm_multipleSeeds_NoTopo
      COMMAND ITKFastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_wm_multipleSeeds_NoTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm_multipleSeeds.nii.gz
    150
    0
)

itk_add_test( NAME itkFastMarchingImageFilterTest_wm_multipleSeeds_StrictTopo
      COMMAND ITKFastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_wm_multipleSeeds_StrictTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm_multipleSeeds.nii.gz
    150
    1
)

itk_add_test( NAME itkFastMarchingImageFilterTest_wm_multipleSeeds_NoHandlesTopo
      COMMAND ITKFastMarchingTestDriver
    itkFastMarchingImageTopologicalTest
    3
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/test_wm_multipleSeeds_NoHandlesTopo.nii.gz
    ${ITK_DATA_ROOT}/Baseline/Filtering/wm_multipleSeeds.nii.gz
    150
    2
)

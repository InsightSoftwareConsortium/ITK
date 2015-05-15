itk_module_test()

set(
  MorphologicalContourInterpolationTests
  itkMorphologicalContourInterpolationTest.cxx
)

createtestdriver( MorphologicalContourInterpolation "${MorphologicalContourInterpolation-Test_LIBRARIES}" "${MorphologicalContourInterpolationTests}")

itk_add_test(
  NAME itkMorphologicalContourInterpolationTest_hippocampus_exvivo
  COMMAND
    MorphologicalContourInterpolationTestDriver
    itkMorphologicalContourInterpolationTest
    DATA{Input/105769moving_resliced_to_fixedhz2_segmentation.nii.gz}
    ${ITK_TEST_OUTPUT_DIR}/105769moving_resliced_to_fixedhz2_segmentation_interpolated.nii.gz
)

itk_add_test(
  NAME itkMorphologicalContourInterpolationTest_hippocampus_invivo
  COMMAND
    MorphologicalContourInterpolationTestDriver
    itkMorphologicalContourInterpolationTest
    DATA{Input/105769fixed_segmentation.nii.gz}
    ${ITK_TEST_OUTPUT_DIR}/105769fixed_segmentation_interpolated.nii.gz
)

itk_add_test(
  NAME itkMorphologicalContourInterpolationTest_amygdala_exvivoLF
  COMMAND
    MorphologicalContourInterpolationTestDriver
    itkMorphologicalContourInterpolationTest
    DATA{Input/64816L_amygdala_fin.nii.gz}
    ${ITK_TEST_OUTPUT_DIR}/4816L_amygdala_fin_interpolated.nii.gz
)

itk_add_test(
  NAME itkMorphologicalContourInterpolationTest_amygdala_exvivoLI
  COMMAND
    MorphologicalContourInterpolationTestDriver
    itkMorphologicalContourInterpolationTest
    DATA{Input/64816L_amygdala_int.nii.gz}
    ${ITK_TEST_OUTPUT_DIR}/64816L_amygdala_int_interpolated.nii.gz
)

itk_add_test(
  NAME itkMorphologicalContourInterpolationTest_amygdala_exvivoRF
  COMMAND
    MorphologicalContourInterpolationTestDriver
    itkMorphologicalContourInterpolationTest
    DATA{Input/65239R_amygdala_fin.nii.gz}
    ${ITK_TEST_OUTPUT_DIR}/65239R_amygdala_fin_interpolated.nii.gz
)

itk_add_test(
  NAME itkMorphologicalContourInterpolationTest_amygdala_exvivoRI
  COMMAND
    MorphologicalContourInterpolationTestDriver
    itkMorphologicalContourInterpolationTest
    DATA{Input/65239R_amygdala_int.nii.gz}
    ${ITK_TEST_OUTPUT_DIR}/65239R_amygdala_int_interpolated.nii.gz
)

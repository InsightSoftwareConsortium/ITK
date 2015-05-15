itk_module_test()

set(
  MorphologicalContourInterpolationTests
  itkMorphologicalContourInterpolationTest.cxx
)

createtestdriver( MorphologicalContourInterpolation "${MorphologicalContourInterpolation-Test_LIBRARIES}" "${MorphologicalContourInterpolationTests}")

itk_add_test(
  NAME itkMorphologicalContourInterpolationTest
  COMMAND
    MorphologicalContourInterpolationTestDriver
    itkMorphologicalContourInterpolationTest
    DATA{Input/105769moving_resliced_to_fixedhz2_segmentation.nii.gz}
    ${ITK_TEST_OUTPUT_DIR}/105769moving_resliced_to_fixedhz2_segmentation_interpolated.nii.gz
)

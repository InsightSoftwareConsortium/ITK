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
)

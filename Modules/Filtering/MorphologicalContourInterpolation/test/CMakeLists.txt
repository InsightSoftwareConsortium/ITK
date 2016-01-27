itk_module_test()

set(
  MorphologicalContourInterpolationTests
  itkMorphologicalContourInterpolationTest.cxx
)

createtestdriver( MorphologicalContourInterpolation "${MorphologicalContourInterpolation-Test_LIBRARIES}" "${MorphologicalContourInterpolationTests}")

add_executable(
  runFromIDE
  manualTest.cxx
  ${MorphologicalContourInterpolationTests}
)
target_link_libraries(
  runFromIDE
  ${MorphologicalContourInterpolation-Test_LIBRARIES}
)

function(JustInputTest ImageName Ext)
  itk_add_test(
    NAME itkMorphologicalContourInterpolation_JustInputTest_${ImageName}
    COMMAND
      MorphologicalContourInterpolationTestDriver
      itkMorphologicalContourInterpolationTest
      DATA{Input/${ImageName}.${Ext}}
      ${ITK_TEST_OUTPUT_DIR}/${ImageName}_interpolated.${Ext}
  )
endfunction()

function(RegressionTest ImageName Ext)
  itk_add_test(
    NAME itkMorphologicalContourInterpolation_RegressionTest_${ImageName}
    COMMAND
      MorphologicalContourInterpolationTestDriver
      --compare
      DATA{Baseline/${ImageName}.${Ext}}
      ${ITK_TEST_OUTPUT_DIR}/${ImageName}_interpolated.${Ext}
      itkMorphologicalContourInterpolationTest
      DATA{Input/${ImageName}.${Ext}}
      ${ITK_TEST_OUTPUT_DIR}/${ImageName}_interpolated.${Ext}
  )
endfunction()

# Handcrafted tests
regressiontest(Empty nrrd)
regressiontest(NoSlices nrrd) #interpolation should not be invoked (output==input)
regressiontest(Micro1 nrrd)
regressiontest(SimplestOneToOne nrrd)
regressiontest(OneToOne nrrd)
regressiontest(OneToThree nrrd)
regressiontest(ExtrapolationAppearing nrrd)
justinputtest(DoubleTwoLabelBranching nrrd)
justinputtest(TwoAxisDoubleTwoLabelBranchingConflict nrrd)
justinputtest(ThreeAxisFourLabelConflict nrrd)
justinputtest(AccidentalMiddleSliceSegmentation nrrd)
justinputtest(SevenLabels nrrd)
justinputtest(FullEnd nrrd)
justinputtest(ManyToMany16 nrrd)
regressiontest(ManyToMany nrrd)
justinputtest(GridSeg nrrd)

# Paul's tests
justinputtest(105769moving_resliced_to_fixedhz2_segmentation nii.gz)
justinputtest(105769fixed_segmentation nii.gz)
justinputtest(64816L_amygdala_int nii.gz)
justinputtest(64816L_amygdala_fin nii.gz)
justinputtest(65239R_amygdala_int nii.gz)
justinputtest(65239R_amygdala_fin nii.gz)

# Long-running handcrafted tests
justinputtest(BigZ nrrd)
justinputtest(FaceRoI nrrd) #TODO: convert into regression test with FaceRoIFinished.nrrd as expected output

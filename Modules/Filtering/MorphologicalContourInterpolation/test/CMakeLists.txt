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
    NAME itkMorphologicalContourInterpolationTest_${ImageName}
    COMMAND
      MorphologicalContourInterpolationTestDriver
      itkMorphologicalContourInterpolationTest
      DATA{Input/${ImageName}.${Ext}}
      ${ITK_TEST_OUTPUT_DIR}/${ImageName}_interpolated.${Ext}
  )
endfunction()

# Handcrafted tests
justinputtest(Micro1 nrrd)
justinputtest(SimplestOneToOne nrrd)
justinputtest(OneToOne nrrd)
justinputtest(OneToThree nrrd)
justinputtest(ExtrapolationAppearing nrrd)
justinputtest(DoubleTwoLabelBranching nrrd)
justinputtest(TwoAxisDoubleTwoLabelBranchingConflict nrrd)
justinputtest(ThreeAxisFourLabelConflict nrrd)
justinputtest(AccidentalMiddleSliceSegmentation nrrd)
justinputtest(SevenLabels nrrd)
justinputtest(FullEnd nrrd)
justinputtest(ManyToMany16 nrrd)
justinputtest(ManyToMany nrrd)
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

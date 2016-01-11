itk_module_test()

set(
  MorphologicalContourInterpolationTests
  itkMorphologicalContourInterpolationTest.cxx
)

createtestdriver( MorphologicalContourInterpolation "${MorphologicalContourInterpolation-Test_LIBRARIES}" "${MorphologicalContourInterpolationTests}")

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
justinputtest(SimplestOneToOne nrrd)
justinputtest(OneToOne nrrd)
justinputtest(OneToThree nrrd)
justinputtest(ExtrapolationAppearing nrrd)
justinputtest(DoubleTwoLabelBranching nrrd)
justinputtest(TwoAxisDoubleTwoLabelBranchingConflict nrrd)
justinputtest(ThreeAxisFourLabelConflict nrrd)
justinputtest(AccidentalMiddleSliceSegmentation nrrd)
justinputtest(FullEndSevenLabels nrrd)
justinputtest(ManyToMany nrrd)

# Paul's tests
justinputtest(105769moving_resliced_to_fixedhz2_segmentation nii.gz)
justinputtest(105769fixed_segmentation nii.gz)
justinputtest(64816L_amygdala_fin nii.gz)
justinputtest(64816L_amygdala_int nii.gz)
justinputtest(65239R_amygdala_fin nii.gz)
justinputtest(65239R_amygdala_int nii.gz)

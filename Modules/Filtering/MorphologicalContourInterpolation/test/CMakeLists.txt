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

function(JustInputTest ImageName Ext) #optional: Axis Label
  # used to produce an output which is then taken as a baseline for a regression test
  foreach(f ${ARGN}) #create unique test name
    set(optArgs "${optArgs}_${f}")
  endforeach()
  set(
    outImage
    "${ITK_TEST_OUTPUT_DIR}/${ImageName}_interpolated${optArgs}.${Ext}"
  )
  # set(imageNameOpt "${ImageName}${optArgs}") # only used for regression

  itk_add_test(
    NAME
      itkMorphologicalContourInterpolation_RegressionTest_${ImageName}${optArgs}
    COMMAND
      MorphologicalContourInterpolationTestDriver
      # --compare DATA{Baseline/${imageNameOpt}.${Ext}} ${outImage} # no regression
      itkMorphologicalContourInterpolationTest
      DATA{Input/${ImageName}.${Ext}}
      ${outImage}
      ${ARGN}
  )
endfunction()

function(RegressionTest ImageName Ext) #optional: Axis Label
  foreach(f ${ARGN}) #create unique test name
    set(optArgs "${optArgs}_${f}")
  endforeach()
  set(
    outImage
    "${ITK_TEST_OUTPUT_DIR}/${ImageName}_interpolated${optArgs}.${Ext}"
  )
  set(imageNameOpt "${ImageName}${optArgs}")

  itk_add_test(
    NAME
      itkMorphologicalContourInterpolation_RegressionTest_${ImageName}${optArgs}
    COMMAND
      MorphologicalContourInterpolationTestDriver
      --compare
      DATA{Baseline/${imageNameOpt}.${Ext}}
      ${outImage}
      itkMorphologicalContourInterpolationTest
      DATA{Input/${ImageName}.${Ext}}
      ${outImage}
      ${ARGN}
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
regressiontest(DoubleTwoLabelBranching nrrd)
regressiontest(TwoAxisDoubleTwoLabelBranchingConflict nrrd)
regressiontest(AccidentalMiddleSliceSegmentation nrrd)
regressiontest(SevenLabels nrrd)
regressiontest(FullEnd nrrd)
regressiontest(1MN1 nrrd) # double sided many broken into many + 1
regressiontest(ManyToMany16 nrrd)
regressiontest(ManyToMany nrrd)
regressiontest(GridSeg nrrd)
regressiontest(GridSeg2 nrrd)

# Per axis/label tests
regressiontest(GridSeg nrrd 0)
regressiontest(GridSeg nrrd 1)
regressiontest(GridSeg nrrd 2)
regressiontest(GridSeg2 nrrd 0)
regressiontest(GridSeg2 nrrd 1)
regressiontest(GridSeg2 nrrd 2)
regressiontest(ThreeAxisFourLabelConflict nrrd)
regressiontest(ThreeAxisFourLabelConflict nrrd 0)
regressiontest(ThreeAxisFourLabelConflict nrrd 1)
regressiontest(ThreeAxisFourLabelConflict nrrd 2)
regressiontest(ThreeAxisFourLabelConflict nrrd 0 0) # equals to _0
regressiontest(ThreeAxisFourLabelConflict nrrd 1 1)
regressiontest(ThreeAxisFourLabelConflict nrrd 2 3) # equals to _2
regressiontest(ThreeAxisFourLabelConflict nrrd 0 4) # equals to _0
regressiontest(ThreeAxisFourLabelConflict nrrd 1 2)
regressiontest(ThreeAxisFourLabelConflict nrrd 2 2) # does not modify image

# Paul's tests
justinputtest(105769moving_resliced_to_fixedhz2_segmentation nii.gz)
justinputtest(105769fixed_segmentation nii.gz)
justinputtest(64816L_amygdala_int nii.gz)
justinputtest(64816L_amygdala_fin nii.gz)
justinputtest(65239R_amygdala_int nii.gz)
justinputtest(65239R_amygdala_fin nii.gz)
#TODO: make different combinations of axis and label parameters

# Long-running handcrafted tests
regressiontest(FaceRoI nrrd)
regressiontest(FaceRoI nrrd 0)
regressiontest(FaceRoI nrrd 1)
regressiontest(FaceRoI nrrd 2)
justinputtest(BigZ nrrd)
justinputtest(BigZ nrrd 0)
justinputtest(BigZ nrrd 1)
justinputtest(BigZ nrrd 2)

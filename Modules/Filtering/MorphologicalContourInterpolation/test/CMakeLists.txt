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

# used to produce an output which is then taken as a baseline for a regression test
function(JustInputTest ImageName Ext) #optional: Algo Axis Label
  foreach(f ${ARGN}) #create unique test name
    set(optArgs "${optArgs}_${f}")
  endforeach()
  set(outImage "${ITK_TEST_OUTPUT_DIR}/${ImageName}${optArgs}.${Ext}")
  # set(imageNameOpt "${ImageName}${optArgs}") # only used for regression

  itk_add_test(
    NAME itkMCI_JustInOut_${ImageName}${optArgs}
    COMMAND
      MorphologicalContourInterpolationTestDriver
      # --compare DATA{Baseline/${imageNameOpt}.${Ext}} ${outImage} # no regression
      itkMorphologicalContourInterpolationTest
      DATA{Input/${ImageName}.${Ext}}
      ${outImage}
      ${ARGN}
  )
endfunction()

function(RegressionTest ImageName Ext) #optional: Algo Axis Label
  foreach(f ${ARGN}) #create unique test name
    set(optArgs "${optArgs}_${f}")
  endforeach()
  set(outImage "${ITK_TEST_OUTPUT_DIR}/${ImageName}${optArgs}.${Ext}")
  set(imageNameOpt "${ImageName}${optArgs}")

  itk_add_test(
    NAME itkMCI_${ImageName}${optArgs}
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
regressiontest(Empty nrrd RD)
regressiontest(NoSlices nrrd RD) #interpolation should not be invoked (output==input)
regressiontest(Micro1 nrrd RD)
regressiontest(SimplestOneToOne nrrd RD)
regressiontest(OneToOne nrrd RD)
regressiontest(OneToThree nrrd RD)
regressiontest(ExtrapolationAppearing nrrd RD)
regressiontest(DoubleTwoLabelBranching nrrd RD)
regressiontest(TwoAxisDoubleTwoLabelBC nrrd RD)
regressiontest(AccidentalMiddleSliceSeg nrrd RD)
regressiontest(SevenLabels nrrd RD)
regressiontest(FullEnd nrrd RD)
regressiontest(1MN1 nrrd RD) # double sided many broken into many + 1
regressiontest(ManyToMany16 nrrd RD)
regressiontest(ManyToMany nrrd RD)
regressiontest(GridSeg nrrd RD)
regressiontest(GridSeg2 nrrd RD)

# Per axis/label tests
regressiontest(GridSeg nrrd RD 0)
regressiontest(GridSeg nrrd RD 1)
regressiontest(GridSeg nrrd RD 2)
regressiontest(GridSeg2 nrrd RD 0)
regressiontest(GridSeg2 nrrd RD 1)
regressiontest(GridSeg2 nrrd RD 2)
regressiontest(ThreeAxisFourLabelConflict nrrd RD)
regressiontest(ThreeAxisFourLabelConflict nrrd RD 0)
regressiontest(ThreeAxisFourLabelConflict nrrd RD 1)
regressiontest(ThreeAxisFourLabelConflict nrrd RD 2)
regressiontest(ThreeAxisFourLabelConflict nrrd RD 0 0) # equals to _0
regressiontest(ThreeAxisFourLabelConflict nrrd RD 1 1)
regressiontest(ThreeAxisFourLabelConflict nrrd RD 2 3) # equals to _2
regressiontest(ThreeAxisFourLabelConflict nrrd RD 0 4) # equals to _0
regressiontest(ThreeAxisFourLabelConflict nrrd RD 1 2)
regressiontest(ThreeAxisFourLabelConflict nrrd RD 2 2) # does not modify image

# Paul's single axis tests
regressiontest(105769moving nii.gz RD) #defaults: all axes all labels
regressiontest(105769moving nii.gz RD 2) #axis 2
regressiontest(105769moving nii.gz RD -1 0) #all axes all labels
regressiontest(105769moving nii.gz RD -1 1) #all axes label 1
regressiontest(105769moving nii.gz RD -1 2) #all axes label 2
regressiontest(105769moving nii.gz RD 2 1) #axis 2 label 1
regressiontest(105769moving nii.gz RD 2 2) #axis 2 label 2
regressiontest(105769fixed nii.gz RD) #defaults: all axes all labels
regressiontest(105769fixed nii.gz RD 2) #axis 2
regressiontest(105769fixed nii.gz RD -1 0) #all axes all labels
regressiontest(105769fixed nii.gz RD -1 1) #all axes label 1
regressiontest(105769fixed nii.gz RD -1 2) #all axes label 2
regressiontest(105769fixed nii.gz RD 2 1) #axis 2 label 1
regressiontest(105769fixed nii.gz RD 2 2) #axis 2 label 2

# 64816L_amygdala_int
regressiontest(64816L_amygdala_int nii.gz RD)
regressiontest(64816L_amygdala_int nii.gz RD 0)
regressiontest(64816L_amygdala_int nii.gz RD 1)
regressiontest(64816L_amygdala_int nii.gz RD 2)
regressiontest(64816L_amygdala_int nii.gz RD 0 1)
regressiontest(64816L_amygdala_int nii.gz RD 0 2)
regressiontest(64816L_amygdala_int nii.gz RD 0 3)
regressiontest(64816L_amygdala_int nii.gz RD 0 4)
regressiontest(64816L_amygdala_int nii.gz RD 1 1)
regressiontest(64816L_amygdala_int nii.gz RD 1 2)
regressiontest(64816L_amygdala_int nii.gz RD 1 3)
regressiontest(64816L_amygdala_int nii.gz RD 1 4)
regressiontest(64816L_amygdala_int nii.gz RD 2 1)
regressiontest(64816L_amygdala_int nii.gz RD 2 2)
regressiontest(64816L_amygdala_int nii.gz RD 2 3)
regressiontest(64816L_amygdala_int nii.gz RD 2 4)

# 64816L_amygdala_fin
regressiontest(64816L_amygdala_fin nii.gz RD)
regressiontest(64816L_amygdala_fin nii.gz RD 0)
regressiontest(64816L_amygdala_fin nii.gz RD 1)
regressiontest(64816L_amygdala_fin nii.gz RD 2)
regressiontest(64816L_amygdala_fin nii.gz RD 0 1)
regressiontest(64816L_amygdala_fin nii.gz RD 0 2)
regressiontest(64816L_amygdala_fin nii.gz RD 0 3)
regressiontest(64816L_amygdala_fin nii.gz RD 0 4)
regressiontest(64816L_amygdala_fin nii.gz RD 1 1)
regressiontest(64816L_amygdala_fin nii.gz RD 1 2)
regressiontest(64816L_amygdala_fin nii.gz RD 1 3)
regressiontest(64816L_amygdala_fin nii.gz RD 1 4)
regressiontest(64816L_amygdala_fin nii.gz RD 2 1)
regressiontest(64816L_amygdala_fin nii.gz RD 2 2)
regressiontest(64816L_amygdala_fin nii.gz RD 2 3)
regressiontest(64816L_amygdala_fin nii.gz RD 2 4)

# 65239R_amygdala_int
regressiontest(65239R_amygdala_int nii.gz RD)
regressiontest(65239R_amygdala_int nii.gz RD 0)
regressiontest(65239R_amygdala_int nii.gz RD 1)
regressiontest(65239R_amygdala_int nii.gz RD 2)
regressiontest(65239R_amygdala_int nii.gz RD 0 1)
regressiontest(65239R_amygdala_int nii.gz RD 0 2)
regressiontest(65239R_amygdala_int nii.gz RD 0 3)
regressiontest(65239R_amygdala_int nii.gz RD 0 4)
regressiontest(65239R_amygdala_int nii.gz RD 1 1)
regressiontest(65239R_amygdala_int nii.gz RD 1 2)
regressiontest(65239R_amygdala_int nii.gz RD 1 3)
regressiontest(65239R_amygdala_int nii.gz RD 1 4)
regressiontest(65239R_amygdala_int nii.gz RD 2 1)
regressiontest(65239R_amygdala_int nii.gz RD 2 2)
regressiontest(65239R_amygdala_int nii.gz RD 2 3)
regressiontest(65239R_amygdala_int nii.gz RD 2 4)

# 65239R_amygdala_fin
regressiontest(65239R_amygdala_fin nii.gz RD)
regressiontest(65239R_amygdala_fin nii.gz RD 0)
regressiontest(65239R_amygdala_fin nii.gz RD 1)
regressiontest(65239R_amygdala_fin nii.gz RD 2)
regressiontest(65239R_amygdala_fin nii.gz RD 0 1)
regressiontest(65239R_amygdala_fin nii.gz RD 0 2)
regressiontest(65239R_amygdala_fin nii.gz RD 0 3)
regressiontest(65239R_amygdala_fin nii.gz RD 0 4)
regressiontest(65239R_amygdala_fin nii.gz RD 1 1)
regressiontest(65239R_amygdala_fin nii.gz RD 1 2)
regressiontest(65239R_amygdala_fin nii.gz RD 1 3)
regressiontest(65239R_amygdala_fin nii.gz RD 1 4)
regressiontest(65239R_amygdala_fin nii.gz RD 2 1)
regressiontest(65239R_amygdala_fin nii.gz RD 2 2)
regressiontest(65239R_amygdala_fin nii.gz RD 2 3)
regressiontest(65239R_amygdala_fin nii.gz RD 2 4)

# Long-running handcrafted tests
regressiontest(BigZ nrrd RD)
regressiontest(BigZ nrrd RD 0)
regressiontest(BigZ nrrd RD 1)
regressiontest(BigZ nrrd RD 2)
regressiontest(FaceRoI nrrd RD)
regressiontest(FaceRoI nrrd RD 0)
regressiontest(FaceRoI nrrd RD 1)
regressiontest(FaceRoI nrrd RD 2)

# Non-3D tests
regressiontest(TwoD png RD)
regressiontest(TwoD png RD -1 255) # same as regular
regressiontest(TwoD png RD 0) # same as regular
regressiontest(TwoD png RD -1 1) # does not modify image
regressiontest(TwoD png RD 1) # does not modify image
regressiontest(FourD nrrd RD)

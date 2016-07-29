itk_module_test()

set(
  MorphologicalContourInterpolationTests
  itkMorphologicalContourInterpolationTest.cxx
)

createtestdriver( MorphologicalContourInterpolation "${MorphologicalContourInterpolation-Test_LIBRARIES}" "${MorphologicalContourInterpolationTests}")

add_executable(
  manualTest
  manualTest.cxx
  ${MorphologicalContourInterpolationTests}
)
target_link_libraries(
  manualTest
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
regressiontest(Empty nrrd ${MCI_Algorithm})
regressiontest(NoSlices nrrd ${MCI_Algorithm}) #interpolation should not be invoked (output==input)
regressiontest(Micro1 nrrd ${MCI_Algorithm})
regressiontest(SimplestOneToOne nrrd ${MCI_Algorithm})
regressiontest(OneToOne nrrd ${MCI_Algorithm})
regressiontest(OneToThree nrrd ${MCI_Algorithm})
regressiontest(ExtrapolationAppearing nrrd ${MCI_Algorithm})
regressiontest(DoubleTwoLabelBranching nrrd ${MCI_Algorithm})
regressiontest(TwoAxisDoubleTwoLabelBC nrrd ${MCI_Algorithm})
regressiontest(AccidentalMiddleSliceSeg nrrd ${MCI_Algorithm})
regressiontest(SevenLabels nrrd ${MCI_Algorithm})
regressiontest(FullEnd nrrd ${MCI_Algorithm})
regressiontest(1MN1 nrrd ${MCI_Algorithm}) # double sided many broken into many + 1
regressiontest(ManyToMany16 nrrd ${MCI_Algorithm})
regressiontest(ManyToMany nrrd ${MCI_Algorithm})
regressiontest(GridSeg nrrd ${MCI_Algorithm})
regressiontest(GridSeg2 nrrd ${MCI_Algorithm})

# Per axis/label tests
regressiontest(GridSeg nrrd ${MCI_Algorithm} 0)
regressiontest(GridSeg nrrd ${MCI_Algorithm} 1)
regressiontest(GridSeg nrrd ${MCI_Algorithm} 2)
regressiontest(GridSeg2 nrrd ${MCI_Algorithm} 0)
regressiontest(GridSeg2 nrrd ${MCI_Algorithm} 1)
regressiontest(GridSeg2 nrrd ${MCI_Algorithm} 2)
regressiontest(ThreeAxisFourLabelConflict nrrd ${MCI_Algorithm})
regressiontest(ThreeAxisFourLabelConflict nrrd ${MCI_Algorithm} 0)
regressiontest(ThreeAxisFourLabelConflict nrrd ${MCI_Algorithm} 1)
regressiontest(ThreeAxisFourLabelConflict nrrd ${MCI_Algorithm} 2)
regressiontest(ThreeAxisFourLabelConflict nrrd ${MCI_Algorithm} 0 0) # equals to _0
regressiontest(ThreeAxisFourLabelConflict nrrd ${MCI_Algorithm} 1 1)
regressiontest(ThreeAxisFourLabelConflict nrrd ${MCI_Algorithm} 2 3) # equals to _2
regressiontest(ThreeAxisFourLabelConflict nrrd ${MCI_Algorithm} 0 4) # equals to _0
regressiontest(ThreeAxisFourLabelConflict nrrd ${MCI_Algorithm} 1 2)
regressiontest(ThreeAxisFourLabelConflict nrrd ${MCI_Algorithm} 2 2) # does not modify image

# Paul's single axis tests
regressiontest(105769moving nii.gz ${MCI_Algorithm}) #defaults: all axes all labels
regressiontest(105769moving nii.gz ${MCI_Algorithm} 2) #axis 2
regressiontest(105769moving nii.gz ${MCI_Algorithm} -1 0) #all axes all labels
regressiontest(105769moving nii.gz ${MCI_Algorithm} -1 1) #all axes label 1
regressiontest(105769moving nii.gz ${MCI_Algorithm} -1 2) #all axes label 2
regressiontest(105769moving nii.gz ${MCI_Algorithm} 2 1) #axis 2 label 1
regressiontest(105769moving nii.gz ${MCI_Algorithm} 2 2) #axis 2 label 2
regressiontest(105769fixed nii.gz ${MCI_Algorithm}) #defaults: all axes all labels
regressiontest(105769fixed nii.gz ${MCI_Algorithm} 2) #axis 2
regressiontest(105769fixed nii.gz ${MCI_Algorithm} -1 0) #all axes all labels
regressiontest(105769fixed nii.gz ${MCI_Algorithm} -1 1) #all axes label 1
regressiontest(105769fixed nii.gz ${MCI_Algorithm} -1 2) #all axes label 2
regressiontest(105769fixed nii.gz ${MCI_Algorithm} 2 1) #axis 2 label 1
regressiontest(105769fixed nii.gz ${MCI_Algorithm} 2 2) #axis 2 label 2

# 64816L_amygdala_int
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm})
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 0)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 1)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 2)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 0 1)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 0 2)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 0 3)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 0 4)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 1 1)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 1 2)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 1 3)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 1 4)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 2 1)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 2 2)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 2 3)
regressiontest(64816L_amygdala_int nii.gz ${MCI_Algorithm} 2 4)

# 64816L_amygdala_fin
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm})
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 0)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 1)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 2)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 0 1)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 0 2)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 0 3)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 0 4)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 1 1)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 1 2)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 1 3)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 1 4)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 2 1)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 2 2)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 2 3)
regressiontest(64816L_amygdala_fin nii.gz ${MCI_Algorithm} 2 4)

# 65239R_amygdala_int
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm})
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 0)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 1)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 2)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 0 1)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 0 2)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 0 3)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 0 4)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 1 1)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 1 2)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 1 3)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 1 4)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 2 1)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 2 2)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 2 3)
regressiontest(65239R_amygdala_int nii.gz ${MCI_Algorithm} 2 4)

# 65239R_amygdala_fin
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm})
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 0)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 1)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 2)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 0 1)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 0 2)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 0 3)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 0 4)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 1 1)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 1 2)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 1 3)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 1 4)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 2 1)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 2 2)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 2 3)
regressiontest(65239R_amygdala_fin nii.gz ${MCI_Algorithm} 2 4)

# Long-running handcrafted tests
regressiontest(BigZ nrrd ${MCI_Algorithm})
regressiontest(BigZ nrrd ${MCI_Algorithm} 0)
regressiontest(BigZ nrrd ${MCI_Algorithm} 1)
regressiontest(BigZ nrrd ${MCI_Algorithm} 2)
regressiontest(FaceRoI nrrd ${MCI_Algorithm})
regressiontest(FaceRoI nrrd ${MCI_Algorithm} 0)
regressiontest(FaceRoI nrrd ${MCI_Algorithm} 1)
regressiontest(FaceRoI nrrd ${MCI_Algorithm} 2)

# Non-3D tests
regressiontest(FourD nrrd ${MCI_Algorithm})

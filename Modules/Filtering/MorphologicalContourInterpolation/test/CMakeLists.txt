itk_module_test()

set(
  MorphologicalContourInterpolationTests
  itkMorphologicalContourInterpolationTest.cxx
  itkMorphologicalContourInterpolationTestWithRLEImage.cxx
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

add_executable(dscComparison dscComparison.cxx)
target_link_libraries(
  dscComparison
  ${MorphologicalContourInterpolation-Test_LIBRARIES}
)

# used to produce an output which is then taken as a baseline for a regression test
function(JustInputTest ImageName Ext) #optional: Axis Label
  foreach(f ${ARGN}) # for creating unique test name
    set(optArgs "${optArgs}_${f}")
  endforeach()
  foreach(algo B;C;T)
    set(imageAlgoArgs "${ImageName}_${algo}${optArgs}")

    itk_add_test(
      NAME itkMCI_JustInOut_${imageAlgoArgs}
      COMMAND
        MorphologicalContourInterpolationTestDriver
        itkMorphologicalContourInterpolationTest
        DATA{Input/${ImageName}.${Ext}}
        "${ITK_TEST_OUTPUT_DIR}/${imageAlgoArgs}.${Ext}"
        ${algo}
        ${ARGN}
    )
  endforeach()
endfunction()

function(RegressionTest RunsLong ImageName Ext) #optional: Axis Label
  foreach(f ${ARGN}) # for creating unique test name
    set(optArgs "${optArgs}_${f}")
  endforeach()

  # B = repeated dilations with ball structuring element
  # C = repeated dilations with cross structuring element
  # T = distance transform (not geodesic!)
  foreach(algo B;C;T)
    set(imageAlgoArgs "${ImageName}_${algo}${optArgs}")
    set(outImage "${ITK_TEST_OUTPUT_DIR}/${imageAlgoArgs}.${Ext}")

    itk_add_test(
      NAME itkMCI_${imageAlgoArgs}
      COMMAND
        MorphologicalContourInterpolationTestDriver
        --compare
        DATA{Baseline/${imageAlgoArgs}.${Ext}}
        ${outImage}
        itkMorphologicalContourInterpolationTest
        DATA{Input/${ImageName}.${Ext}}
        ${outImage}
        ${algo}
        ${ARGN}
    )

    itk_add_test(
      NAME itkMCIRLE_${imageAlgoArgs}
      COMMAND
        MorphologicalContourInterpolationTestDriver
        --compare
        DATA{Baseline/${imageAlgoArgs}.${Ext}}
        ${outImage}
        itkMorphologicalContourInterpolationTestWithRLEImage
        DATA{Input/${ImageName}.${Ext}}
        ${outImage}
        ${algo}
        ${ARGN}
    )

    if(RunsLong)
      set_property(
        TEST
          itkMCI_${imageAlgoArgs}
        APPEND
        PROPERTY
          LABELS
            RUNS_LONG
      )
      set_property(
        TEST
          itkMCIRLE_${imageAlgoArgs}
        APPEND
        PROPERTY
          LABELS
            RUNS_LONG
      )
    endif()
  endforeach()
endfunction()

# functional comparison using Dice Similarity Coefficient
function(DSCTest ImageName Ext)
  set(outFilenameBase "${ITK_TEST_OUTPUT_DIR}/${ImageName}")

  itk_add_test(
    NAME itkMCI_DSC_${ImageName}
    COMMAND
      dscComparison
      DATA{Input/${ImageName}.${Ext}}
      ${outFilenameBase}
      ${ARGN}
  )
  set_property(
    TEST
      itkMCI_DSC_${ImageName}
    APPEND
    PROPERTY
      LABELS
        RUNS_LONG
  )
endfunction()

# Handcrafted tests
regressiontest(0 Empty nrrd)
regressiontest(0 NoSlices nrrd) #interpolation should not be invoked (output==input)
regressiontest(0 Micro1 nrrd)
regressiontest(0 SimplestOneToOne nrrd)
regressiontest(0 OneToOne nrrd)
regressiontest(0 OneToThree nrrd)
regressiontest(0 ExtrapolationAppearing nrrd)
regressiontest(0 DoubleTwoLabelBranching nrrd)
regressiontest(0 TwoAxisDoubleTwoLabelBC nrrd)
regressiontest(0 AccidentalMiddleSliceSeg nrrd)
regressiontest(0 SevenLabels nrrd)
regressiontest(0 FullEnd nrrd)
regressiontest(0 1MN1 nrrd) # double sided many broken into many + 1
regressiontest(0 ManyToMany16 nrrd)
regressiontest(0 ManyToMany nrrd)
regressiontest(0 GridSeg nrrd)
regressiontest(0 GridSeg2 nrrd)

# Per axis/label tests
regressiontest(0 GridSeg nrrd 0)
regressiontest(0 GridSeg nrrd 1)
regressiontest(0 GridSeg nrrd 2)
regressiontest(0 GridSeg2 nrrd 0)
regressiontest(0 GridSeg2 nrrd 1)
regressiontest(0 GridSeg2 nrrd 2)
regressiontest(0 ThreeAxisFourLabelConflict nrrd)
regressiontest(0 ThreeAxisFourLabelConflict nrrd 0)
regressiontest(0 ThreeAxisFourLabelConflict nrrd 1)
regressiontest(0 ThreeAxisFourLabelConflict nrrd 2)
regressiontest(0 ThreeAxisFourLabelConflict nrrd 0 0) # equals to _0
regressiontest(0 ThreeAxisFourLabelConflict nrrd 1 1)
regressiontest(0 ThreeAxisFourLabelConflict nrrd 2 3) # equals to _2
regressiontest(0 ThreeAxisFourLabelConflict nrrd 0 4) # equals to _0
regressiontest(0 ThreeAxisFourLabelConflict nrrd 1 2)
regressiontest(0 ThreeAxisFourLabelConflict nrrd 2 2) # does not modify image

# Paul's single axis tests
regressiontest(0 105769moving nii.gz) #defaults: all axes all labels
regressiontest(0 105769moving nii.gz 2) #axis 2
regressiontest(0 105769moving nii.gz -1 0) #all axes all labels
regressiontest(0 105769moving nii.gz -1 1) #all axes label 1
regressiontest(0 105769moving nii.gz -1 2) #all axes label 2
regressiontest(0 105769moving nii.gz 2 1) #axis 2 label 1
regressiontest(0 105769moving nii.gz 2 2) #axis 2 label 2
regressiontest(0 105769fixed nii.gz) #defaults: all axes all labels
regressiontest(0 105769fixed nii.gz 2) #axis 2
regressiontest(0 105769fixed nii.gz -1 0) #all axes all labels
regressiontest(0 105769fixed nii.gz -1 1) #all axes label 1
regressiontest(0 105769fixed nii.gz -1 2) #all axes label 2
regressiontest(0 105769fixed nii.gz 2 1) #axis 2 label 1
regressiontest(0 105769fixed nii.gz 2 2) #axis 2 label 2

# 64816L_amygdala_int
regressiontest(0 64816L_amygdala_int nii.gz)
regressiontest(0 64816L_amygdala_int nii.gz 0)
regressiontest(0 64816L_amygdala_int nii.gz 1)
regressiontest(0 64816L_amygdala_int nii.gz 2)
regressiontest(0 64816L_amygdala_int nii.gz 0 1)
regressiontest(0 64816L_amygdala_int nii.gz 0 2)
regressiontest(0 64816L_amygdala_int nii.gz 0 3)
regressiontest(0 64816L_amygdala_int nii.gz 0 4)
regressiontest(0 64816L_amygdala_int nii.gz 1 1)
regressiontest(0 64816L_amygdala_int nii.gz 1 2)
regressiontest(0 64816L_amygdala_int nii.gz 1 3)
regressiontest(0 64816L_amygdala_int nii.gz 1 4)
regressiontest(0 64816L_amygdala_int nii.gz 2 1)
regressiontest(0 64816L_amygdala_int nii.gz 2 2)
regressiontest(0 64816L_amygdala_int nii.gz 2 3)
regressiontest(0 64816L_amygdala_int nii.gz 2 4)

# 64816L_amygdala_fin
regressiontest(0 64816L_amygdala_fin nii.gz)
regressiontest(0 64816L_amygdala_fin nii.gz 0)
regressiontest(0 64816L_amygdala_fin nii.gz 1)
regressiontest(0 64816L_amygdala_fin nii.gz 2)
regressiontest(0 64816L_amygdala_fin nii.gz 0 1)
regressiontest(0 64816L_amygdala_fin nii.gz 0 2)
regressiontest(0 64816L_amygdala_fin nii.gz 0 3)
regressiontest(0 64816L_amygdala_fin nii.gz 0 4)
regressiontest(0 64816L_amygdala_fin nii.gz 1 1)
regressiontest(0 64816L_amygdala_fin nii.gz 1 2)
regressiontest(0 64816L_amygdala_fin nii.gz 1 3)
regressiontest(0 64816L_amygdala_fin nii.gz 1 4)
regressiontest(0 64816L_amygdala_fin nii.gz 2 1)
regressiontest(0 64816L_amygdala_fin nii.gz 2 2)
regressiontest(0 64816L_amygdala_fin nii.gz 2 3)
regressiontest(0 64816L_amygdala_fin nii.gz 2 4)

# 65239R_amygdala_int
regressiontest(0 65239R_amygdala_int nii.gz)
regressiontest(0 65239R_amygdala_int nii.gz 0)
regressiontest(0 65239R_amygdala_int nii.gz 1)
regressiontest(0 65239R_amygdala_int nii.gz 2)
regressiontest(0 65239R_amygdala_int nii.gz 0 1)
regressiontest(0 65239R_amygdala_int nii.gz 0 2)
regressiontest(0 65239R_amygdala_int nii.gz 0 3)
regressiontest(0 65239R_amygdala_int nii.gz 0 4)
regressiontest(0 65239R_amygdala_int nii.gz 1 1)
regressiontest(0 65239R_amygdala_int nii.gz 1 2)
regressiontest(0 65239R_amygdala_int nii.gz 1 3)
regressiontest(0 65239R_amygdala_int nii.gz 1 4)
regressiontest(0 65239R_amygdala_int nii.gz 2 1)
regressiontest(0 65239R_amygdala_int nii.gz 2 2)
regressiontest(0 65239R_amygdala_int nii.gz 2 3)
regressiontest(0 65239R_amygdala_int nii.gz 2 4)

# 65239R_amygdala_fin
regressiontest(0 65239R_amygdala_fin nii.gz)
regressiontest(0 65239R_amygdala_fin nii.gz 0)
regressiontest(0 65239R_amygdala_fin nii.gz 1)
regressiontest(0 65239R_amygdala_fin nii.gz 2)
regressiontest(0 65239R_amygdala_fin nii.gz 0 1)
regressiontest(0 65239R_amygdala_fin nii.gz 0 2)
regressiontest(0 65239R_amygdala_fin nii.gz 0 3)
regressiontest(0 65239R_amygdala_fin nii.gz 0 4)
regressiontest(0 65239R_amygdala_fin nii.gz 1 1)
regressiontest(0 65239R_amygdala_fin nii.gz 1 2)
regressiontest(0 65239R_amygdala_fin nii.gz 1 3)
regressiontest(0 65239R_amygdala_fin nii.gz 1 4)
regressiontest(0 65239R_amygdala_fin nii.gz 2 1)
regressiontest(0 65239R_amygdala_fin nii.gz 2 2)
regressiontest(0 65239R_amygdala_fin nii.gz 2 3)
regressiontest(0 65239R_amygdala_fin nii.gz 2 4)

# Long-running handcrafted tests
regressiontest(1 BigZ nrrd)
regressiontest(1 BigZ nrrd 0)
regressiontest(1 BigZ nrrd 1)
regressiontest(1 BigZ nrrd 2)
regressiontest(1 FaceRoI nrrd)
regressiontest(1 FaceRoI nrrd 0)
regressiontest(1 FaceRoI nrrd 1)
regressiontest(1 FaceRoI nrrd 2)

# Non-3D tests
regressiontest(0 FourD nrrd)

# dscComparison (crops)
dsctest(c2l  mha saveImages)
dsctest(c2b  mha saveImages)
dsctest(c10l mha saveImages)
dsctest(c10b mha saveImages)

# dscComparison (full resolution)
dsctest(case_2_labels  mha saveImages)
dsctest(case_2_binary  mha saveImages)
dsctest(case_10_labels mha saveImages)
dsctest(case_10_binary mha saveImages)

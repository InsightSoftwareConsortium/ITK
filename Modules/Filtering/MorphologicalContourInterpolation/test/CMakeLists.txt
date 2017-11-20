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
    # have the RLE version output the results into a different file
    # otherwise parallel invocation of tests causes undeterministic failures
    # when both tests try to write to the same file at the same time
    set(outImageRLE "${ITK_TEST_OUTPUT_DIR}/${imageAlgoArgs}_RLE.${Ext}")

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
        ${outImageRLE}
        itkMorphologicalContourInterpolationTestWithRLEImage
        DATA{Input/${ImageName}.${Ext}}
        ${outImageRLE}
        ${algo}
        ${ARGN}
    )

    set_property(
      TEST
        itkMCI_${imageAlgoArgs}
      APPEND
      PROPERTY
        LABELS
          MCI_PLAIN
    )
    set_property(
      TEST
        itkMCIRLE_${imageAlgoArgs}
      APPEND
      PROPERTY
        LABELS
          MCI_RLE
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

function(RegressionTestShort)
  regressiontest(No ${ARGN})
endfunction()

function(RegressionTestLong)
  regressiontest(Yes ${ARGN})
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
regressiontestshort(Empty nrrd)
regressiontestshort(NoSlices nrrd) #interpolation should not be invoked (output==input)
regressiontestshort(Micro1 nrrd)
regressiontestshort(SimplestOneToOne nrrd)
regressiontestshort(OneToOne nrrd)
regressiontestshort(OneToThree nrrd)
regressiontestshort(ExtrapolationAppearing nrrd)
regressiontestshort(DoubleTwoLabelBranching nrrd)
regressiontestshort(TwoAxisDoubleTwoLabelBC nrrd)
regressiontestshort(AccidentalMiddleSliceSeg nrrd)
regressiontestshort(SevenLabels nrrd)
regressiontestshort(FullEnd nrrd)
regressiontestshort(1MN1 nrrd) # double sided many broken into many + 1
regressiontestshort(ManyToMany16 nrrd)
regressiontestshort(ManyToMany nrrd)
regressiontestshort(GridSeg nrrd)
regressiontestshort(GridSeg2 nrrd)

# Per axis/label tests
regressiontestshort(GridSeg nrrd 0)
regressiontestshort(GridSeg nrrd 1)
regressiontestshort(GridSeg nrrd 2)
regressiontestshort(GridSeg2 nrrd 0)
regressiontestshort(GridSeg2 nrrd 1)
regressiontestshort(GridSeg2 nrrd 2)
regressiontestshort(ThreeAxisFourLabelConflict nrrd)
regressiontestshort(ThreeAxisFourLabelConflict nrrd 0)
regressiontestshort(ThreeAxisFourLabelConflict nrrd 1)
regressiontestshort(ThreeAxisFourLabelConflict nrrd 2)
regressiontestshort(ThreeAxisFourLabelConflict nrrd 0 0) # equals to _0
regressiontestshort(ThreeAxisFourLabelConflict nrrd 1 1)
regressiontestshort(ThreeAxisFourLabelConflict nrrd 2 3) # equals to _2
regressiontestshort(ThreeAxisFourLabelConflict nrrd 0 4) # equals to _0
regressiontestshort(ThreeAxisFourLabelConflict nrrd 1 2)
regressiontestshort(ThreeAxisFourLabelConflict nrrd 2 2) # does not modify image

# Paul's single axis tests
regressiontestshort(105769moving nii.gz) #defaults: all axes all labels
regressiontestshort(105769moving nii.gz 2) #axis 2
regressiontestshort(105769moving nii.gz -1 0) #all axes all labels
regressiontestshort(105769moving nii.gz -1 1) #all axes label 1
regressiontestshort(105769moving nii.gz -1 2) #all axes label 2
regressiontestshort(105769moving nii.gz 2 1) #axis 2 label 1
regressiontestshort(105769moving nii.gz 2 2) #axis 2 label 2
regressiontestshort(105769fixed nii.gz) #defaults: all axes all labels
regressiontestshort(105769fixed nii.gz 2) #axis 2
regressiontestshort(105769fixed nii.gz -1 0) #all axes all labels
regressiontestshort(105769fixed nii.gz -1 1) #all axes label 1
regressiontestshort(105769fixed nii.gz -1 2) #all axes label 2
regressiontestshort(105769fixed nii.gz 2 1) #axis 2 label 1
regressiontestshort(105769fixed nii.gz 2 2) #axis 2 label 2

# 64816L_amygdala_int
regressiontestshort(64816L_amygdala_int nii.gz)
regressiontestshort(64816L_amygdala_int nii.gz 0)
regressiontestshort(64816L_amygdala_int nii.gz 1)
regressiontestshort(64816L_amygdala_int nii.gz 2)
regressiontestshort(64816L_amygdala_int nii.gz 0 1)
regressiontestshort(64816L_amygdala_int nii.gz 0 2)
regressiontestshort(64816L_amygdala_int nii.gz 0 3)
regressiontestshort(64816L_amygdala_int nii.gz 0 4)
regressiontestshort(64816L_amygdala_int nii.gz 1 1)
regressiontestshort(64816L_amygdala_int nii.gz 1 2)
regressiontestshort(64816L_amygdala_int nii.gz 1 3)
regressiontestshort(64816L_amygdala_int nii.gz 1 4)
regressiontestshort(64816L_amygdala_int nii.gz 2 1)
regressiontestshort(64816L_amygdala_int nii.gz 2 2)
regressiontestshort(64816L_amygdala_int nii.gz 2 3)
regressiontestshort(64816L_amygdala_int nii.gz 2 4)

# 64816L_amygdala_fin
regressiontestshort(64816L_amygdala_fin nii.gz)
regressiontestshort(64816L_amygdala_fin nii.gz 0)
regressiontestshort(64816L_amygdala_fin nii.gz 1)
regressiontestshort(64816L_amygdala_fin nii.gz 2)
regressiontestshort(64816L_amygdala_fin nii.gz 0 1)
regressiontestshort(64816L_amygdala_fin nii.gz 0 2)
regressiontestshort(64816L_amygdala_fin nii.gz 0 3)
regressiontestshort(64816L_amygdala_fin nii.gz 0 4)
regressiontestshort(64816L_amygdala_fin nii.gz 1 1)
regressiontestshort(64816L_amygdala_fin nii.gz 1 2)
regressiontestshort(64816L_amygdala_fin nii.gz 1 3)
regressiontestshort(64816L_amygdala_fin nii.gz 1 4)
regressiontestshort(64816L_amygdala_fin nii.gz 2 1)
regressiontestshort(64816L_amygdala_fin nii.gz 2 2)
regressiontestshort(64816L_amygdala_fin nii.gz 2 3)
regressiontestshort(64816L_amygdala_fin nii.gz 2 4)

# 65239R_amygdala_int
regressiontestshort(65239R_amygdala_int nii.gz)
regressiontestshort(65239R_amygdala_int nii.gz 0)
regressiontestshort(65239R_amygdala_int nii.gz 1)
regressiontestshort(65239R_amygdala_int nii.gz 2)
regressiontestshort(65239R_amygdala_int nii.gz 0 1)
regressiontestshort(65239R_amygdala_int nii.gz 0 2)
regressiontestshort(65239R_amygdala_int nii.gz 0 3)
regressiontestshort(65239R_amygdala_int nii.gz 0 4)
regressiontestshort(65239R_amygdala_int nii.gz 1 1)
regressiontestshort(65239R_amygdala_int nii.gz 1 2)
regressiontestshort(65239R_amygdala_int nii.gz 1 3)
regressiontestshort(65239R_amygdala_int nii.gz 1 4)
regressiontestshort(65239R_amygdala_int nii.gz 2 1)
regressiontestshort(65239R_amygdala_int nii.gz 2 2)
regressiontestshort(65239R_amygdala_int nii.gz 2 3)
regressiontestshort(65239R_amygdala_int nii.gz 2 4)

# 65239R_amygdala_fin
regressiontestshort(65239R_amygdala_fin nii.gz)
regressiontestshort(65239R_amygdala_fin nii.gz 0)
regressiontestshort(65239R_amygdala_fin nii.gz 1)
regressiontestshort(65239R_amygdala_fin nii.gz 2)
regressiontestshort(65239R_amygdala_fin nii.gz 0 1)
regressiontestshort(65239R_amygdala_fin nii.gz 0 2)
regressiontestshort(65239R_amygdala_fin nii.gz 0 3)
regressiontestshort(65239R_amygdala_fin nii.gz 0 4)
regressiontestshort(65239R_amygdala_fin nii.gz 1 1)
regressiontestshort(65239R_amygdala_fin nii.gz 1 2)
regressiontestshort(65239R_amygdala_fin nii.gz 1 3)
regressiontestshort(65239R_amygdala_fin nii.gz 1 4)
regressiontestshort(65239R_amygdala_fin nii.gz 2 1)
regressiontestshort(65239R_amygdala_fin nii.gz 2 2)
regressiontestshort(65239R_amygdala_fin nii.gz 2 3)
regressiontestshort(65239R_amygdala_fin nii.gz 2 4)

# Long-running handcrafted tests
regressiontestlong(BigZ nrrd)
regressiontestlong(BigZ nrrd 0)
regressiontestlong(BigZ nrrd 1)
regressiontestlong(BigZ nrrd 2)
regressiontestlong(FaceRoI nrrd)
regressiontestlong(FaceRoI nrrd 0)
regressiontestlong(FaceRoI nrrd 1)
regressiontestlong(FaceRoI nrrd 2)

# Non-3D tests
regressiontestshort(FourD nrrd)

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

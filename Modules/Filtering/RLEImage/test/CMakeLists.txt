itk_module_test()

set(
  RLEImageTests
  itkRLEImageTest.cxx
  itkRegionOfInterestRLEImageFilterTest.cxx
  itkIteratorTestsForRLEImage.cxx
  itkRLEImageIteratorsForwardBackwardTest.cxx
  itkRLEImageIteratorTest.cxx
  itkRLEImageIteratorWithIndexTest.cxx
  itkRLEImageRegionConstIteratorWithOnlyIndexTest.cxx
  itkRLEImageRegionIteratorTest.cxx
  itkRLEImageScanlineIteratorTest1.cxx
)

createtestdriver( RLEImage "${RLEImage-Test_LIBRARIES}" "${RLEImageTests}" )

add_executable(
  runFromIDE
  manualTest.cxx
  ${RLEImageTests}
)
target_link_libraries(runFromIDE ${RLEImage-Test_LIBRARIES})

add_executable(rleStats rleStats.cxx)
target_link_libraries(rleStats ${RLEImage-Test_LIBRARIES})

itk_add_test(
  NAME itkIteratorTestsForRLEImage
  COMMAND
    RLEImageTestDriver
    itkIteratorTestsForRLEImage
)
itk_add_test(
  NAME itkRegionOfInterestRLEImageFilterTest
  COMMAND
    RLEImageTestDriver
    itkRegionOfInterestRLEImageFilterTest
)
itk_add_test(
  NAME itkRLEImageIteratorsForwardBackwardTest
  COMMAND
    RLEImageTestDriver
    itkRLEImageIteratorsForwardBackwardTest
)
itk_add_test(
  NAME itkRLEImageIteratorTest
  COMMAND
    RLEImageTestDriver
    itkRLEImageIteratorTest
)
itk_add_test(
  NAME itkRLEImageIteratorWithIndexTest
  COMMAND
    RLEImageTestDriver
    itkRLEImageIteratorWithIndexTest
)
itk_add_test(
  NAME itkRLEImageRegionConstIteratorWithOnlyIndexTest
  COMMAND
    RLEImageTestDriver
    itkRLEImageRegionConstIteratorWithOnlyIndexTest
)
itk_add_test(
  NAME itkRLEImageRegionIteratorTest
  COMMAND
    RLEImageTestDriver
    itkRLEImageRegionIteratorTest
)
itk_add_test(
  NAME itkRLEImageScanlineIteratorTest1
  COMMAND
    RLEImageTestDriver
    itkRLEImageScanlineIteratorTest1
)

function(ReadWriteTest ImageName Ext) # optional: big
  set(outImage "${ITK_TEST_OUTPUT_DIR}/${ImageName}.${Ext}")
  if("${ARGN}" STREQUAL "big")
    if(DEFINED ITK_COMPUTER_MEMORY_SIZE)
      if(${ITK_COMPUTER_MEMORY_SIZE} GREATER_EQUAL 16)
        itk_add_test(
          NAME RLEImage${ImageName}
          COMMAND
            RLEImageTestDriver
            --compare
            DATA{Input/${ImageName}.${Ext}}
            ${outImage}
            itkRLEImageTest
            DATA{Input/${ImageName}.${Ext}}
            ${outImage}
        )
        set_property(
          TEST
            RLEImage${ImageName}
          APPEND
          PROPERTY
            LABELS
              RUNS_LONG
        )
        set_property(
          TEST
            RLEImage${ImageName}
          APPEND
          PROPERTY
            RUN_SERIAL
              True
        )
      endif()
    endif()
  else()
    itk_add_test(
      NAME RLEImage${ImageName}
      COMMAND
        RLEImageTestDriver
        --compare
        DATA{Input/${ImageName}.${Ext}}
        ${outImage}
        itkRLEImageTest
        DATA{Input/${ImageName}.${Ext}}
        ${outImage}
    )
  endif()
endfunction()

# RLEImage specific tests - quick
readwritetest(Small2D png)
readwritetest(brainParc mha)
readwritetest(vb-seg mha)
readwritetest(ws nrrd)
readwritetest(wb-crop nrrd)

# Handcrafted tests for MorphologicalContourInterpolation
readwritetest(Empty nrrd)
readwritetest(NoSlices nrrd)
readwritetest(Micro1 nrrd)
readwritetest(SimplestOneToOne nrrd)
readwritetest(OneToOne nrrd)
readwritetest(OneToThree nrrd)
readwritetest(ExtrapolationAppearing nrrd)
readwritetest(DoubleTwoLabelBranching nrrd)
readwritetest(TwoAxisDoubleTwoLabelBC nrrd)
readwritetest(AccidentalMiddleSliceSeg nrrd)
readwritetest(ThreeAxisFourLabelConflict nrrd)
readwritetest(SevenLabels nrrd)
readwritetest(FullEnd nrrd)
readwritetest(1MN1 nrrd)
readwritetest(ManyToMany16 nrrd)
readwritetest(ManyToMany nrrd)
readwritetest(GridSeg nrrd)
readwritetest(GridSeg2 nrrd)
readwritetest(FourD nrrd)
readwritetest(2D png)
readwritetest(BigZ nrrd)
readwritetest(FaceRoI nrrd)

# Paul's test images
readwritetest(105769moving nii.gz)
readwritetest(105769fixed nii.gz)
readwritetest(64816L_amygdala_fin nii.gz)
readwritetest(64816L_amygdala_int nii.gz)
readwritetest(65239R_amygdala_fin nii.gz)
readwritetest(65239R_amygdala_int nii.gz)

# RLEImage specific tests - long
readwritetest(wb-seg nrrd big)
readwritetest(wb-seg32 nrrd big)

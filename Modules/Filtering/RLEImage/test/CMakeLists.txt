itk_module_test()

set(RLEImageTests itkRLEImageTest.cxx)

createtestdriver( RLEImage "${RLEImage-Test_LIBRARIES}" "${RLEImageTests}" )

add_executable(
  runFromIDE
  manualTest.cxx
  ${RLEImageTests}
)
target_link_libraries(runFromIDE ${RLEImage-Test_LIBRARIES})

add_executable(rleStats rleStats.cxx)
target_link_libraries(rleStats ${RLEImage-Test_LIBRARIES})

add_executable(
  iteratorTests
  itkRegionOfInterestImageFilterTest.cxx
  itkIteratorTests.cxx
  itkImageIteratorsForwardBackwardTest.cxx
  itkImageIteratorTest.cxx
  itkImageIteratorWithIndexTest.cxx
  itkImageRegionConstIteratorWithOnlyIndexTest.cxx
  itkImageRegionIteratorTest.cxx
  itkImageScanlineIteratorTest1.cxx
  iteratorTests.cxx
)
target_link_libraries(iteratorTests "${RLEImage-Test_LIBRARIES}")
itk_add_test(NAME RLEImageiteratorTests COMMAND iteratorTests)

function(ReadWriteTest ImageName Ext)
  set(outImage "${ITK_TEST_OUTPUT_DIR}/${ImageName}.${Ext}")
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
endfunction()

# RLEImage specific tests - quick
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
readwritetest(wb-seg nrrd)
readwritetest(wb-seg32 nrrd)

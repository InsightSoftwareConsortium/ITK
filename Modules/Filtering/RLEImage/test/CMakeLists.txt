itk_module_test()

set(RLEImageTest ReadWriteTest.cxx)

createtestdriver( RLEImage "${RLEImage-Test_LIBRARIES}" "${RLEImageTest}" )

add_executable(
  runFromIDE
  manualTest.cxx
  ${RLEImageTest}
)
target_link_libraries(runFromIDE ${RLEImage-Test_LIBRARIES})

function(ReadWriteTest ImageName Ext)
  set(outImage "${ITK_TEST_OUTPUT_DIR}/${ImageName}.${Ext}")
  itk_add_test(
    NAME ${ImageName}
    COMMAND
      RLEImageTestDriver
      --compare
      DATA{Input/${ImageName}.${Ext}}
      ${outImage}
      RLEImageTest
      DATA{Input/${ImageName}.${Ext}}
      ${outImage}
  )
endfunction()

# Handcrafted tests
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
readwritetest(FaceRoI nrrd)
readwritetest(BigZ nrrd)
readwritetest(FourD nrrd)

# Paul's test images
readwritetest(105769moving nii.gz)
readwritetest(105769fixed nii.gz)
readwritetest(64816L_amygdala_fin nii.gz)
readwritetest(64816L_amygdala_int nii.gz)
readwritetest(65239R_amygdala_fin nii.gz)
readwritetest(65239R_amygdala_int nii.gz)

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
target_link_libraries(iteratorTests ${ITK_LIBRARIES})

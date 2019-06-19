

set(ITK_TEST_DRIVER itkTestDriver)

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Registration)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

itk_add_test(NAME ITKv3ImageRegistration1Test COMMAND  ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration1TestPixelCentered.png}
            ${TEMP}/ImageRegistration1TestPixelCentered.png
  $<TARGET_FILE:ITKv3ImageRegistration1>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration1TestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceAfterTestPixelCentered.png
        ${TEMP}/ImageRegistration1DifferenceBeforeTestPixelCentered.png
)

itk_add_test(NAME ITKv3ImageRegistration3Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration3TestPixelCentered.png}
            ${TEMP}/ImageRegistration3TestPixelCentered.png
  $<TARGET_FILE:ITKv3ImageRegistration3>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration3TestPixelCentered.png
)

itk_add_test(NAME ITKv3ImageRegistration4Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ITKv3ImageRegistration4Test.png}
            ${TEMP}/ImageRegistration4Test.png
  $<TARGET_FILE:ITKv3ImageRegistration4>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration4Test.png 0
        ${TEMP}/ImageRegistration4BeforeTest.png
        ${TEMP}/ImageRegistration4AfterTest.png
        24 10000 1
)

itk_add_test(NAME ITKv3ImageRegistration4Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ITKv3ImageRegistration4Test.png}
            ${TEMP}/ImageRegistration4Test2.png
  $<TARGET_FILE:ITKv3ImageRegistration4>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/ImageRegistration4Test2.png 0
        ${TEMP}/ImageRegistration4BeforeTest.png
        ${TEMP}/ImageRegistration4AfterTest.png
        24 10000 0
)

itk_add_test(NAME ITKv3ImageRegistration4Test3 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:ITKv3ImageRegistration4>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20Mask.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration4Test3.png 0
        ${TEMP}/ImageRegistration4BeforeTest3.png
        ${TEMP}/ImageRegistration4AfterTest3.png
        5 10000 0
)

itk_add_test(NAME ITKv3ImageRegistration5Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration5Test1PixelCentered.png}
            ${TEMP}/ImageRegistration5Test1PixelCentered.png
  $<TARGET_FILE:ITKv3ImageRegistration5>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceRotated10.png
        ${TEMP}/ImageRegistration5Test1PixelCentered.png
)

itk_add_test(NAME ITKv3ImageRegistration5Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ITKv3ImageRegistration5Test2PixelCentered.png}
            ${TEMP}/ImageRegistration5Test2PixelCentered.png
  $<TARGET_FILE:ITKv3ImageRegistration5>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration5Test2PixelCentered.png
)

itk_add_test(NAME ITKv3ImageRegistration6Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration6TestPixelCentered.png}
            ${TEMP}/ImageRegistration6TestPixelCentered.png
  $<TARGET_FILE:ITKv3ImageRegistration6>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration6TestPixelCentered.png
)

itk_add_test(NAME ITKv3ImageRegistration7Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration7TestPixelCentered.png}
            ${TEMP}/ImageRegistration7TestPixelCentered.png
  $<TARGET_FILE:ITKv3ImageRegistration7>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceR10X13Y17S12.png
        ${TEMP}/ImageRegistration7TestPixelCentered.png
        ${TEMP}/ImageRegistration7BeforeTestPixelCentered.png
        ${TEMP}/ImageRegistration7AfterTestPixelCentered.png
        1.0   1.0  0.0
)

if(ITK_USE_BRAINWEB_DATA)

## A test needed to prepare for making the latex documentation.
itk_add_test(NAME ITKv3ITKv3ConfidenceConnected3DTest COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:ConfidenceConnected3D>
        DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb165a10f17.mha}
        ${TEMP}/WhiteMatterSegmentation.mhd
)

itk_add_test(NAME ITKv3ImageRegistration8Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration8RegisteredSlice.png}
            ${TEMP}/ImageRegistration8v3RegisteredSlice.png
  $<TARGET_FILE:ITKv3ImageRegistration8>
        DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
        DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20Rot10Tx15.mha}
        ${TEMP}/ImageRegistration8v3Output.mhd
        ${TEMP}/ImageRegistration8v3DifferenceBefore.mhd
        ${TEMP}/ImageRegistration8v3DifferenceAfter.mhd
        ${TEMP}/ImageRegistration8v3Output.png
        ${TEMP}/ImageRegistration8v3DifferenceBefore.png
        ${TEMP}/ImageRegistration8v3DifferenceAfter.png
        ${TEMP}/ImageRegistration8v3RegisteredSlice.png
)

endif()

itk_add_test(NAME ITKv3ImageRegistration9Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration9TestPixelCentered.png}
            ${TEMP}/ImageRegistration9TestPixelCentered.png
  $<TARGET_FILE:ITKv3ImageRegistration9>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration9TestPixelCentered.png
)

itk_add_test(NAME ITKv3ImageRegistration13Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration13TestPixelCentered.png}
            ${TEMP}/ImageRegistration13TestPixelCentered.png
  --compareNumberOfPixelsTolerance 5
  $<TARGET_FILE:ITKv3ImageRegistration13>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13TestPixelCentered.png
)

itk_add_test(NAME ITKv3ImageRegistration13Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration13TestPixelCentered.png}
            ${TEMP}/ImageRegistration13Test2PixelCentered.png
  --compareNumberOfPixelsTolerance 10
  $<TARGET_FILE:ITKv3ImageRegistration13>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test2PixelCentered.png
        0 0
)

itk_add_test(NAME ITKv3ImageRegistration13Test3 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration13TestPixelCentered.png}
            ${TEMP}/ImageRegistration13Test3PixelCentered.png
  --compareNumberOfPixelsTolerance 10
  $<TARGET_FILE:ITKv3ImageRegistration13>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test3PixelCentered.png
        0 1
)

itk_add_test(NAME ITKv3ImageRegistration13Test4 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration13TestPixelCentered.png}
            ${TEMP}/ImageRegistration13Test4PixelCentered.png
  --compareNumberOfPixelsTolerance 5
  $<TARGET_FILE:ITKv3ImageRegistration13>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test4PixelCentered.png
        1 0
)

itk_add_test(NAME ITKv3ImageRegistration13Test5 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration13TestPixelCentered.png}
            ${TEMP}/ImageRegistration13Test5PixelCentered.png
  --compareNumberOfPixelsTolerance 5
  $<TARGET_FILE:ITKv3ImageRegistration13>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration13Test5PixelCentered.png
        1 1
)

itk_add_test(NAME ITKv3ImageRegistration14Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration14Test.png,:}
            ${TEMP}/ImageRegistration14Test.png
  --compareNumberOfPixelsTolerance 2000
  $<TARGET_FILE:ITKv3ImageRegistration14>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceR10X13Y17.png
        ${TEMP}/ImageRegistration14Test.png 32 0.01 0.1  0.15  10.0  14.0
)
set_property(TEST ITKv3ImageRegistration14Test APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME ITKv3MultiResImageRegistration1Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/MultiResImageRegistration1Test.png,:}
            ${TEMP}/MultiResImageRegistration1Test.png
  --compareNumberOfPixelsTolerance 10
  --compareIntensityTolerance 7
  $<TARGET_FILE:ITKv3MultiResImageRegistration1>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration1Test.png 100
        ${TEMP}/MultiResImageRegistration1CheckerBoardBeforeTest.png
        ${TEMP}/MultiResImageRegistration1CheckerBoardAfterTest.png
)
set_property(TEST  ITKv3MultiResImageRegistration1Test APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME  ITKv3MultiResImageRegistration1Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/MultiResImageRegistration1Test.png,:}
            ${TEMP}/MultiResImageRegistration1Test2.png
  --compareNumberOfPixelsTolerance 10
  --compareIntensityTolerance 7
  $<TARGET_FILE:ITKv3MultiResImageRegistration1>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.png
        ${TEMP}/MultiResImageRegistration1Test2.png 100
        ${TEMP}/MultiResImageRegistration1CheckerBoardBeforeTest2.png
        ${TEMP}/MultiResImageRegistration1CheckerBoardAfterTest2.png
        0
)
set_property(TEST  ITKv3MultiResImageRegistration1Test2 APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME  ITKv3DeformableRegistration12Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/DeformableRegistration12Test.png}
            ${TEMP}/DeformableRegistration12Test1.png
  --compareNumberOfPixelsTolerance 600
  $<TARGET_FILE:ITKv3DeformableRegistration12>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        DATA{Baseline/BSplineWarping1Test.png}
        ${TEMP}/DeformableRegistration12Test1.png
        ${TEMP}/DeformableRegistration12Test1DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test1DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test1DisplacementField.mhd
        0 0
        ${TEMP}/DeformableRegistration12Test1FinalTransformParameters.txt
)
set_property(TEST  ITKv3DeformableRegistration12Test1 APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME  ITKv3DeformableRegistration12Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/DeformableRegistration12Test.png}
            ${TEMP}/DeformableRegistration12Test2.png
  --compareNumberOfPixelsTolerance 600
  $<TARGET_FILE:ITKv3DeformableRegistration12>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        DATA{Baseline/BSplineWarping1Test.png}
        ${TEMP}/DeformableRegistration12Test2.png
        ${TEMP}/DeformableRegistration12Test2DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test2DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test2DisplacementField.mhd
        0 1
        ${TEMP}/DeformableRegistration12Test2FinalTransformParameters.txt
)

itk_add_test(NAME  ITKv3DeformableRegistration12Test3 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/DeformableRegistration12Test.png}
            ${TEMP}/DeformableRegistration12Test3.png
  --compareNumberOfPixelsTolerance 600
  $<TARGET_FILE:ITKv3DeformableRegistration12>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        DATA{Baseline/BSplineWarping1Test.png}
        ${TEMP}/DeformableRegistration12Test3.png
        ${TEMP}/DeformableRegistration12Test3DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test3DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test3DisplacementField.mhd
        1 0
        ${TEMP}/DeformableRegistration12Test3FinalTransformParameters.txt
)
set_property(TEST  ITKv3DeformableRegistration12Test3 APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(NAME  ITKv3DeformableRegistration12Test4 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/DeformableRegistration12Test.png}
            ${TEMP}/DeformableRegistration12Test4.png
  --compareNumberOfPixelsTolerance 600
  $<TARGET_FILE:ITKv3DeformableRegistration12>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        DATA{Baseline/BSplineWarping1Test.png}
        ${TEMP}/DeformableRegistration12Test4.png
        ${TEMP}/DeformableRegistration12Test4DifferenceAfter.png
        ${TEMP}/DeformableRegistration12Test4DifferenceBefore.png
        ${TEMP}/DeformableRegistration12Test4DisplacementField.mhd
        1 1
        ${TEMP}/DeformableRegistration12Test4FinalTransformParameters.txt
)

if( ITK_USE_BRAINWEB_DATA )

  set( NUMBER_OF_BSPLINE_GRID_NODES 5 )

  if( CTEST_TEST_TIMEOUT GREATER 3000 )

    itk_add_test(NAME  ITKv3DeformableRegistration8Test1 COMMAND ${ITK_TEST_DRIVER}
        --compare DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
                  ${TEMP}/DeformableRegistration8Test1.mhd
        --compareNumberOfPixelsTolerance 700000
        --compareIntensityTolerance 15
      $<TARGET_FILE:ITKv3DeformableRegistration8>
            DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test1.mhd
            ${TEMP}/DeformableRegistration8Test1DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test1DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test1DisplacementField.mhd
            0 0
            ${TEMP}/DeformableRegistration8Test1FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
            ${ITK_EXAMPLE_DATA_ROOT}/IdentityTransform.tfm
    )
  set_property(TEST  ITKv3DeformableRegistration8Test1 PROPERTY DEPENDS BSplineWarping2Test)
  set_property(TEST  ITKv3DeformableRegistration8Test1 APPEND PROPERTY LABELS RUNS_LONG)

    itk_add_test(NAME ITKv3DeformableRegistration8Test2 COMMAND ${ITK_TEST_DRIVER}
        --compare DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
                  ${TEMP}/DeformableRegistration8Test2.mhd
        --compareNumberOfPixelsTolerance 700000
        --compareIntensityTolerance 15
      $<TARGET_FILE:ITKv3DeformableRegistration8>
            DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test2.mhd
            ${TEMP}/DeformableRegistration8Test2DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test2DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test2DisplacementField.mhd
            1 0
            ${TEMP}/DeformableRegistration8Test2FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
            ${ITK_EXAMPLE_DATA_ROOT}/IdentityTransform.tfm
    )
  set_property(TEST  ITKv3DeformableRegistration8Test2 PROPERTY DEPENDS BSplineWarping2Test)
  set_property(TEST  ITKv3DeformableRegistration8Test2 APPEND PROPERTY LABELS RUNS_LONG)

    itk_add_test(NAME ITKv3DeformableRegistration8Test3 COMMAND ${ITK_TEST_DRIVER}
        --compare DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
                  ${TEMP}/DeformableRegistration8Test3.mhd
        --compareNumberOfPixelsTolerance 700000
        --compareIntensityTolerance 15
      $<TARGET_FILE:ITKv3DeformableRegistration8>
            DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test3.mhd
            ${TEMP}/DeformableRegistration8Test3DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test3DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test3DisplacementField.mhd
            0 1
            ${TEMP}/DeformableRegistration8Test3FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
            ${ITK_EXAMPLE_DATA_ROOT}/IdentityTransform.tfm
    )
  set_property(TEST  ITKv3DeformableRegistration8Test3 PROPERTY DEPENDS BSplineWarping2Test)
  set_property(TEST  ITKv3DeformableRegistration8Test3 APPEND PROPERTY LABELS RUNS_LONG)

    itk_add_test(NAME ITKv3DeformableRegistration8Test4 COMMAND ${ITK_TEST_DRIVER}
        --compare DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
                  ${TEMP}/DeformableRegistration8Test4.mhd
        --compareNumberOfPixelsTolerance 700000
        --compareIntensityTolerance 15
      $<TARGET_FILE:ITKv3DeformableRegistration8>
            DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test4.mhd
            ${TEMP}/DeformableRegistration8Test4DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test4DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test4DisplacementField.mhd
            1 1
            ${TEMP}/DeformableRegistration8Test4FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
            ${ITK_EXAMPLE_DATA_ROOT}/IdentityTransform.tfm
    )
  set_property(TEST  ITKv3DeformableRegistration8Test4 PROPERTY DEPENDS BSplineWarping2Test)
  set_property(TEST  ITKv3DeformableRegistration8Test4 APPEND PROPERTY LABELS RUNS_LONG)

  endif() # CTEST_TEST_TIMEOUT GREATER 3000

  if( "${ITK_COMPUTER_MEMORY_SIZE}" GREATER 6 )

    set(NUMBER_OF_BSPLINE_GRID_NODES 32)
    itk_add_test(NAME ITKv3DeformableRegistration8Test5 COMMAND ${ITK_TEST_DRIVER}
      $<TARGET_FILE:ITKv3DeformableRegistration8>
            DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test5.mhd
            ${TEMP}/DeformableRegistration8Test5DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test5DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test5DisplacementField.mhd
            0 0
            ${TEMP}/DeformableRegistration8Test5FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
    )
    set_property(TEST ITKv3DeformableRegistration8Test5 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST ITKv3DeformableRegistration8Test5 APPEND PROPERTY LABELS RUNS_LONG)

    itk_add_test(NAME ITKv3DeformableRegistration8Test6 COMMAND ${ITK_TEST_DRIVER}
      $<TARGET_FILE:ITKv3DeformableRegistration8>
            DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test6.mhd
            ${TEMP}/DeformableRegistration8Test6DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test6DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test6DisplacementField.mhd
            0 1
            ${TEMP}/DeformableRegistration8Test6FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
    )
    set_property(TEST ITKv3DeformableRegistration8Test6 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST ITKv3DeformableRegistration8Test6 APPEND PROPERTY LABELS RUNS_LONG)

    itk_add_test(NAME ITKv3DeformableRegistration8Test7 COMMAND ${ITK_TEST_DRIVER}
      $<TARGET_FILE:ITKv3DeformableRegistration8>
            DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test7.mhd
            ${TEMP}/DeformableRegistration8Test7DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test7DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test7DisplacementField.mhd
            1 0
            ${TEMP}/DeformableRegistration8Test7FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
    )
    set_property(TEST ITKv3DeformableRegistration8Test7 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST ITKv3DeformableRegistration8Test7 APPEND PROPERTY LABELS RUNS_LONG)

    itk_add_test(NAME ITKv3DeformableRegistration8Test8 COMMAND ${ITK_TEST_DRIVER}
      $<TARGET_FILE:ITKv3DeformableRegistration8>
            DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test8.mhd
            ${TEMP}/DeformableRegistration8Test8DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test8DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test8DisplacementField.mhd
            1 1
            ${TEMP}/DeformableRegistration8Test8FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
    )
    set_property(TEST ITKv3DeformableRegistration8Test8 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST ITKv3DeformableRegistration8Test8 APPEND PROPERTY LABELS RUNS_LONG)

    set(NUMBER_OF_BSPLINE_GRID_NODES 100)
    itk_add_test(NAME ITKv3DeformableRegistration8Test9 COMMAND ${ITK_TEST_DRIVER}
      $<TARGET_FILE:ITKv3DeformableRegistration8>
            DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test9.mhd
            ${TEMP}/DeformableRegistration8Test9DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test9DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test9DisplacementField.mhd
            0 0
            ${TEMP}/DeformableRegistration8Test9FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
    )
    set_property(TEST ITKv3DeformableRegistration8Test9 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST ITKv3DeformableRegistration8Test9 APPEND PROPERTY LABELS RUNS_LONG)

    itk_add_test(NAME ITKv3DeformableRegistration8Test10 COMMAND ${ITK_TEST_DRIVER}
      $<TARGET_FILE:ITKv3DeformableRegistration8>
            DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
            ${TEMP}/BSplineWarping2Test.mhd
            ${TEMP}/DeformableRegistration8Test10.mhd
            ${TEMP}/DeformableRegistration8Test10DifferenceAfter.mhd
            ${TEMP}/DeformableRegistration8Test10DifferenceBefore.mhd
            ${TEMP}/DeformableRegistration8Test10DisplacementField.mhd
            0 1
            ${TEMP}/DeformableRegistration8Test10FinalTransformParameters.txt
            ${NUMBER_OF_BSPLINE_GRID_NODES}
    )
    set_property(TEST ITKv3DeformableRegistration8Test10 PROPERTY DEPENDS BSplineWarping2Test)
    set_property(TEST ITKv3DeformableRegistration8Test10 APPEND PROPERTY LABELS RUNS_LONG)

    # Due to the large memory requirements this tests must be run one by one
    set_tests_properties(
      ITKv3DeformableRegistration8Test5
      ITKv3DeformableRegistration8Test6
      ITKv3DeformableRegistration8Test7
      ITKv3DeformableRegistration8Test8
      ITKv3DeformableRegistration8Test9
      ITKv3DeformableRegistration8Test10
      PROPERTIES
      RESOURCE_LOCK MEMORY_SIZE
      )

    if( "${ITK_COMPUTER_MEMORY_SIZE}" GREATER 16 )

      itk_add_test(NAME ITKv3DeformableRegistration8Test11 COMMAND ${ITK_TEST_DRIVER}
        $<TARGET_FILE:ITKv3DeformableRegistration8>
              DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
              ${TEMP}/BSplineWarping2Test.mhd
              ${TEMP}/DeformableRegistration8Test11.mhd
              ${TEMP}/DeformableRegistration8Test11DifferenceAfter.mhd
              ${TEMP}/DeformableRegistration8Test11DifferenceBefore.mhd
              ${TEMP}/DeformableRegistration8Test11DisplacementField.mhd
              1 0
              ${TEMP}/DeformableRegistration8Test11FinalTransformParameters.txt
              ${NUMBER_OF_BSPLINE_GRID_NODES}
      )
      set_property(TEST ITKv3DeformableRegistration8Test11 PROPERTY DEPENDS BSplineWarping2Test)
      set_property(TEST ITKv3DeformableRegistration8Test11 APPEND PROPERTY LABELS RUNS_LONG)

      itk_add_test(NAME ITKv3DeformableRegistration8Test12 COMMAND ${ITK_TEST_DRIVER}
        $<TARGET_FILE:ITKv3DeformableRegistration8>
              DATA{${ITK_EXAMPLE_DATA_ROOT}/BrainWeb/brainweb1e1a10f20.mha}
              ${TEMP}/BSplineWarping2Test.mhd
              ${TEMP}/DeformableRegistration8Test12.mhd
              ${TEMP}/DeformableRegistration8Test12DifferenceAfter.mhd
              ${TEMP}/DeformableRegistration8Test12DifferenceBefore.mhd
              ${TEMP}/DeformableRegistration8Test12DisplacementField.mhd
              1 1
              ${TEMP}/DeformableRegistration8Test12FinalTransformParameters.txt
              ${NUMBER_OF_BSPLINE_GRID_NODES}
      )
      set_property(TEST ITKv3DeformableRegistration8Test12 PROPERTY DEPENDS BSplineWarping2Test)
      set_property(TEST ITKv3DeformableRegistration8Test12 APPEND PROPERTY LABELS RUNS_LONG)

      # Due to the large memory requirements this tests lock the memory size resource
        set_tests_properties(
          ITKv3DeformableRegistration8Test11
          ITKv3DeformableRegistration8Test12
          PROPERTIES
          RESOURCE_LOCK MEMORY_SIZE
          )
      endif() # ITK_COMPUTER_MEMORY_SIZE GREATER 16

  endif() # ITK_COMPUTER_MEMORY_SIZE GREATER 6

endif() # ITK_USE_BRAINWEB_DATA

# This example in getting stuck and timing out, likely due to spatial
# object changes.
if (TEST_DISABLED)
itk_add_test(NAME ITKv3ImageRegistration12Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ImageRegistration12TestPixelCentered.png}
            ${TEMP}/ImageRegistration12TestPixelCentered.png
  --compareNumberOfPixelsTolerance 2
  $<TARGET_FILE:ITKv3ImageRegistration12>
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceR10X13Y17.png
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20Mask.png
        ${TEMP}/ImageRegistration12TestPixelCentered.png
)
endif()

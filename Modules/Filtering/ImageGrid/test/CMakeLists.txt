itk_module_test()
set(ITKImageGridTests
itkBasicArchitectureTest.cxx
itkBinShrinkImageFilterTest1.cxx
itkBinShrinkImageFilterTest2.cxx
itkBSplineScatteredDataPointSetToImageFilterTest.cxx
itkBSplineScatteredDataPointSetToImageFilterTest2.cxx
itkBSplineScatteredDataPointSetToImageFilterTest3.cxx
itkBSplineScatteredDataPointSetToImageFilterTest4.cxx
itkBSplineScatteredDataPointSetToImageFilterTest5.cxx
itkBSplineControlPointImageFilterTest.cxx
itkBSplineControlPointImageFunctionTest.cxx
itkChangeInformationImageFilterTest.cxx
itkConstantPadImageTest.cxx
itkCoxDeBoorBSplineKernelFunctionTest.cxx
itkCoxDeBoorBSplineKernelFunctionTest2.cxx
itkCyclicReferences.cxx
itkCyclicShiftImageFilterTest.cxx
itkInterpolateImagePointsFilterTest.cxx
itkCropImageFilterTest.cxx
itkCropImageFilter3DTest.cxx
itkFlipImageFilterTest.cxx
itkExpandImageFilterTest.cxx
itkRegionOfInterestImageFilterTest.cxx
itkBSplineUpsampleImageFilterTest.cxx
itkBSplineResampleImageFilterTest.cxx
itkBSplineDownsampleImageFilterTest.cxx
itkTileImageFilterTest.cxx
itkInterpolateImageFilterTest.cxx
itkPasteImageFilterTest.cxx
itkVectorResampleImageFilterTest.cxx
itkPermuteAxesImageFilterTest.cxx
itkOrientedImage2DTest.cxx
itkOrientedImage3DTest.cxx
itkOrientedImageAdaptorTest.cxx
itkOrientedImageProfileTest1.cxx
itkOrientedImageProfileTest2.cxx
itkOrientedImageProfileTest3.cxx
itkOrientImageFilterTest.cxx
itkOrientImageFilterTest2.cxx
itkWarpImageFilterTest.cxx
itkWarpImageFilterTest2.cxx
itkWarpVectorImageFilterTest.cxx
itkWrapPadImageTest.cxx
itkMirrorPadImageTest.cxx
itkResampleImageTest.cxx
itkResampleImageTest2.cxx
itkResampleImageTest3.cxx
itkResampleImageTest4.cxx
itkResampleImageTest5.cxx
itkResampleImageTest6.cxx
itkResamplePhasedArray3DSpecialCoordinatesImageTest.cxx
itkPushPopTileImageFilterTest.cxx
itkShrinkImageStreamingTest.cxx
itkShrinkImageTest.cxx
itkZeroFluxNeumannPadImageFilterTest.cxx
itkSliceBySliceImageFilterTest.cxx
itkPadImageFilterTest.cxx
)

CreateTestDriver(ITKImageGrid  "${ITKImageGrid-Test_LIBRARIES}" "${ITKImageGridTests}")

itk_add_test(NAME itkBasicArchitectureTest
      COMMAND ITKImageGridTestDriver itkBasicArchitectureTest)
itk_add_test(NAME itkBinShrinkImageFilterTest1
  COMMAND ${itk-module}TestDriver  itkBinShrinkImageFilterTest1 )
itk_add_test(NAME itkBinShrinkImageFilterTest2
  COMMAND ${itk-module}TestDriver  itkBinShrinkImageFilterTest2 )
itk_add_test(NAME itkBSplineScatteredDataPointSetToImageFilterTest01
      COMMAND ITKImageGridTestDriver
    --compare DATA{Baseline/itkBSplineScatteredDataPointSetToImageFilterTest01.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
    itkBSplineScatteredDataPointSetToImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cake_easy.png} ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha)
itk_add_test(NAME itkBSplineScatteredDataPointSetToImageFilterTest02
      COMMAND ITKImageGridTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest02.mha
              1ffdb3ea274ec766af8fe2bb929f9624
    itkBSplineScatteredDataPointSetToImageFilterTest2 ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest02.mha)
itk_add_test(NAME itkBSplineScatteredDataPointSetToImageFilterTest03
      COMMAND ITKImageGridTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest03.mha
              9df6a57780d641dca792877ee158c3d7
    itkBSplineScatteredDataPointSetToImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/BSplineScatteredApproximationDataPointsInput.txt} ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest03.mha)
itk_add_test(NAME itkBSplineScatteredDataPointSetToImageFilterTest04
      COMMAND ITKImageGridTestDriver itkBSplineScatteredDataPointSetToImageFilterTest4)
itk_add_test(NAME itkBSplineScatteredDataPointSetToImageFilterTest05
      COMMAND ITKImageGridTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest05.mha
              c2100a7dc86472006d77b62dffa1a8dd
    itkBSplineScatteredDataPointSetToImageFilterTest5 ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest05.mha)
itk_add_test(NAME itkBSplineControlPointImageFilterTest1
      COMMAND ITKImageGridTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_2D_output.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_2D_outputRefined.nii.gz
    itkBSplineControlPointImageFilterTest 2 DATA{Baseline/N4ControlPoints_2D.nii.gz} ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_2D_output.nii.gz ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_2D_outputRefined.nii.gz)
itk_add_test(NAME itkBSplineControlPointImageFilterTest2
      COMMAND ITKImageGridTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_3D_output.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_3D_outputRefined.nii.gz
    itkBSplineControlPointImageFilterTest 3 DATA{Baseline/N4ControlPoints_3D.nii.gz} ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_3D_output.nii.gz ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_3D_outputRefined.nii.gz)
itk_add_test(NAME itkBSplineControlPointImageFunctionTest
      COMMAND ITKImageGridTestDriver itkBSplineControlPointImageFunctionTest)
itk_add_test(NAME itkChangeInformationImageFilterTest
      COMMAND ITKImageGridTestDriver itkChangeInformationImageFilterTest)
itk_add_test(NAME itkConstantPadImageTest
      COMMAND ITKImageGridTestDriver itkConstantPadImageTest)
itk_add_test(NAME itkCoxDeBoorBSplineKernelFunctionTest01
      COMMAND ITKImageGridTestDriver itkCoxDeBoorBSplineKernelFunctionTest)
itk_add_test(NAME itkCoxDeBoorBSplineKernelFunctionTest02
      COMMAND ITKImageGridTestDriver itkCoxDeBoorBSplineKernelFunctionTest2)
itk_add_test(NAME itkCyclicReferences
      COMMAND ITKImageGridTestDriver itkCyclicReferences)
itk_add_test(NAME itkCyclicShiftImageFilterTest0
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} 0 0 )
itk_add_test(NAME itkCyclicShiftImageFilterTest1
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} 256 256 )
itk_add_test(NAME itkCyclicShiftImageFilterTest2
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} 40 0 )
itk_add_test(NAME itkCyclicShiftImageFilterTest3
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} 0 40 )
itk_add_test(NAME itkCyclicShiftImageFilterTest4
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} 40 40 )
itk_add_test(NAME itkCyclicShiftImageFilterTest5
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} 128 128 )
itk_add_test(NAME itkCyclicShiftImageFilterTest6
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} -51 40 )
itk_add_test(NAME itkInterpolateImagePointsFilterTest
      COMMAND ITKImageGridTestDriver itkInterpolateImagePointsFilterTest)
itk_add_test(NAME itkCropImageFilterTest
      COMMAND ITKImageGridTestDriver itkCropImageFilterTest)
itk_add_test(NAME itkCropImageFilter3DTest
      COMMAND ITKImageGridTestDriver itkCropImageFilter3DTest)
itk_add_test(NAME itkFlipImageFilterTest1
      COMMAND ITKImageGridTestDriver itkFlipImageFilterTest 0)
itk_add_test(NAME itkFlipImageFilterTest2
      COMMAND ITKImageGridTestDriver itkFlipImageFilterTest 1)
itk_add_test(NAME itkExpandImageFilterTest
      COMMAND ITKImageGridTestDriver itkExpandImageFilterTest)
itk_add_test(NAME itkRegionOfInterestImageFilterTest
      COMMAND ITKImageGridTestDriver itkRegionOfInterestImageFilterTest)
itk_add_test(NAME itkBSplineUpsampleImageFilterTest
      COMMAND ITKImageGridTestDriver itkBSplineUpsampleImageFilterTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/itkBSplineUpsampleImageFilterTest1.mha 3)

itk_add_test(NAME itkBSplineResampleImageFilterTest
      COMMAND ITKImageGridTestDriver
        --redirectOutput ${ITK_TEST_OUTPUT_DIR}/itkBSplineResampleImageFilterTest
    itkBSplineResampleImageFilterTest)
set_tests_properties(itkBSplineResampleImageFilterTest PROPERTIES ATTACHED_FILES_ON_FAIL ${ITK_TEST_OUTPUT_DIR}/itkBSplineResampleImageFilterTest.txt)

itk_add_test(NAME itkWarpImageFilterTest2
      COMMAND ITKImageGridTestDriver itkWarpImageFilterTest2)
itk_add_test(NAME itkBSplineDownsampleImageFilterTest
      COMMAND ITKImageGridTestDriver itkBSplineDownsampleImageFilterTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/itkBSplineDownsampleImageFilterTest1.mha 3)
itk_add_test(NAME itkTileImageFilterTest
      COMMAND ITKImageGridTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/TileImageFilterTest4.png}
              ${ITK_TEST_OUTPUT_DIR}/TileImageFilterTest4.png
    itkTileImageFilterTest 1 2 0 DATA{${ITK_DATA_ROOT}/Input/STAPLE1.png} DATA{${ITK_DATA_ROOT}/Input/STAPLE2.png} DATA{${ITK_DATA_ROOT}/Input/STAPLE3.png} DATA{${ITK_DATA_ROOT}/Input/STAPLE4.png} DATA{${ITK_DATA_ROOT}/Input/Shapes.png} DATA{${ITK_DATA_ROOT}/Input/Spots.png} DATA{${ITK_DATA_ROOT}/Input/SpotsInverted.png} DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSliceInitialLevelSet.png} DATA{${ITK_DATA_ROOT}/Input/cake_easy.png} DATA{${ITK_DATA_ROOT}/Input/cake_hard.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/sf4.png} DATA{${ITK_DATA_ROOT}/Input/smooth_circle.png} DATA{${ITK_DATA_ROOT}/Input/smooth_square.png} ${ITK_TEST_OUTPUT_DIR}/TileImageFilterTest%d.png)
itk_add_test(NAME itkInterpolateImageFilterTest
      COMMAND ITKImageGridTestDriver itkInterpolateImageFilterTest)
itk_add_test(NAME itkPasteImageFilterTest
      COMMAND ITKImageGridTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/PasteImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png
    itkPasteImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cake_easy.png} ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png)
itk_add_test(NAME itkVectorResampleImageFilterTest
      COMMAND ITKImageGridTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorResampleImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png
    itkVectorResampleImageFilterTest ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png)
itk_add_test(NAME itkPermuteAxesImageFilterTest
      COMMAND ITKImageGridTestDriver itkPermuteAxesImageFilterTest)
itk_add_test(NAME itkOrientedImage2DTest01
      COMMAND ITKImageGridTestDriver itkOrientedImage2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd 0.000 0.000 191.392 110.500 -128.500 222.569 4.000 -8.000 7.4641 -4.9282)
itk_add_test(NAME itkOrientedImage3DTest01
      COMMAND ITKImageGridTestDriver itkOrientedImage3DTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd} 0.000 0.000 0.000 166.277 -96.000 0.000 124.000 214.774 0.000 0.000 0.000 168.000 0.125 -4.125 9.000 -1.95425 -3.63485 9.000)
itk_add_test(NAME itkOrientedImageAdaptorTest
      COMMAND ITKImageGridTestDriver itkOrientedImageAdaptorTest)
itk_add_test(NAME itkOrientedImageProfileTest1
      COMMAND ITKImageGridTestDriver itkOrientedImageProfileTest1)
itk_add_test(NAME itkOrientedImageProfileTest2
      COMMAND ITKImageGridTestDriver itkOrientedImageProfileTest2)
itk_add_test(NAME itkOrientedImageProfileTest3
      COMMAND ITKImageGridTestDriver itkOrientedImageProfileTest3)
itk_add_test(NAME itkOrientImageFilterTest
      COMMAND ITKImageGridTestDriver itkOrientImageFilterTest)
itk_add_test(NAME itkOrientImageFilterTest2
      COMMAND ITKImageGridTestDriver itkOrientImageFilterTest2)
itk_add_test(NAME itkWarpImageFilterTest
      COMMAND ITKImageGridTestDriver itkWarpImageFilterTest)
itk_add_test(NAME itkWarpVectorImageFilterTest
      COMMAND ITKImageGridTestDriver itkWarpVectorImageFilterTest)
itk_add_test(NAME itkWrapPadImageTest
      COMMAND ITKImageGridTestDriver itkWrapPadImageTest)
itk_add_test(NAME itkMirrorPadImageTest
      COMMAND ITKImageGridTestDriver itkMirrorPadImageTest)
itk_add_test(NAME itkResampleImageTest
      COMMAND ITKImageGridTestDriver itkResampleImageTest)
itk_add_test(NAME itkResampleImageTest2
      COMMAND ITKImageGridTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ResampleImageTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2a.png
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ResampleImageTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2b.png
    --compare DATA{Baseline/ResampleImageTest2NearestExtrapolate.png}
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2c.png
    --compare DATA{Baseline/ResampleImageTest2NearestExtrapolate.png}
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2d.png
    itkResampleImageTest2 DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
                          DATA{${ITK_DATA_ROOT}/Input/circle.png}
                          ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2a.png
                          ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2b.png
                          ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2c.png
                          ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2d.png)
itk_add_test(NAME itkResampleImageTest3
      COMMAND ITKImageGridTestDriver
    --compare DATA{Baseline/ResampleImageTest3.png}
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest3.png
    itkResampleImageTest3 DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
                          ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest3.png)
itk_add_test(NAME itkResampleImageTest4
      COMMAND ITKImageGridTestDriver itkResampleImageTest4)
itk_add_test(NAME itkResampleImageTest5
      COMMAND ITKImageGridTestDriver
    --compare DATA{Baseline/ResampleImageTest5.png}
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest5.png
    itkResampleImageTest5 10 ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest5.png)
itk_add_test(NAME itkResampleImageTest6
      COMMAND ITKImageGridTestDriver
    --compare DATA{Baseline/ResampleImageTest6.png}
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest6.png
    itkResampleImageTest6 10 ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest6.png)
itk_add_test(NAME itkResamplePhasedArray3DSpecialCoordinatesImageTest
      COMMAND ITKImageGridTestDriver itkResamplePhasedArray3DSpecialCoordinatesImageTest)
itk_add_test(NAME itkPushPopTileImageFilterTest
      COMMAND ITKImageGridTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/PushPopTileImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png
    itkPushPopTileImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Shapes.png} DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} DATA{${ITK_DATA_ROOT}/Input/cake_easy.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png)
itk_add_test(NAME itkShrinkImageStreamingTest
      COMMAND ITKImageGridTestDriver itkShrinkImageStreamingTest)
itk_add_test(NAME itkShrinkImageTest
      COMMAND ITKImageGridTestDriver itkShrinkImageTest)
itk_add_test(NAME itkZeroFluxNeumannPadImageFilterTest
      COMMAND ITKImageGridTestDriver itkZeroFluxNeumannPadImageFilterTest)
itk_add_test(NAME itkSliceBySliceImageFilterDimension0Test
      COMMAND ITKImageGridTestDriver itkSliceBySliceImageFilterTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/itkSliceBySliceImageFilterDimension0Test.mha 0)
itk_add_test(NAME itkSliceBySliceImageFilterDimension1Test
      COMMAND ITKImageGridTestDriver itkSliceBySliceImageFilterTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/itkSliceBySliceImageFilterDimension1Test.mha 1)
itk_add_test(NAME itkSliceBySliceImageFilterDimension2Test
      COMMAND ITKImageGridTestDriver itkSliceBySliceImageFilterTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/itkSliceBySliceImageFilterDimension2Test.mha 2)
itk_add_test(NAME itkPadImageFilterTest
      COMMAND ITKImageGridTestDriver itkPadImageFilterTest)

set( ITKImageGridGTests
  itkSliceImageFilterTest.cxx )
CreateGoogleTestDriver(ITKImageGrid "${ITKImageGrid-Test_LIBRARIES}" "${ITKImageGridGTests}")

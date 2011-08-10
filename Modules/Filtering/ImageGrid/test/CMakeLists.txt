itk_module_test()
set(ITKImageGridTests
itkBasicArchitectureTest.cxx
itkBSplineScatteredDataPointSetToImageFilterTest.cxx
itkBSplineScatteredDataPointSetToImageFilterTest2.cxx
itkBSplineScatteredDataPointSetToImageFilterTest3.cxx
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
itkResamplePhasedArray3DSpecialCoordinatesImageTest.cxx
itkPushPopTileImageFilterTest.cxx
itkShrinkImagePreserveObjectPhysicalLocations.cxx
itkShrinkImageStreamingTest.cxx
itkShrinkImageTest.cxx
itkZeroFluxNeumannPadImageFilterTest.cxx
itkSliceBySliceImageFilterTest.cxx
itkPadImageFilterTest.cxx
)

CreateTestDriver(ITKImageGrid  "${ITKImageGrid-Test_LIBRARIES}" "${ITKImageGridTests}")

itk_add_test(NAME itkBasicArchitectureTest
      COMMAND ITKImageGridTestDriver itkBasicArchitectureTest)
itk_add_test(NAME itkBSplineScatteredDataPointSetToImageFilterTest01
      COMMAND ITKImageGridTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
    itkBSplineScatteredDataPointSetToImageFilterTest ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha)
itk_add_test(NAME itkBSplineScatteredDataPointSetToImageFilterTest02
      COMMAND ITKImageGridTestDriver itkBSplineScatteredDataPointSetToImageFilterTest2)
itk_add_test(NAME itkBSplineScatteredDataPointSetToImageFilterTest03
      COMMAND ITKImageGridTestDriver itkBSplineScatteredDataPointSetToImageFilterTest3
              ${ITK_DATA_ROOT}/Input/BSplineScatteredApproximationDataPointsInput.txt)
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
      itkCyclicShiftImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png 0 0 )
itk_add_test(NAME itkCyclicShiftImageFilterTest1
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png 256 256 )
itk_add_test(NAME itkCyclicShiftImageFilterTest2
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png 40 0 )
itk_add_test(NAME itkCyclicShiftImageFilterTest3
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png 0 40 )
itk_add_test(NAME itkCyclicShiftImageFilterTest4
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png 40 40 )
itk_add_test(NAME itkCyclicShiftImageFilterTest5
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png 128 128 )
itk_add_test(NAME itkCyclicShiftImageFilterTest6
      COMMAND ITKImageGridTestDriver
      itkCyclicShiftImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png -51 40 )
itk_add_test(NAME itkInterpolateImagePointsFilterTest
      COMMAND ITKImageGridTestDriver itkInterpolateImagePointsFilterTest)
itk_add_test(NAME itkCropImageFilterTest
      COMMAND ITKImageGridTestDriver itkCropImageFilterTest)
itk_add_test(NAME itkCropImageFilter3DTest
      COMMAND ITKImageGridTestDriver itkCropImageFilter3DTest)
itk_add_test(NAME itkFlipImageFilterTest
      COMMAND ITKImageGridTestDriver itkFlipImageFilterTest)
itk_add_test(NAME itkExpandImageFilterTest
      COMMAND ITKImageGridTestDriver itkExpandImageFilterTest)
itk_add_test(NAME itkRegionOfInterestImageFilterTest
      COMMAND ITKImageGridTestDriver itkRegionOfInterestImageFilterTest)
itk_add_test(NAME itkBSplineUpsampleImageFilterTest
      COMMAND ITKImageGridTestDriver itkBSplineUpsampleImageFilterTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkBSplineUpsampleImageFilterTest1.mha 3)
itk_add_test(NAME itkBSplineResampleImageFilterTest
      COMMAND ITKImageGridTestDriver itkBSplineResampleImageFilterTest)
itk_add_test(NAME itkWarpImageFilterTest2
      COMMAND ITKImageGridTestDriver itkWarpImageFilterTest2)
itk_add_test(NAME itkBSplineDownsampleImageFilterTest
      COMMAND ITKImageGridTestDriver itkBSplineDownsampleImageFilterTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkBSplineDownsampleImageFilterTest1.mha 3)
itk_add_test(NAME itkTileImageFilterTest
      COMMAND ITKImageGridTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/TileImageFilterTest4.png
              ${ITK_TEST_OUTPUT_DIR}/TileImageFilterTest4.png
    itkTileImageFilterTest 1 2 0 ${ITK_DATA_ROOT}/Input/STAPLE1.png ${ITK_DATA_ROOT}/Input/STAPLE2.png ${ITK_DATA_ROOT}/Input/STAPLE3.png ${ITK_DATA_ROOT}/Input/STAPLE4.png ${ITK_DATA_ROOT}/Input/Shapes.png ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSliceInitialLevelSet.png ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_DATA_ROOT}/Input/cake_hard.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_DATA_ROOT}/Input/smooth_circle.png ${ITK_DATA_ROOT}/Input/smooth_square.png ${ITK_TEST_OUTPUT_DIR}/TileImageFilterTest%d.png)
itk_add_test(NAME itkInterpolateImageFilterTest
      COMMAND ITKImageGridTestDriver itkInterpolateImageFilterTest)
itk_add_test(NAME itkPasteImageFilterTest
      COMMAND ITKImageGridTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PasteImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png
    itkPasteImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png)
itk_add_test(NAME itkVectorResampleImageFilterTest
      COMMAND ITKImageGridTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorResampleImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png
    itkVectorResampleImageFilterTest ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png)
itk_add_test(NAME itkPermuteAxesImageFilterTest
      COMMAND ITKImageGridTestDriver itkPermuteAxesImageFilterTest)
itk_add_test(NAME itkOrientedImage2DTest01
      COMMAND ITKImageGridTestDriver itkOrientedImage2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd 0.000 0.000 191.392 110.500 -128.500 222.569 4.000 -8.000 7.4641 -4.9282)
itk_add_test(NAME itkOrientedImage3DTest01
      COMMAND ITKImageGridTestDriver itkOrientedImage3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd 0.000 0.000 0.000 166.277 -96.000 0.000 124.000 214.774 0.000 0.000 0.000 168.000 0.125 -4.125 9.000 -1.95425 -3.63485 9.000)
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
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ResampleImageTest2.png
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2a.png
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ResampleImageTest2.png
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2b.png
    --compare DATA{Baseline/ResampleImageTest2NearestExtrapolate.png}
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2c.png
    --compare DATA{Baseline/ResampleImageTest2NearestExtrapolate.png}
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2d.png
    itkResampleImageTest2 ${ITK_DATA_ROOT}/Input/cthead1.png
                          ${ITK_DATA_ROOT}/Input/circle.png
                          ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2a.png
                          ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2b.png
                          ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2c.png
                          ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2d.png)
itk_add_test(NAME itkResamplePhasedArray3DSpecialCoordinatesImageTest
      COMMAND ITKImageGridTestDriver itkResamplePhasedArray3DSpecialCoordinatesImageTest)
itk_add_test(NAME itkPushPopTileImageFilterTest
      COMMAND ITKImageGridTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PushPopTileImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png
    itkPushPopTileImageFilterTest ${ITK_DATA_ROOT}/Input/Shapes.png ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png)
itk_add_test(NAME itkShrinkImagePreserveObjectPhysicalLocations
      COMMAND ITKImageGridTestDriver itkShrinkImagePreserveObjectPhysicalLocations)
itk_add_test(NAME itkShrinkImageStreamingTest
      COMMAND ITKImageGridTestDriver itkShrinkImageStreamingTest)
itk_add_test(NAME itkShrinkImageTest
      COMMAND ITKImageGridTestDriver itkShrinkImageTest)
itk_add_test(NAME itkZeroFluxNeumannPadImageFilterTest
      COMMAND ITKImageGridTestDriver itkZeroFluxNeumannPadImageFilterTest)
itk_add_test(NAME itkSliceBySliceImageFilterTest
      COMMAND ITKImageGridTestDriver itkSliceBySliceImageFilterTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkSliceBySliceImageFilterTest.mha)
itk_add_test(NAME itkPadImageFilterTest
      COMMAND ITKImageGridTestDriver itkPadImageFilterTest)

itk_module_test()
set(ITK-ImageGridTests
itkBasicArchitectureTest.cxx
itkChangeInformationImageFilterTest.cxx
itkConstantPadImageTest.cxx
itkCyclicReferences.cxx
itkInterpolateImagePointsFilterTest.cxx
itkCropImageFilterTest.cxx
itkFlipImageFilterTest.cxx
itkExpandImageFilterTest.cxx
itkRegionOfInterestImageFilterTest.cxx
itkBSplineUpsampleImageFilterTest.cxx
itkBSplineResampleImageFilterTest.cxx
itkBSplineResampleImageFunctionTest.cxx
itkBSplineInterpolateImageFunctionTest.cxx
itkImageGridHeaderTest.cxx
itkWarpImageFilterTest2.cxx
itkBSplineDecompositionImageFilterTest.cxx
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
itkWarpVectorImageFilterTest.cxx
itkWrapPadImageTest.cxx
)

CreateTestDriver(ITK-ImageGrid  "${ITK-ImageGrid-Test_LIBRARIES}" "${ITK-ImageGridTests}")

itk_add_test(NAME itkImageGridHeaderTest
      COMMAND ITK-ImageGridTestDriver itkImageGridHeaderTest)
itk_add_test(NAME itkBasicArchitectureTest
      COMMAND ITK-ImageGridTestDriver itkBasicArchitectureTest)
itk_add_test(NAME itkChangeInformationImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkChangeInformationImageFilterTest)
itk_add_test(NAME itkConstantPadImageTest
      COMMAND ITK-ImageGridTestDriver itkConstantPadImageTest)
itk_add_test(NAME itkCyclicReferences
      COMMAND ITK-ImageGridTestDriver itkCyclicReferences)
itk_add_test(NAME itkInterpolateImagePointsFilterTest
      COMMAND ITK-ImageGridTestDriver itkInterpolateImagePointsFilterTest)
itk_add_test(NAME itkCropImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkCropImageFilterTest)
itk_add_test(NAME itkFlipImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkFlipImageFilterTest)
itk_add_test(NAME itkExpandImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkExpandImageFilterTest)
itk_add_test(NAME itkRegionOfInterestImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkRegionOfInterestImageFilterTest)
itk_add_test(NAME itkBSplineUpsampleImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkBSplineUpsampleImageFilterTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkBSplineUpsampleImageFilterTest1.mha 3)
itk_add_test(NAME itkBSplineResampleImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkBSplineResampleImageFilterTest)
itk_add_test(NAME itkBSplineResampleImageFunctionTest
      COMMAND ITK-ImageGridTestDriver itkBSplineResampleImageFunctionTest)
itk_add_test(NAME itkBSplineInterpolateImageFunctionTest
      COMMAND ITK-ImageGridTestDriver itkBSplineInterpolateImageFunctionTest)
itk_add_test(NAME itkWarpImageFilterTest2
      COMMAND ITK-ImageGridTestDriver itkWarpImageFilterTest2)
itk_add_test(NAME itkBSplineDecompositionImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkBSplineDecompositionImageFilterTest)
itk_add_test(NAME itkBSplineDownsampleImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkBSplineDownsampleImageFilterTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkBSplineDownsampleImageFilterTest1.mha 3)
itk_add_test(NAME itkTileImageFilterTest
      COMMAND ITK-ImageGridTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/TileImageFilterTest4.png
              ${ITK_TEST_OUTPUT_DIR}/TileImageFilterTest4.png
    itkTileImageFilterTest 1 2 0 ${ITK_DATA_ROOT}/Input/STAPLE1.png ${ITK_DATA_ROOT}/Input/STAPLE2.png ${ITK_DATA_ROOT}/Input/STAPLE3.png ${ITK_DATA_ROOT}/Input/STAPLE4.png ${ITK_DATA_ROOT}/Input/Shapes.png ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSliceInitialLevelSet.png ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_DATA_ROOT}/Input/cake_hard.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_DATA_ROOT}/Input/smooth_circle.png ${ITK_DATA_ROOT}/Input/smooth_square.png ${ITK_TEST_OUTPUT_DIR}/TileImageFilterTest%d.png)
itk_add_test(NAME itkInterpolateImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkInterpolateImageFilterTest)
itk_add_test(NAME itkPasteImageFilterTest
      COMMAND ITK-ImageGridTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PasteImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png
    itkPasteImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png)
itk_add_test(NAME itkVectorResampleImageFilterTest
      COMMAND ITK-ImageGridTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorResampleImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png
    itkVectorResampleImageFilterTest ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png)
itk_add_test(NAME itkPermuteAxesImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkPermuteAxesImageFilterTest)
itk_add_test(NAME itkOrientedImage2DTest01
      COMMAND ITK-ImageGridTestDriver itkOrientedImage2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd 0.000 0.000 191.392 110.500 -128.500 222.569 4.000 -8.000 7.4641 -4.9282)
itk_add_test(NAME itkOrientedImage3DTest01
      COMMAND ITK-ImageGridTestDriver itkOrientedImage3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd 0.000 0.000 0.000 166.277 -96.000 0.000 124.000 214.774 0.000 0.000 0.000 168.000 0.125 -4.125 9.000 -1.95425 -3.63485 9.000)
itk_add_test(NAME itkOrientedImageAdaptorTest
      COMMAND ITK-ImageGridTestDriver itkOrientedImageAdaptorTest)
itk_add_test(NAME itkOrientedImageProfileTest1
      COMMAND ITK-ImageGridTestDriver itkOrientedImageProfileTest1)
itk_add_test(NAME itkOrientedImageProfileTest2
      COMMAND ITK-ImageGridTestDriver itkOrientedImageProfileTest2)
itk_add_test(NAME itkOrientedImageProfileTest3
      COMMAND ITK-ImageGridTestDriver itkOrientedImageProfileTest3)
itk_add_test(NAME itkOrientImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkOrientImageFilterTest)
itk_add_test(NAME itkOrientImageFilterTest2
      COMMAND ITK-ImageGridTestDriver itkOrientImageFilterTest2)
itk_add_test(NAME itkWarpImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkWarpImageFilterTest)
itk_add_test(NAME itkWarpVectorImageFilterTest
      COMMAND ITK-ImageGridTestDriver itkWarpVectorImageFilterTest)
itk_add_test(NAME itkWrapPadImageTest
      COMMAND ITK-ImageGridTestDriver itkWrapPadImageTest)

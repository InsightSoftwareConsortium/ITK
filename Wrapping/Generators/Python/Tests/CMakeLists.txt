# TODO: drop -E option once all filters/spatial objects/cost functions are actually wrapped
itk_python_add_test(NAME NonBlockingPythonFilterCoverage COMMAND wrappingCoverage.py -E -b Filter -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-filters.txt ${ITK_INCLUDE_DIRS})
itk_python_add_test(NAME NonBlockingPythonSpatialObjectCoverage COMMAND wrappingCoverage.py -E -b SpatialObject ${ITK_INCLUDE_DIRS})
itk_python_add_test(NAME NonBlockingPythonCostFunctionCoverage COMMAND wrappingCoverage.py -E -b CostFunction ${ITK_INCLUDE_DIRS})

itk_python_add_test(NAME PythonImageIOCoverage COMMAND wrappingCoverage.py -b ImageIO -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-imageio.txt ${ITK_INCLUDE_DIRS})
itk_python_add_test(NAME PythonOptimizerCoverage COMMAND wrappingCoverage.py -b Optimizer ${ITK_INCLUDE_DIRS})
itk_python_add_test(NAME PythonTransformCoverage COMMAND wrappingCoverage.py -b Transform -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-transform.txt ${ITK_INCLUDE_DIRS})

# These two tests can use up all of system memory when the are executed.
# TODO: find the root cause and re-enable them.  See ITK-3006 on issues.itk.org.
#itk_python_add_test(PythonFindEmptyClasses findEmptyClasses.py)
itk_python_add_test(NAME PythonGetNameOfClass COMMAND getNameOfClass.py)

itk_python_add_test(NAME PythonTiming COMMAND timing.py)


# some tests will fail if dim=2 and unsigned short are not wrapped
INTERSECTION(WRAP_2 2 "${ITK_WRAP_IMAGE_DIMS}")
if(ITK_WRAP_unsigned_char AND WRAP_2)
  itk_python_add_test(NAME PythonTypemaps COMMAND typemaps.py)

  itk_python_add_test(NAME PythonTemplate COMMAND template.py)

  itk_python_add_test(NAME PythonAutoPipeline
    TEST_DRIVER_ARGS
      --compare auto_pipeline.png DATA{${WrapITK_SOURCE_DIR}/images/MedianImageFilter.png}
    COMMAND auto_pipeline.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      auto_pipeline.png
      3
    )

  itk_python_add_test(NAME PythonTemplatedPipeline
    TEST_DRIVER_ARGS
      --compare templated_pipeline.png DATA{${WrapITK_SOURCE_DIR}/images/templated_pipeline.png}
    COMMAND templated_pipeline.py
      DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
      templated_pipeline.png
      10
    )

  itk_python_add_test(NAME PythonExtras
    COMMAND extras.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png} out.png
    )

  itk_python_add_test(NAME PythonModuleToModule
    COMMAND module2module.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      5
    )

  itk_python_add_test(NAME PythonBinaryDilateImageFilter
    TEST_DRIVER_ARGS
      --compare BinaryDilateImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryDilateImageFilter.png}
    COMMAND BinaryDilateImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
      BinaryDilateImageFilter.png
      5
    )

  itk_python_add_test(NAME PythonBinaryErodeImageFilter
    TEST_DRIVER_ARGS
      --compare BinaryErodeImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryErodeImageFilter.png}
    COMMAND BinaryErodeImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
      BinaryErodeImageFilter.png
      5
    )

  itk_python_add_test(NAME PythonBinaryThresholdImageFilter
    TEST_DRIVER_ARGS
      --compare BinaryThresholdImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryThresholdImageFilter.png}
    COMMAND BinaryThresholdImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      BinaryThresholdImageFilter.png
      50 150
      0 255
    )

  itk_python_add_test(NAME PythonCastImageFilter
    TEST_DRIVER_ARGS
      --compare CastImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    COMMAND CastImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      CastImageFilter.png
    )

  itk_python_add_test(NAME PythonGrayscaleDilateImageFilter
    TEST_DRIVER_ARGS
      --compare GrayscaleDilateImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/GrayscaleDilateImageFilter.png}
    COMMAND GrayscaleDilateImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      GrayscaleDilateImageFilter.png
      5
    )

  itk_python_add_test(NAME PythonBoxGrayscaleDilateImageFilter
    TEST_DRIVER_ARGS
      --compare BoxGrayscaleDilateImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BoxGrayscaleDilateImageFilter.png}
    COMMAND BoxGrayscaleDilateImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      BoxGrayscaleDilateImageFilter.png
    )

#   itk_python_add_test(NAME PythonStrelFromImageGrayscaleDilateImageFilter
#     StrelFromImageGrayscaleDilateImageFilter.py
#     DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
#     StrelFromImageGrayscaleDilateImageFilter.png
#     DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImage.png}
#     --compare StrelFromImageGrayscaleDilateImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImageGrayscaleDilateImageFilter.png}
#   )

  itk_python_add_test(NAME PythonGrayscaleErodeImageFilter
    TEST_DRIVER_ARGS
      --compare GrayscaleErodeImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/GrayscaleErodeImageFilter.png}
    COMMAND GrayscaleErodeImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      GrayscaleErodeImageFilter.png
      5
    )

#   itk_python_add_test(NAME PythonFlatStructuringElementBall
#     FlatStructuringElement.py
#     Ball-5.png
#     Ball
#     5
#     --compare Ball-5.png DATA{${WrapITK_SOURCE_DIR}/images/Ball-5.png}
#   )

#   itk_python_add_test(NAME PythonFlatStructuringElementBox
#     FlatStructuringElement.py
#     Box-5.png
#     Box
#     5
#     --compare Box-5.png DATA{${WrapITK_SOURCE_DIR}/images/Box-5.png}
#   )

#   itk_python_add_test(NAME PythonFlatStructuringElementFromImage
#     FlatStructuringElement.py
#     StrelFromImage.png
#     FromImage
#     DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImage.png}
#     --compare StrelFromImage.png DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImage.png}
#   )

  itk_python_add_test(NAME PythonMeanImageFilter
    TEST_DRIVER_ARGS
      --compare MeanImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/MeanImageFilter.png}
    COMMAND MeanImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      MeanImageFilter.png
      3
    )

  itk_python_add_test(NAME PythonMedianImageFilter
    TEST_DRIVER_ARGS
      --compare MedianImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/MedianImageFilter.png}
    COMMAND MedianImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      MedianImageFilter.png
      3
    )

  itk_python_add_test(NAME PythonSigmoidImageFilter
    TEST_DRIVER_ARGS
      --compare SigmoidImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/SigmoidImageFilter.png}
    COMMAND SigmoidImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      SigmoidImageFilter.png
      10 240 10 170
    )

  itk_python_add_test(NAME PythonThresholdImageFilter
    TEST_DRIVER_ARGS
      --compare ThresholdImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/ThresholdImageFilter.png}
    COMMAND ThresholdImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      ThresholdImageFilter.png
      150 150
    )

  itk_python_add_test(NAME PythonResampleImageFilterTest1
    TEST_DRIVER_ARGS
    --compare ResampleImageFilterTest1.png
              DATA{${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest1.png}
    COMMAND ResampleImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      ResampleImageFilterTest1.png
      0
    )

  itk_python_add_test(NAME PythonResampleImageFilterTest2
    TEST_DRIVER_ARGS
      --compare ResampleImageFilterTest2.png
                DATA{${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest2.png}
    COMMAND ResampleImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      ResampleImageFilterTest2.png
      1
    )

  itk_python_add_test(NAME PythonResampleImageFilterTest3
    TEST_DRIVER_ARGS
      --compare ResampleImageFilterTest3.png
                DATA{${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest3.png}
    COMMAND ResampleImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      ResampleImageFilterTest3.png
      2
    )

  itk_python_add_test(NAME PythonResampleImageFilterTest4
    TEST_DRIVER_ARGS
      --compare ResampleImageFilterTest4.png
                DATA{${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest4.png}
    COMMAND ResampleImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      ResampleImageFilterTest4.png
      3
    )
endif()

itk_python_add_test(NAME PythonLazyLoadingImage COMMAND LazyLoadingImage.py)

if(ITK_WRAP_float AND WRAP_2)
  itk_python_add_test(NAME PythonCurvatureAnisotropicDiffusionImageFilter
    COMMAND CurvatureAnisotropicDiffusionImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      CurvatureAnisotropicDiffusionImageFilter.png
      5 0.125 3
    )

  itk_python_add_test(NAME PythonCurvatureFlowImageFilter
    COMMAND CurvatureFlowImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      CurvatureFlowImageFilter.png
      5 0.125 1.0
    )

  itk_python_add_test(NAME PythonSmoothingRecursiveGaussianImageFilter
    COMMAND SmoothingRecursiveGaussianImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      SmoothingRecursiveGaussianImageFilter.png
      3
    )

  itk_python_add_test(NAME PythonLaplacianImageFilter
    COMMAND LaplacianImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      LaplacianImageFilter.png
    )

  itk_python_add_test(NAME PythonFFTImageFilter
    TEST_DRIVER_ARGS
      --compare FFTImageFilterReal.png DATA{${WrapITK_SOURCE_DIR}/images/FFTImageFilterReal.png}
      --compare FFTImageFilterImaginary.png DATA{${WrapITK_SOURCE_DIR}/images/FFTImageFilterImaginary.png}
    COMMAND FFTImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      FFTImageFilterReal.png
      FFTImageFilterImaginary.png
  )

  itk_python_add_test(NAME  PythonGradientMagnitudeRecursiveGaussianImageFilterTest
    TEST_DRIVER_ARGS
      --compare GradientMagnitudeRecursiveGaussianImageFilter.png
              DATA{${WrapITK_SOURCE_DIR}/images/GradientMagnitudeRecursiveGaussianImageFilter.png}
    COMMAND GradientMagnitudeRecursiveGaussianImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      GradientMagnitudeRecursiveGaussianImageFilter.png
      5
    )

  itk_python_add_test(NAME  PythonGradientAnisotropicDiffusionImageFilterTest
    TEST_DRIVER_ARGS
      --compare GradientAnisotropicDiffusionImageFilter.png
                DATA{${WrapITK_SOURCE_DIR}/images/GradientAnisotropicDiffusionImageFilter.png}
    COMMAND GradientAnisotropicDiffusionImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      GradientAnisotropicDiffusionImageFilter.png
      5 0.125 1.0
    )


# TODO: reenable the geodesic test once we get why the result is not the
# same than with c++
# keep just one to be sure it run, but don't compare the images

  itk_python_add_test(NAME PythonGeodesicActiveContourLeftVentricleTest
#     --compare GeodesicActiveContourLeftVentricleTest.png
#             DATA{${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourLeftVentricleTest.png}
    COMMAND GeodesicActiveContourImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      GeodesicActiveContourLeftVentricleTest.png
      81 114 5.0 1.0 -0.5 3.0 2.0
    )

#   itk_python_add_test(NAME  PythonGeodesicActiveContourWhiteMatterTest
#     --compare GeodesicActiveContourWhiteMatterTest.png
#             DATA{${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourWhiteMatterTest.png}
#     --
#     GeodesicActiveContourImageFilter.py
#     DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
#     GeodesicActiveContourWhiteMatterTest.png
#     56 92 5.0 1.0 -0.3 2.0 10.0
#   )

#   itk_python_add_test(NAME  PythonGeodesicActiveContourGrayMatterTest
#     --compare GeodesicActiveContourGrayMatterTest.png
#             DATA{${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourGrayMatterTest.png}
#     --
#     GeodesicActiveContourImageFilter.py
#     DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
#     GeodesicActiveContourGrayMatterTest.png
#     40 90 5.0 .5 -0.3 2.0 10.0
#   )


  itk_python_add_test(NAME PythonFastMarchingLeftVentricleTest
    TEST_DRIVER_ARGS
      --compare FastMarchingLeftVentricleTest.png
                DATA{${WrapITK_SOURCE_DIR}/images/FastMarchingLeftVentricleTest.png}
    COMMAND FastMarchingImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      FastMarchingLeftVentricleTest.png
      81 114 1.0 -0.5 3.0 100 100
    )

  itk_python_add_test(NAME PythonFastMarchingRightVentricleTest
    TEST_DRIVER_ARGS
      --compare FastMarchingRightVentricleTest.png
                DATA{${WrapITK_SOURCE_DIR}/images/FastMarchingRightVentricleTest.png}
    COMMAND FastMarchingImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      FastMarchingRightVentricleTest.png
      99 114 1.0 -0.5 3.0 100 100
    )

  itk_python_add_test(NAME PythonFastMarchingWhiteMatterTest
    TEST_DRIVER_ARGS
      --compare FastMarchingWhiteMatterTest.png
                DATA{${WrapITK_SOURCE_DIR}/images/FastMarchingWhiteMatterTest.png}
    COMMAND FastMarchingImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      FastMarchingWhiteMatterTest.png
      56 92 1.0 -0.3 2.0 200 100
    )

  itk_python_add_test(NAME PythonFastMarchingGrayMatterTest
    TEST_DRIVER_ARGS
      --compare FastMarchingGrayMatterTest.png
                DATA{${WrapITK_SOURCE_DIR}/images/FastMarchingGrayMatterTest.png}
    COMMAND FastMarchingImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      FastMarchingGrayMatterTest.png
      40 90 .5 -0.3 2.0 200 100
    )

  itk_python_add_test(NAME PythonThresholdSegmentationLevelSetVentricleTest
    TEST_DRIVER_ARGS
    --compare ThresholdSegmentationLevelSetVentricleTest.png
              DATA{${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetVentricleTest.png}
    COMMAND ThresholdSegmentationLevelSetImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      ThresholdSegmentationLevelSetVentricleTest.png
      81 112 5 210 250
    )

  itk_python_add_test(NAME PythonThresholdSegmentationLevelSetWhiteMatterTest
    TEST_DRIVER_ARGS
      --compare ThresholdSegmentationLevelSetWhiteMatterTest.png
                DATA{${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetWhiteMatterTest.png}
    COMMAND ThresholdSegmentationLevelSetImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      ThresholdSegmentationLevelSetWhiteMatterTest.png
      60 116 5 150 180
    )

  itk_python_add_test(NAME PythonThresholdSegmentationLevelSetGrayMatterTest
    TEST_DRIVERS_ARGS
      --compare ThresholdSegmentationLevelSetGrayMatterTest.png
                DATA{${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetGrayMatterTest.png}
    COMMAND ThresholdSegmentationLevelSetImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      ThresholdSegmentationLevelSetGrayMatterTest.png
      107 69 5 180 210
    )

  itk_python_add_test(NAME PythonCannyEdgeDetectionImageFilterTest
    TEST_DRIVER_ARGS
      --compareNumberOfPixelsTolerance 1
      --compare CannyEdgeDetectionImageFilter.png
                DATA{${WrapITK_SOURCE_DIR}/images/CannyEdgeDetectionImageFilter.png}
    COMMAND CannyEdgeDetectionImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      CannyEdgeDetectionImageFilter.png
      9
    )

  itk_python_add_test(NAME PythonWatershedSegmentation1
    TEST_DRIVER_ARGS
      --compare WatershedSegmentation1.png
                DATA{${WrapITK_SOURCE_DIR}/images/WatershedSegmentation1.png}
    COMMAND WatershedSegmentation1.py
      DATA{${WrapITK_SOURCE_DIR}/images/VisibleWomanEyeSlice.png}
      WatershedSegmentation1.png
    )

  itk_python_add_test(NAME PythonSpatialObject
    TEST_DRIVER_ARGS
      --compare SpatialObject.png
                DATA{${WrapITK_SOURCE_DIR}/images/SpatialObject.png}
    COMMAND SpatialObjectTest.py
          SpatialObject.png
    )

  itk_python_add_test(NAME PythonReadDicomAndReadTag
    COMMAND ReadDicomAndReadTag.py
      DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
  )
endif()


INTERSECTION(WRAP_3 3 "${ITK_WRAP_IMAGE_DIMS}")
if(ITK_WRAP_float AND WRAP_3)
  itk_python_add_test(NAME PythonAntiAliasBinaryImageFilterTest
    TEST_DRIVER_ARGS
      --compare AntiAliasBinaryImageFilter.png
                DATA{${WrapITK_SOURCE_DIR}/images/AntiAliasBinaryImageFilter.png}
    COMMAND AntiAliasBinaryImageFilter.py
      DATA{${WrapITK_SOURCE_DIR}/images/VentricleModel.png}
      AntiAliasBinaryImageFilter.png
      .001 50
    )
endif()


UNIQUE(types "${WRAP_ITK_SCALAR};UC")
# signed char can't be used to store an image with values up to 255
list(REMOVE_ITEM types SC)
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  foreach(t ${types})
    itk_python_add_test(NAME PythonSimplePipeline${t}${d}
      TEST_DRIVER_ARGS
        --compare simple_pipeline${t}${d}.nrrd DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      COMMAND simple_pipeline.py
        ${t}
        ${d}
        DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
        simple_pipeline${t}${d}.nrrd
    )
  endforeach()
endforeach()

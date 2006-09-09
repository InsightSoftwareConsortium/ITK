
# configure the test driver
FIND_PROGRAM(ITK_TEST_DRIVER itkTestDriver)

SET(PYTHON_DRIVER "${ITK_TEST_DRIVER}"
  --add-before-env PYTHONPATH "${WrapITK_BINARY_DIR}/Python/\${CTEST_CONFIGURATION_TYPE}"
  --add-before-env PYTHONPATH "${LIBRARY_OUTPUT_PATH}/\${CTEST_CONFIGURATION_TYPE}"
  "${PYTHON_EXECUTABLE}"
)


ADD_TEST(PythonTestDirectory ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/TestDirectory.py)

ADD_TEST(PythonTestObject ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/TestObject.py)

ADD_TEST(PythonFilterCoverage ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/filterCoverage.py ${ITK_INCLUDE_DIRS})

ADD_TEST(PythonFindEmptyClasses ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/findEmptyClasses.py)


# some tests will fail if dim=2 and unsigned short are not wrapped
INTERSECTION(WRAP_2 2 "${WRAP_ITK_DIMS}")
IF(WRAP_unsigned_short AND WRAP_2)
  ADD_TEST(PythonTypemaps ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/typemaps.py)
  
  ADD_TEST(PythonTemplate ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/template.py)
  
  ADD_TEST(PythonExtras ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/extras.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png out.png
  )
  
  ADD_TEST(PythonModuleToModule ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/module2module.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
  )

  ADD_TEST(PythonBinaryDilateImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryDilateImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/2th_cthead1.png
    BinaryDilateImageFilter.png
    --compare BinaryDilateImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryDilateImageFilter.png
  )

  ADD_TEST(PythonBinaryErodeImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryErodeImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/2th_cthead1.png
    BinaryErodeImageFilter.png
    --compare BinaryErodeImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryErodeImageFilter.png
  )

  ADD_TEST(PythonBinaryThresholdImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryThresholdImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    BinaryThresholdImageFilter.png
    50 150
    0 65535
    --compare BinaryThresholdImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryThresholdImageFilter.png
  )

  ADD_TEST(PythonCastImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CastImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CastImageFilter.png
    --compare CastImageFilter.png ${WrapITK_SOURCE_DIR}/images/cthead1.png
  )

  ADD_TEST(PythonGrayscaleDilateImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/GrayscaleDilateImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    GrayscaleDilateImageFilter.png
    --compare GrayscaleDilateImageFilter.png ${WrapITK_SOURCE_DIR}/images/GrayscaleDilateImageFilter.png
  )

  ADD_TEST(PythonBoxGrayscaleDilateImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BoxGrayscaleDilateImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    BoxGrayscaleDilateImageFilter.png
    --compare BoxGrayscaleDilateImageFilter.png ${WrapITK_SOURCE_DIR}/images/BoxGrayscaleDilateImageFilter.png
  )

  ADD_TEST(PythonStrelFromImageGrayscaleDilateImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/StrelFromImageGrayscaleDilateImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    StrelFromImageGrayscaleDilateImageFilter.png
    ${WrapITK_SOURCE_DIR}/images/StrelFromImage.png
    --compare StrelFromImageGrayscaleDilateImageFilter.png ${WrapITK_SOURCE_DIR}/images/StrelFromImageGrayscaleDilateImageFilter.png
  )

  ADD_TEST(PythonGrayscaleErodeImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/GrayscaleErodeImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    GrayscaleErodeImageFilter.png
    --compare GrayscaleErodeImageFilter.png ${WrapITK_SOURCE_DIR}/images/GrayscaleErodeImageFilter.png
  )

  ADD_TEST(PythonFlatStructuringElementBall
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/FlatStructuringElement.py
    Ball-5.png
    Ball
    5
    --compare Ball-5.png ${WrapITK_SOURCE_DIR}/images/Ball-5.png
  )

  ADD_TEST(PythonFlatStructuringElementBox
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/FlatStructuringElement.py
    Box-5.png
    Box
    5
    --compare Box-5.png ${WrapITK_SOURCE_DIR}/images/Box-5.png
  )

  ADD_TEST(PythonFlatStructuringElementFromImage
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/FlatStructuringElement.py
    StrelFromImage.png
    FromImage
    ${WrapITK_SOURCE_DIR}/images/StrelFromImage.png
    --compare StrelFromImage.png ${WrapITK_SOURCE_DIR}/images/StrelFromImage.png
  )

  ADD_TEST(PythonMeanImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/MeanImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    MeanImageFilter.png
    3
    --compare MeanImageFilter.png ${WrapITK_SOURCE_DIR}/images/MeanImageFilter.png
  )

  ADD_TEST(PythonMedianImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/MedianImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    MedianImageFilter.png
    3
    --compare MedianImageFilter.png ${WrapITK_SOURCE_DIR}/images/MedianImageFilter.png
  )

  ADD_TEST(PythonSigmoidImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/SigmoidImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    SigmoidImageFilter.png
    10 240 10 170
    --compare SigmoidImageFilter.png ${WrapITK_SOURCE_DIR}/images/SigmoidImageFilter.png
  )

  ADD_TEST(PythonThresholdImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    ThresholdImageFilter.png
    150 150
    --compare ThresholdImageFilter.png ${WrapITK_SOURCE_DIR}/images/ThresholdImageFilter.png
  )

  ADD_TEST(PythonResampleImageFilterTest1
    ${PYTHON_DRIVER}
    --compare ResampleImageFilterTest1.png
              ${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest1.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ResampleImageFilterTest1.png
          0
  )

  ADD_TEST(PythonResampleImageFilterTest2
    ${PYTHON_DRIVER}
    --compare ResampleImageFilterTest2.png
              ${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest2.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ResampleImageFilterTest2.png
          1
  )

  ADD_TEST(PythonResampleImageFilterTest3
    ${PYTHON_DRIVER}
    --compare ResampleImageFilterTest3.png
              ${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest3.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ResampleImageFilterTest3.png
          2
  )

  ADD_TEST(PythonResampleImageFilterTest4
    ${PYTHON_DRIVER}
    --compare ResampleImageFilterTest4.png
              ${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest4.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ResampleImageFilterTest4.png
          3
  )

ENDIF(WRAP_unsigned_short AND WRAP_2)


IF(WRAP_float AND WRAP_2)
  ADD_TEST(PythonCurvatureAnisotropicDiffusionImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureAnisotropicDiffusionImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CurvatureAnisotropicDiffusionImageFilter.png
    5 0.125 3
  )

  ADD_TEST(PythonCurvatureFlowImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureFlowImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CurvatureFlowImageFilter.png
    5 0.125 1.0
  )

  ADD_TEST(PythonSmoothingRecursiveGaussianImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/SmoothingRecursiveGaussianImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    SmoothingRecursiveGaussianImageFilter.png
    3
  )

  ADD_TEST(PythonLaplacianImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/LaplacianImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    LaplacianImageFilter.png
  )

  ADD_TEST(PythonFFTImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/FFTImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    FFTImageFilterReal.png
    FFTImageFilterImaginary.png
    --compare FFTImageFilterReal.png ${WrapITK_SOURCE_DIR}/images/FFTImageFilterReal.png
    --compare FFTImageFilterImaginary.png ${WrapITK_SOURCE_DIR}/images/FFTImageFilterImaginary.png
#     --compare complexImage.mhd ${WrapITK_SOURCE_DIR}/images/complexImage.mhd
  )

  ADD_TEST( PythonGradientMagnitudeRecursiveGaussianImageFilterTest
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/GradientMagnitudeRecursiveGaussianImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
    GradientMagnitudeRecursiveGaussianImageFilter.png
    5
    --compare GradientMagnitudeRecursiveGaussianImageFilter.png
            ${WrapITK_SOURCE_DIR}/images/GradientMagnitudeRecursiveGaussianImageFilter.png
  )

  ADD_TEST( PythonGradientAnisotropicDiffusionImageFilterTest
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/GradientAnisotropicDiffusionImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
    GradientAnisotropicDiffusionImageFilter.png
    5 0.125 1.0
    --compare GradientAnisotropicDiffusionImageFilter.png
              ${WrapITK_SOURCE_DIR}/images/GradientAnisotropicDiffusionImageFilter.png
  )


# TODO: reenable the geodesic test once we get why the result is not the
# same than with c++
# keep just one to be sure it run, but don't compare the images

  ADD_TEST( PythonGeodesicActiveContourLeftVentricleTest
    ${PYTHON_DRIVER}
#     --compare GeodesicActiveContourLeftVentricleTest.png
#             ${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourLeftVentricleTest.png
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/GeodesicActiveContourImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
    GeodesicActiveContourLeftVentricleTest.png
    81 114 5.0 1.0 -0.5 3.0 2.0
  )

#   ADD_TEST( PythonGeodesicActiveContourWhiteMatterTest
#     ${PYTHON_DRIVER}
#     --compare GeodesicActiveContourWhiteMatterTest.png
#             ${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourWhiteMatterTest.png
#     --
#     ${CMAKE_CURRENT_SOURCE_DIR}/GeodesicActiveContourImageFilter.py
#     ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
#     GeodesicActiveContourWhiteMatterTest.png
#     56 92 5.0 1.0 -0.3 2.0 10.0
#   )

#   ADD_TEST( PythonGeodesicActiveContourGrayMatterTest
#     ${PYTHON_DRIVER}
#     --compare GeodesicActiveContourGrayMatterTest.png
#             ${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourGrayMatterTest.png
#     --
#     ${CMAKE_CURRENT_SOURCE_DIR}/GeodesicActiveContourImageFilter.py
#     ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
#     GeodesicActiveContourGrayMatterTest.png
#     40 90 5.0 .5 -0.3 2.0 10.0
#   )


  ADD_TEST(PythonFastMarchingLeftVentricleTest
    ${PYTHON_DRIVER}
    --compare FastMarchingLeftVentricleTest.png
              ${WrapITK_SOURCE_DIR}/images/FastMarchingLeftVentricleTest.png
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          FastMarchingLeftVentricleTest.png
          81 114 1.0 -0.5 3.0 100 100
  )
  
  ADD_TEST(PythonFastMarchingRightVentricleTest
    ${PYTHON_DRIVER}
    --compare FastMarchingRightVentricleTest.png
              ${WrapITK_SOURCE_DIR}/images/FastMarchingRightVentricleTest.png
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          FastMarchingRightVentricleTest.png
          99 114 1.0 -0.5 3.0 100 100
  )
  
  ADD_TEST(PythonFastMarchingWhiteMatterTest
    ${PYTHON_DRIVER}
    --compare FastMarchingWhiteMatterTest.png
              ${WrapITK_SOURCE_DIR}/images/FastMarchingWhiteMatterTest.png
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          FastMarchingWhiteMatterTest.png
          56 92 1.0 -0.3 2.0 200 100
  )
  
  ADD_TEST(PythonFastMarchingGrayMatterTest
    ${PYTHON_DRIVER}
    --compare FastMarchingGrayMatterTest.png
              ${WrapITK_SOURCE_DIR}/images/FastMarchingGrayMatterTest.png
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          FastMarchingGrayMatterTest.png
          40 90 .5 -0.3 2.0 200 100
  )

  ADD_TEST(PythonThresholdSegmentationLevelSetVentricleTest
    ${PYTHON_DRIVER}
    --compare ThresholdSegmentationLevelSetVentricleTest.png
              ${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetVentricleTest.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdSegmentationLevelSetImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ThresholdSegmentationLevelSetVentricleTest.png
          81 112 5 210 250
  )
  
  ADD_TEST(PythonThresholdSegmentationLevelSetWhiteMatterTest
    ${PYTHON_DRIVER}
    --compare ThresholdSegmentationLevelSetWhiteMatterTest.png
              ${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetWhiteMatterTest.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdSegmentationLevelSetImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ThresholdSegmentationLevelSetWhiteMatterTest.png
          60 116 5 150 180
  )
  
  ADD_TEST(PythonThresholdSegmentationLevelSetGrayMatterTest
    ${PYTHON_DRIVER}
    --compare ThresholdSegmentationLevelSetGrayMatterTest.png
              ${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetGrayMatterTest.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdSegmentationLevelSetImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ThresholdSegmentationLevelSetGrayMatterTest.png
          107 69 5 180 210
  )

  ADD_TEST(PythonCannyEdgeDetectionImageFilterTest
    ${PYTHON_DRIVER}
    --compare CannyEdgeDetectionImageFilter.png
              ${WrapITK_SOURCE_DIR}/images/CannyEdgeDetectionImageFilter.png
    ${CMAKE_CURRENT_SOURCE_DIR}/CannyEdgeDetectionImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/cthead1.png
          CannyEdgeDetectionImageFilter.png
          9
  )

  ADD_TEST(PythonWatershedSegmentation1
    ${PYTHON_DRIVER}
    --compare WatershedSegmentation1.png
              ${WrapITK_SOURCE_DIR}/images/WatershedSegmentation1.png
    ${CMAKE_CURRENT_SOURCE_DIR}/WatershedSegmentation1.py
          ${WrapITK_SOURCE_DIR}/images/VisibleWomanEyeSlice.png
          WatershedSegmentation1.png
  )

  ADD_TEST(PythonSpatialObject
    ${PYTHON_DRIVER}
    --compare SpatialObject.png
              ${WrapITK_SOURCE_DIR}/images/SpatialObject.png
    ${CMAKE_CURRENT_SOURCE_DIR}/SpatialObject.py
          SpatialObject.png
  )

ENDIF(WRAP_float AND WRAP_2)


INTERSECTION(WRAP_3 3 "${WRAP_ITK_DIMS}")
IF(WRAP_float AND WRAP_3)
  ADD_TEST(PythonAntiAliasBinaryImageFilterTest
    ${PYTHON_DRIVER}
    --compare AntiAliasBinaryImageFilter.png
              ${WrapITK_SOURCE_DIR}/images/AntiAliasBinaryImageFilter.png
     ${CMAKE_CURRENT_SOURCE_DIR}/AntiAliasBinaryImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/VentricleModel.png
          AntiAliasBinaryImageFilter.png
          .001 50
  )
ENDIF(WRAP_float AND WRAP_3)


UNIQUE(types "${WRAP_ITK_SCALAR};UC")
# signed char can't be used to store an image with values up to 255
LIST(REMOVE_ITEM types SC)
FOREACH(d ${WRAP_ITK_DIMS})
  FOREACH(t ${types})
    ADD_TEST(PythonSimplePipeline${t}${d} ${PYTHON_DRIVER}
      ${CMAKE_CURRENT_SOURCE_DIR}/simple_pipeline.py
      ${t}
      ${d}
      ${WrapITK_SOURCE_DIR}/images/cthead1.png
      simple_pipeline${t}${d}.nrrd
      --compare simple_pipeline${t}${d}.nrrd ${WrapITK_SOURCE_DIR}/images/cthead1.png
    )
  ENDFOREACH(t)
ENDFOREACH(d)


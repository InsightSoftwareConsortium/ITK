
# configure the test driver
find_program(ITK_TEST_DRIVER itkTestDriver)

set(PYTHON_DRIVER "${ITK_TEST_DRIVER}"
  --add-before-env PYTHONPATH "${WrapITK_BINARY_DIR}/Python/\${CTEST_CONFIGURATION_TYPE}"
  --add-before-env PYTHONPATH "${LIBRARY_OUTPUT_PATH}/\${CTEST_CONFIGURATION_TYPE}"
  "${PYTHON_EXECUTABLE}"
)


add_test(PythonTestDirectory ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/TestDirectory.py)

add_test(PythonTestObject ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/TestObject.py)

add_test(PythonFilterCoverage ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/filterCoverage.py ${ITK_INCLUDE_DIRS})

add_test(PythonFindEmptyClasses ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/findEmptyClasses.py)

add_test(PythonGetNameOfClass ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/getNameOfClass.py)


# some tests will fail if dim=2 and unsigned short are not wrapped
INTERSECTION(WRAP_2 2 "${WRAP_ITK_DIMS}")
if(WRAP_unsigned_short AND WRAP_2)
  add_test(PythonTypemaps ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/typemaps.py)

  add_test(PythonTemplate ${PYTHON_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/template.py)

  add_test(PythonExtras ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/extras.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png out.png
  )

  add_test(PythonModuleToModule ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/module2module.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
  )

  add_test(PythonBinaryDilateImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryDilateImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/2th_cthead1.png
    BinaryDilateImageFilter.png
    --compare BinaryDilateImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryDilateImageFilter.png
  )

  add_test(PythonBinaryErodeImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryErodeImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/2th_cthead1.png
    BinaryErodeImageFilter.png
    --compare BinaryErodeImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryErodeImageFilter.png
  )

  add_test(PythonBinaryThresholdImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryThresholdImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    BinaryThresholdImageFilter.png
    50 150
    0 65535
    --compare BinaryThresholdImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryThresholdImageFilter.png
  )

  add_test(PythonCastImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CastImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CastImageFilter.png
    --compare CastImageFilter.png ${WrapITK_SOURCE_DIR}/images/cthead1.png
  )

  add_test(PythonGrayscaleDilateImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/GrayscaleDilateImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    GrayscaleDilateImageFilter.png
    --compare GrayscaleDilateImageFilter.png ${WrapITK_SOURCE_DIR}/images/GrayscaleDilateImageFilter.png
  )

  add_test(PythonBoxGrayscaleDilateImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BoxGrayscaleDilateImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    BoxGrayscaleDilateImageFilter.png
    --compare BoxGrayscaleDilateImageFilter.png ${WrapITK_SOURCE_DIR}/images/BoxGrayscaleDilateImageFilter.png
  )

#   add_test(PythonStrelFromImageGrayscaleDilateImageFilter
#     ${PYTHON_DRIVER}
#     ${CMAKE_CURRENT_SOURCE_DIR}/StrelFromImageGrayscaleDilateImageFilter.py
#     ${WrapITK_SOURCE_DIR}/images/cthead1.png
#     StrelFromImageGrayscaleDilateImageFilter.png
#     ${WrapITK_SOURCE_DIR}/images/StrelFromImage.png
#     --compare StrelFromImageGrayscaleDilateImageFilter.png ${WrapITK_SOURCE_DIR}/images/StrelFromImageGrayscaleDilateImageFilter.png
#   )

  add_test(PythonGrayscaleErodeImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/GrayscaleErodeImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    GrayscaleErodeImageFilter.png
    --compare GrayscaleErodeImageFilter.png ${WrapITK_SOURCE_DIR}/images/GrayscaleErodeImageFilter.png
  )

#   add_test(PythonFlatStructuringElementBall
#     ${PYTHON_DRIVER}
#     ${CMAKE_CURRENT_SOURCE_DIR}/FlatStructuringElement.py
#     Ball-5.png
#     Ball
#     5
#     --compare Ball-5.png ${WrapITK_SOURCE_DIR}/images/Ball-5.png
#   )

#   add_test(PythonFlatStructuringElementBox
#     ${PYTHON_DRIVER}
#     ${CMAKE_CURRENT_SOURCE_DIR}/FlatStructuringElement.py
#     Box-5.png
#     Box
#     5
#     --compare Box-5.png ${WrapITK_SOURCE_DIR}/images/Box-5.png
#   )

#   add_test(PythonFlatStructuringElementFromImage
#     ${PYTHON_DRIVER}
#     ${CMAKE_CURRENT_SOURCE_DIR}/FlatStructuringElement.py
#     StrelFromImage.png
#     FromImage
#     ${WrapITK_SOURCE_DIR}/images/StrelFromImage.png
#     --compare StrelFromImage.png ${WrapITK_SOURCE_DIR}/images/StrelFromImage.png
#   )

  add_test(PythonMeanImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/MeanImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    MeanImageFilter.png
    3
    --compare MeanImageFilter.png ${WrapITK_SOURCE_DIR}/images/MeanImageFilter.png
  )

  add_test(PythonMedianImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/MedianImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    MedianImageFilter.png
    3
    --compare MedianImageFilter.png ${WrapITK_SOURCE_DIR}/images/MedianImageFilter.png
  )

  add_test(PythonSigmoidImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/SigmoidImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    SigmoidImageFilter.png
    10 240 10 170
    --compare SigmoidImageFilter.png ${WrapITK_SOURCE_DIR}/images/SigmoidImageFilter.png
  )

  add_test(PythonThresholdImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    ThresholdImageFilter.png
    150 150
    --compare ThresholdImageFilter.png ${WrapITK_SOURCE_DIR}/images/ThresholdImageFilter.png
  )

  add_test(PythonResampleImageFilterTest1
    ${PYTHON_DRIVER}
    --compare ResampleImageFilterTest1.png
              ${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest1.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ResampleImageFilterTest1.png
          0
  )

  add_test(PythonResampleImageFilterTest2
    ${PYTHON_DRIVER}
    --compare ResampleImageFilterTest2.png
              ${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest2.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ResampleImageFilterTest2.png
          1
  )

  add_test(PythonResampleImageFilterTest3
    ${PYTHON_DRIVER}
    --compare ResampleImageFilterTest3.png
              ${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest3.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ResampleImageFilterTest3.png
          2
  )

  add_test(PythonResampleImageFilterTest4
    ${PYTHON_DRIVER}
    --compare ResampleImageFilterTest4.png
              ${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest4.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ResampleImageFilterTest4.png
          3
  )

endif(WRAP_unsigned_short AND WRAP_2)


if(WRAP_float AND WRAP_2)
  add_test(PythonCurvatureAnisotropicDiffusionImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureAnisotropicDiffusionImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CurvatureAnisotropicDiffusionImageFilter.png
    5 0.125 3
  )

  add_test(PythonCurvatureFlowImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureFlowImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CurvatureFlowImageFilter.png
    5 0.125 1.0
  )

  add_test(PythonSmoothingRecursiveGaussianImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/SmoothingRecursiveGaussianImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    SmoothingRecursiveGaussianImageFilter.png
    3
  )

  add_test(PythonLaplacianImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/LaplacianImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    LaplacianImageFilter.png
  )

  add_test(PythonFFTImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/FFTImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    FFTImageFilterReal.png
    FFTImageFilterImaginary.png
    --compare FFTImageFilterReal.png ${WrapITK_SOURCE_DIR}/images/FFTImageFilterReal.png
    --compare FFTImageFilterImaginary.png ${WrapITK_SOURCE_DIR}/images/FFTImageFilterImaginary.png
#     --compare complexImage.mhd ${WrapITK_SOURCE_DIR}/images/complexImage.mhd
  )

  add_test( PythonGradientMagnitudeRecursiveGaussianImageFilterTest
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/GradientMagnitudeRecursiveGaussianImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
    GradientMagnitudeRecursiveGaussianImageFilter.png
    5
    --compare GradientMagnitudeRecursiveGaussianImageFilter.png
            ${WrapITK_SOURCE_DIR}/images/GradientMagnitudeRecursiveGaussianImageFilter.png
  )

  add_test( PythonGradientAnisotropicDiffusionImageFilterTest
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

  add_test( PythonGeodesicActiveContourLeftVentricleTest
    ${PYTHON_DRIVER}
#     --compare GeodesicActiveContourLeftVentricleTest.png
#             ${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourLeftVentricleTest.png
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/GeodesicActiveContourImageFilter.py
    ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
    GeodesicActiveContourLeftVentricleTest.png
    81 114 5.0 1.0 -0.5 3.0 2.0
  )

#   add_test( PythonGeodesicActiveContourWhiteMatterTest
#     ${PYTHON_DRIVER}
#     --compare GeodesicActiveContourWhiteMatterTest.png
#             ${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourWhiteMatterTest.png
#     --
#     ${CMAKE_CURRENT_SOURCE_DIR}/GeodesicActiveContourImageFilter.py
#     ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
#     GeodesicActiveContourWhiteMatterTest.png
#     56 92 5.0 1.0 -0.3 2.0 10.0
#   )

#   add_test( PythonGeodesicActiveContourGrayMatterTest
#     ${PYTHON_DRIVER}
#     --compare GeodesicActiveContourGrayMatterTest.png
#             ${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourGrayMatterTest.png
#     --
#     ${CMAKE_CURRENT_SOURCE_DIR}/GeodesicActiveContourImageFilter.py
#     ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
#     GeodesicActiveContourGrayMatterTest.png
#     40 90 5.0 .5 -0.3 2.0 10.0
#   )


  add_test(PythonFastMarchingLeftVentricleTest
    ${PYTHON_DRIVER}
    --compare FastMarchingLeftVentricleTest.png
              ${WrapITK_SOURCE_DIR}/images/FastMarchingLeftVentricleTest.png
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          FastMarchingLeftVentricleTest.png
          81 114 1.0 -0.5 3.0 100 100
  )

  add_test(PythonFastMarchingRightVentricleTest
    ${PYTHON_DRIVER}
    --compare FastMarchingRightVentricleTest.png
              ${WrapITK_SOURCE_DIR}/images/FastMarchingRightVentricleTest.png
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          FastMarchingRightVentricleTest.png
          99 114 1.0 -0.5 3.0 100 100
  )

  add_test(PythonFastMarchingWhiteMatterTest
    ${PYTHON_DRIVER}
    --compare FastMarchingWhiteMatterTest.png
              ${WrapITK_SOURCE_DIR}/images/FastMarchingWhiteMatterTest.png
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          FastMarchingWhiteMatterTest.png
          56 92 1.0 -0.3 2.0 200 100
  )

  add_test(PythonFastMarchingGrayMatterTest
    ${PYTHON_DRIVER}
    --compare FastMarchingGrayMatterTest.png
              ${WrapITK_SOURCE_DIR}/images/FastMarchingGrayMatterTest.png
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          FastMarchingGrayMatterTest.png
          40 90 .5 -0.3 2.0 200 100
  )

  add_test(PythonThresholdSegmentationLevelSetVentricleTest
    ${PYTHON_DRIVER}
    --compare ThresholdSegmentationLevelSetVentricleTest.png
              ${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetVentricleTest.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdSegmentationLevelSetImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ThresholdSegmentationLevelSetVentricleTest.png
          81 112 5 210 250
  )

  add_test(PythonThresholdSegmentationLevelSetWhiteMatterTest
    ${PYTHON_DRIVER}
    --compare ThresholdSegmentationLevelSetWhiteMatterTest.png
              ${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetWhiteMatterTest.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdSegmentationLevelSetImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ThresholdSegmentationLevelSetWhiteMatterTest.png
          60 116 5 150 180
  )

  add_test(PythonThresholdSegmentationLevelSetGrayMatterTest
    ${PYTHON_DRIVER}
    --compare ThresholdSegmentationLevelSetGrayMatterTest.png
              ${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetGrayMatterTest.png
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdSegmentationLevelSetImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
          ThresholdSegmentationLevelSetGrayMatterTest.png
          107 69 5 180 210
  )

  add_test(PythonCannyEdgeDetectionImageFilterTest
    ${PYTHON_DRIVER}
    --compare CannyEdgeDetectionImageFilter.png
              ${WrapITK_SOURCE_DIR}/images/CannyEdgeDetectionImageFilter.png
    ${CMAKE_CURRENT_SOURCE_DIR}/CannyEdgeDetectionImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/cthead1.png
          CannyEdgeDetectionImageFilter.png
          9
  )

  add_test(PythonWatershedSegmentation1
    ${PYTHON_DRIVER}
    --compare WatershedSegmentation1.png
              ${WrapITK_SOURCE_DIR}/images/WatershedSegmentation1.png
    ${CMAKE_CURRENT_SOURCE_DIR}/WatershedSegmentation1.py
          ${WrapITK_SOURCE_DIR}/images/VisibleWomanEyeSlice.png
          WatershedSegmentation1.png
  )

  add_test(PythonSpatialObject
    ${PYTHON_DRIVER}
    --compare SpatialObject.png
              ${WrapITK_SOURCE_DIR}/images/SpatialObject.png
    ${CMAKE_CURRENT_SOURCE_DIR}/SpatialObject.py
          SpatialObject.png
  )

  if(WRAP_complex_float AND WRAP_FFTW)
    add_test(PythonFFTWImageFilter
      ${PYTHON_DRIVER}
      ${CMAKE_CURRENT_SOURCE_DIR}/FFTWImageFilter.py
      ${WrapITK_SOURCE_DIR}/images/cthead1.png
      FFTWImageFilter.png
      --compare FFTWImageFilter.png
                ${WrapITK_SOURCE_DIR}/images/cthead1.png
    )
  endif(WRAP_complex_float AND WRAP_FFTW)

endif(WRAP_float AND WRAP_2)


INTERSECTION(WRAP_3 3 "${WRAP_ITK_DIMS}")
if(WRAP_float AND WRAP_3)
  add_test(PythonAntiAliasBinaryImageFilterTest
    ${PYTHON_DRIVER}
    --compare AntiAliasBinaryImageFilter.png
              ${WrapITK_SOURCE_DIR}/images/AntiAliasBinaryImageFilter.png
     ${CMAKE_CURRENT_SOURCE_DIR}/AntiAliasBinaryImageFilter.py
          ${WrapITK_SOURCE_DIR}/images/VentricleModel.png
          AntiAliasBinaryImageFilter.png
          .001 50
  )
endif(WRAP_float AND WRAP_3)


UNIQUE(types "${WRAP_ITK_SCALAR};UC")
# signed char can't be used to store an image with values up to 255
list(REMOVE_ITEM types SC)
foreach(d ${WRAP_ITK_DIMS})
  foreach(t ${types})
    add_test(PythonSimplePipeline${t}${d} ${PYTHON_DRIVER}
      ${CMAKE_CURRENT_SOURCE_DIR}/simple_pipeline.py
      ${t}
      ${d}
      ${WrapITK_SOURCE_DIR}/images/cthead1.png
      simple_pipeline${t}${d}.nrrd
      --compare simple_pipeline${t}${d}.nrrd ${WrapITK_SOURCE_DIR}/images/cthead1.png
    )
  endforeach(t)
endforeach(d)


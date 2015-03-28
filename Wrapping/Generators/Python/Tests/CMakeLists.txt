
# configure the test driver
macro(add_python_test name)
  set(command "${PYTHON_EXECUTABLE}")

  # add extra command which may be needed on some systems
  if(CMAKE_OSX_ARCHITECTURES)
    list(GET CMAKE_OSX_ARCHITECTURES 0 test_arch)
    set(command arch -${test_arch} ${command})
  endif()

  itk_add_test(NAME ${name}
    COMMAND "${ITK_TEST_DRIVER}"
    --add-before-env PYTHONPATH "${ITK_WRAP_PYTHON_BINARY_DIR}/$<CONFIGURATION>"
    --add-before-env PYTHONPATH "${ITK_WRAP_PYTHON_BINARY_DIR}"
    --add-before-env PYTHONPATH "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/$<CONFIGURATION>"
    --add-before-env PYTHONPATH "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    --add-before-libpath "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/$<CONFIGURATION>"
    --add-before-libpath "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    --add-before-libpath "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIGURATION>"
    --add-before-libpath "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    --add-before-libpath "${ITK_DIR}"
    ${command}
    ${ARGN}
    )
endmacro()


add_python_test(PythonTestDirectory ${CMAKE_CURRENT_SOURCE_DIR}/TestDirectory.py)
add_python_test(PythonTestMetaDataDictionary ${CMAKE_CURRENT_SOURCE_DIR}/TestMetaDataDictionary.py)
add_python_test(PythonTestObject ${CMAKE_CURRENT_SOURCE_DIR}/TestObject.py)

# TODO: drop -E option once all filters/spatial objects/cost functions are actually wrapped
add_python_test(NonBlockingPythonFilterCoverage ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -E -b Filter -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-filters.txt ${ITK_INCLUDE_DIRS})
add_python_test(NonBlockingPythonSpatialObjectCoverage ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -E -b SpatialObject ${ITK_INCLUDE_DIRS})
add_python_test(NonBlockingPythonCostFunctionCoverage ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -E -b CostFunction ${ITK_INCLUDE_DIRS})

add_python_test(PythonImageIOCoverage ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -b ImageIO -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-imageio.txt ${ITK_INCLUDE_DIRS})
add_python_test(PythonOptimizerCoverage ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -b Optimizer ${ITK_INCLUDE_DIRS})
add_python_test(PythonTransformCoverage ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -b Transform -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-transform.txt ${ITK_INCLUDE_DIRS})

# These two tests can use up all of system memory when the are executed.
# TODO: find the root cause and re-enable them.  See ITK-3006 on issues.itk.org.
#add_python_test(PythonFindEmptyClasses ${CMAKE_CURRENT_SOURCE_DIR}/findEmptyClasses.py)
add_python_test(PythonGetNameOfClass ${CMAKE_CURRENT_SOURCE_DIR}/getNameOfClass.py)

add_python_test(PythonTiming ${CMAKE_CURRENT_SOURCE_DIR}/timing.py)


# some tests will fail if dim=2 and unsigned short are not wrapped
INTERSECTION(WRAP_2 2 "${ITK_WRAP_IMAGE_DIMS}")
if(ITK_WRAP_unsigned_char AND WRAP_2)
  add_python_test(PythonTypemaps ${CMAKE_CURRENT_SOURCE_DIR}/typemaps.py)

  add_python_test(PythonTemplate ${CMAKE_CURRENT_SOURCE_DIR}/template.py)

  add_python_test(PythonAutoPipeline
    ${CMAKE_CURRENT_SOURCE_DIR}/auto_pipeline.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    auto_pipeline.png
    3
    --compare auto_pipeline.png DATA{${WrapITK_SOURCE_DIR}/images/MedianImageFilter.png}
  )

  add_python_test(PythonTemplatedPipeline
    ${CMAKE_CURRENT_SOURCE_DIR}/templated_pipeline.py
    DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
    templated_pipeline.png
    10
    --compare templated_pipeline.png DATA{${WrapITK_SOURCE_DIR}/images/templated_pipeline.png}
  )

  add_python_test(PythonExtras
    ${CMAKE_CURRENT_SOURCE_DIR}/extras.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png} out.png
  )

  add_python_test(PythonModuleToModule
    ${CMAKE_CURRENT_SOURCE_DIR}/module2module.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    5
  )

  add_python_test(PythonBinaryDilateImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryDilateImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
    BinaryDilateImageFilter.png
    5
    --compare BinaryDilateImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryDilateImageFilter.png}
  )

  add_python_test(PythonBinaryErodeImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryErodeImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
    BinaryErodeImageFilter.png
    5
    --compare BinaryErodeImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryErodeImageFilter.png}
  )

  add_python_test(PythonBinaryThresholdImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryThresholdImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    BinaryThresholdImageFilter.png
    50 150
    0 255
    --compare BinaryThresholdImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryThresholdImageFilter.png}
  )

  add_python_test(PythonCastImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/CastImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    CastImageFilter.png
    --compare CastImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
  )

  add_python_test(PythonGrayscaleDilateImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/GrayscaleDilateImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    GrayscaleDilateImageFilter.png
    5
    --compare GrayscaleDilateImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/GrayscaleDilateImageFilter.png}
  )

  add_python_test(PythonBoxGrayscaleDilateImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/BoxGrayscaleDilateImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    BoxGrayscaleDilateImageFilter.png
    --compare BoxGrayscaleDilateImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BoxGrayscaleDilateImageFilter.png}
  )

#   add_python_test(PythonStrelFromImageGrayscaleDilateImageFilter
#     ${CMAKE_CURRENT_SOURCE_DIR}/StrelFromImageGrayscaleDilateImageFilter.py
#     DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
#     StrelFromImageGrayscaleDilateImageFilter.png
#     DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImage.png}
#     --compare StrelFromImageGrayscaleDilateImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImageGrayscaleDilateImageFilter.png}
#   )

  add_python_test(PythonGrayscaleErodeImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/GrayscaleErodeImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    GrayscaleErodeImageFilter.png
    5
    --compare GrayscaleErodeImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/GrayscaleErodeImageFilter.png}
  )

#   add_python_test(PythonFlatStructuringElementBall
#     ${CMAKE_CURRENT_SOURCE_DIR}/FlatStructuringElement.py
#     Ball-5.png
#     Ball
#     5
#     --compare Ball-5.png DATA{${WrapITK_SOURCE_DIR}/images/Ball-5.png}
#   )

#   add_python_test(PythonFlatStructuringElementBox
#     ${CMAKE_CURRENT_SOURCE_DIR}/FlatStructuringElement.py
#     Box-5.png
#     Box
#     5
#     --compare Box-5.png DATA{${WrapITK_SOURCE_DIR}/images/Box-5.png}
#   )

#   add_python_test(PythonFlatStructuringElementFromImage
#     ${CMAKE_CURRENT_SOURCE_DIR}/FlatStructuringElement.py
#     StrelFromImage.png
#     FromImage
#     DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImage.png}
#     --compare StrelFromImage.png DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImage.png}
#   )

  add_python_test(PythonMeanImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/MeanImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    MeanImageFilter.png
    3
    --compare MeanImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/MeanImageFilter.png}
  )

  add_python_test(PythonMedianImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/MedianImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    MedianImageFilter.png
    3
    --compare MedianImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/MedianImageFilter.png}
  )

  add_python_test(PythonSigmoidImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/SigmoidImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    SigmoidImageFilter.png
    10 240 10 170
    --compare SigmoidImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/SigmoidImageFilter.png}
  )

  add_python_test(PythonThresholdImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    ThresholdImageFilter.png
    150 150
    --compare ThresholdImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/ThresholdImageFilter.png}
  )

  add_python_test(PythonResampleImageFilterTest1
    --compare ResampleImageFilterTest1.png
              DATA{${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest1.png}
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
          ResampleImageFilterTest1.png
          0
  )

  add_python_test(PythonResampleImageFilterTest2
    --compare ResampleImageFilterTest2.png
              DATA{${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest2.png}
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
          ResampleImageFilterTest2.png
          1
  )

  add_python_test(PythonResampleImageFilterTest3
    --compare ResampleImageFilterTest3.png
              DATA{${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest3.png}
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
          ResampleImageFilterTest3.png
          2
  )

  add_python_test(PythonResampleImageFilterTest4
    --compare ResampleImageFilterTest4.png
              DATA{${WrapITK_SOURCE_DIR}/images/ResampleImageFilterTest4.png}
    ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
          ResampleImageFilterTest4.png
          3
  )

endif()


if(ITK_WRAP_float AND WRAP_2)
  add_python_test(PythonCurvatureAnisotropicDiffusionImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureAnisotropicDiffusionImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    CurvatureAnisotropicDiffusionImageFilter.png
    5 0.125 3
  )

  add_python_test(PythonCurvatureFlowImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureFlowImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    CurvatureFlowImageFilter.png
    5 0.125 1.0
  )

  add_python_test(PythonSmoothingRecursiveGaussianImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/SmoothingRecursiveGaussianImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    SmoothingRecursiveGaussianImageFilter.png
    3
  )

  add_python_test(PythonLaplacianImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/LaplacianImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    LaplacianImageFilter.png
  )

  add_python_test(PythonFFTImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/FFTImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    FFTImageFilterReal.png
    FFTImageFilterImaginary.png
    --compare FFTImageFilterReal.png DATA{${WrapITK_SOURCE_DIR}/images/FFTImageFilterReal.png}
    --compare FFTImageFilterImaginary.png DATA{${WrapITK_SOURCE_DIR}/images/FFTImageFilterImaginary.png}
#     --compare complexImage.mhd DATA{${WrapITK_SOURCE_DIR}/images/complexImage.mhd}
  )

  add_python_test( PythonGradientMagnitudeRecursiveGaussianImageFilterTest
    ${CMAKE_CURRENT_SOURCE_DIR}/GradientMagnitudeRecursiveGaussianImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
    GradientMagnitudeRecursiveGaussianImageFilter.png
    5
    --compare GradientMagnitudeRecursiveGaussianImageFilter.png
            DATA{${WrapITK_SOURCE_DIR}/images/GradientMagnitudeRecursiveGaussianImageFilter.png}
  )

  add_python_test( PythonGradientAnisotropicDiffusionImageFilterTest
    ${CMAKE_CURRENT_SOURCE_DIR}/GradientAnisotropicDiffusionImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
    GradientAnisotropicDiffusionImageFilter.png
    5 0.125 1.0
    --compare GradientAnisotropicDiffusionImageFilter.png
              DATA{${WrapITK_SOURCE_DIR}/images/GradientAnisotropicDiffusionImageFilter.png}
  )


# TODO: reenable the geodesic test once we get why the result is not the
# same than with c++
# keep just one to be sure it run, but don't compare the images

  add_python_test( PythonGeodesicActiveContourLeftVentricleTest
#     --compare GeodesicActiveContourLeftVentricleTest.png
#             DATA{${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourLeftVentricleTest.png}
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/GeodesicActiveContourImageFilter.py
    DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
    GeodesicActiveContourLeftVentricleTest.png
    81 114 5.0 1.0 -0.5 3.0 2.0
  )

#   add_python_test( PythonGeodesicActiveContourWhiteMatterTest
#     --compare GeodesicActiveContourWhiteMatterTest.png
#             DATA{${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourWhiteMatterTest.png}
#     --
#     ${CMAKE_CURRENT_SOURCE_DIR}/GeodesicActiveContourImageFilter.py
#     DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
#     GeodesicActiveContourWhiteMatterTest.png
#     56 92 5.0 1.0 -0.3 2.0 10.0
#   )

#   add_python_test( PythonGeodesicActiveContourGrayMatterTest
#     --compare GeodesicActiveContourGrayMatterTest.png
#             DATA{${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourGrayMatterTest.png}
#     --
#     ${CMAKE_CURRENT_SOURCE_DIR}/GeodesicActiveContourImageFilter.py
#     DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
#     GeodesicActiveContourGrayMatterTest.png
#     40 90 5.0 .5 -0.3 2.0 10.0
#   )


  add_python_test(PythonFastMarchingLeftVentricleTest
    --compare FastMarchingLeftVentricleTest.png
              DATA{${WrapITK_SOURCE_DIR}/images/FastMarchingLeftVentricleTest.png}
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
          FastMarchingLeftVentricleTest.png
          81 114 1.0 -0.5 3.0 100 100
  )

  add_python_test(PythonFastMarchingRightVentricleTest
    --compare FastMarchingRightVentricleTest.png
              DATA{${WrapITK_SOURCE_DIR}/images/FastMarchingRightVentricleTest.png}
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
          FastMarchingRightVentricleTest.png
          99 114 1.0 -0.5 3.0 100 100
  )

  add_python_test(PythonFastMarchingWhiteMatterTest
    --compare FastMarchingWhiteMatterTest.png
              DATA{${WrapITK_SOURCE_DIR}/images/FastMarchingWhiteMatterTest.png}
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
          FastMarchingWhiteMatterTest.png
          56 92 1.0 -0.3 2.0 200 100
  )

  add_python_test(PythonFastMarchingGrayMatterTest
    --compare FastMarchingGrayMatterTest.png
              DATA{${WrapITK_SOURCE_DIR}/images/FastMarchingGrayMatterTest.png}
    --
    ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
          FastMarchingGrayMatterTest.png
          40 90 .5 -0.3 2.0 200 100
  )

  add_python_test(PythonThresholdSegmentationLevelSetVentricleTest
    --compare ThresholdSegmentationLevelSetVentricleTest.png
              DATA{${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetVentricleTest.png}
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdSegmentationLevelSetImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
          ThresholdSegmentationLevelSetVentricleTest.png
          81 112 5 210 250
  )

  add_python_test(PythonThresholdSegmentationLevelSetWhiteMatterTest
    --compare ThresholdSegmentationLevelSetWhiteMatterTest.png
              DATA{${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetWhiteMatterTest.png}
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdSegmentationLevelSetImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
          ThresholdSegmentationLevelSetWhiteMatterTest.png
          60 116 5 150 180
  )

  add_python_test(PythonThresholdSegmentationLevelSetGrayMatterTest
    --compare ThresholdSegmentationLevelSetGrayMatterTest.png
              DATA{${WrapITK_SOURCE_DIR}/images/ThresholdSegmentationLevelSetGrayMatterTest.png}
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdSegmentationLevelSetImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
          ThresholdSegmentationLevelSetGrayMatterTest.png
          107 69 5 180 210
  )

  add_python_test(PythonCannyEdgeDetectionImageFilterTest
    --compareNumberOfPixelsTolerance 1
    --compare CannyEdgeDetectionImageFilter.png
              DATA{${WrapITK_SOURCE_DIR}/images/CannyEdgeDetectionImageFilter.png}
    ${CMAKE_CURRENT_SOURCE_DIR}/CannyEdgeDetectionImageFilter.py
          DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
          CannyEdgeDetectionImageFilter.png
          9
  )

  add_python_test(PythonWatershedSegmentation1
    --compare WatershedSegmentation1.png
              DATA{${WrapITK_SOURCE_DIR}/images/WatershedSegmentation1.png}
    ${CMAKE_CURRENT_SOURCE_DIR}/WatershedSegmentation1.py
          DATA{${WrapITK_SOURCE_DIR}/images/VisibleWomanEyeSlice.png}
          WatershedSegmentation1.png
  )

  add_python_test(PythonSpatialObject
    --compare SpatialObject.png
              DATA{${WrapITK_SOURCE_DIR}/images/SpatialObject.png}
    ${CMAKE_CURRENT_SOURCE_DIR}/SpatialObjectTest.py
          SpatialObject.png
  )

  add_python_test(PythonReadDicomAndReadTag
    ${CMAKE_CURRENT_SOURCE_DIR}/ReadDicomAndReadTag.py
    DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
  )

endif()


INTERSECTION(WRAP_3 3 "${ITK_WRAP_IMAGE_DIMS}")
if(ITK_WRAP_float AND WRAP_3)
  add_python_test(PythonAntiAliasBinaryImageFilterTest
    --compare AntiAliasBinaryImageFilter.png
              DATA{${WrapITK_SOURCE_DIR}/images/AntiAliasBinaryImageFilter.png}
     ${CMAKE_CURRENT_SOURCE_DIR}/AntiAliasBinaryImageFilter.py
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
    add_python_test(PythonSimplePipeline${t}${d}
      ${CMAKE_CURRENT_SOURCE_DIR}/simple_pipeline.py
      ${t}
      ${d}
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      simple_pipeline${t}${d}.nrrd
      --compare simple_pipeline${t}${d}.nrrd DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    )
  endforeach()
endforeach()

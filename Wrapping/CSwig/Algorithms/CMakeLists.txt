# create the ITKAlgorithmsTcl libraries
SET(WRAP_SOURCES
    wrap_itkCurvatureFlowImageFilter
    wrap_itkHistogramMatchingImageFilter
    wrap_itkImageRegistrationMethod
    wrap_itkImageToImageMetric
    wrap_itkMattesMutualInformationImageToImageMetric
    wrap_itkMeanSquaresImageToImageMetric
    wrap_itkMinMaxCurvatureFlowImageFilter
    wrap_itkMutualInformationImageToImageMetric
    wrap_itkMultiResolutionImageRegistrationMethod
    wrap_itkNormalizedCorrelationImageToImageMetric
    wrap_itkOtsuThresholdImageCalculator
    wrap_itkMeanReciprocalSquareDifferenceImageToImageMetric
    wrap_itkRecursiveMultiResolutionPyramidImageFilter
    wrap_itkSimpleFuzzyConnectednessImageFilterBase
    wrap_itkSimpleFuzzyConnectednessScalarImageFilter
    wrap_itkThresholdSegmentationLevelSetImageFilter
    wrap_itkGeodesicActiveContourLevelSetImageFilter
    wrap_itkShapeDetectionLevelSetImageFilter
    wrap_itkCurvesLevelSetImageFilter
    wrap_itkNarrowBandCurvesLevelSetImageFilter
    wrap_itkWatershedImageFilter
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/Common/ITKCommon.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFilters/ITKBasicFilters.mdx"
                       "${WrapITK_BINARY_DIR}/Algorithms/ITKAlgorithms.mdx"
)

ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKAlgorithms Algorithms 
  "_ITKNumericsPython;_ITKCommonPython;_ITKBasicFiltersPython" "" "")

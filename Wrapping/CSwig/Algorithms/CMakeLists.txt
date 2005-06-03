# create the ITKAlgorithmsTcl libraries
SET(WRAP_SOURCES
    wrap_itkCurvatureFlowImageFilter
    wrap_itkDemonsRegistrationFilter
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
    wrap_itkThresholdSegmentationLevelSetImageFilter
    wrap_itkGeodesicActiveContourLevelSetImageFilter
    wrap_itkShapeDetectionLevelSetImageFilter
    wrap_itkCurvesLevelSetImageFilter
    wrap_itkNarrowBandLevelSetImageFilter
    wrap_itkPDEDeformableRegistrationFilter
    wrap_itkNarrowBandCurvesLevelSetImageFilter
    wrap_itkVoronoiSegmentationImageFilter
    wrap_itkWatershedImageFilter
    wrap_itkSegmentationLevelSetImageFilter
    wrap_itkSparseFieldLevelSetImageFilter
    wrap_itkSymmetricForcesDemonsRegistrationFilter
    wrap_itkTreeNodeSO
    wrap_itkLevelSetFunction
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/CommonA/ITKCommonA.mdx"
                       "${WrapITK_BINARY_DIR}/CommonB/ITKCommonB.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFiltersA/ITKBasicFiltersA.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFiltersB/ITKBasicFiltersB.mdx"
                       "${WrapITK_BINARY_DIR}/Algorithms/ITKAlgorithms.mdx"
)

ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKAlgorithms Algorithms 
  "ITKNumerics;ITKCommonB;ITKCommonA;ITKBasicFiltersA;ITKBasicFiltersB" "" "")

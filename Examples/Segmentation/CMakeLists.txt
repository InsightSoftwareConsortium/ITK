#
# Examples on the use of segmentation algorithms
#

PROJECT(ImageSegmentation)

ADD_EXECUTABLE(ConfidenceConnected ConfidenceConnected.cxx )
TARGET_LINK_LIBRARIES(ConfidenceConnected ITKCommon ITKNumerics VXLNumerics ITKIO)


ADD_EXECUTABLE(HybridSegmentationFuzzyVoronoi HybridSegmentationFuzzyVoronoi.cxx )
TARGET_LINK_LIBRARIES(HybridSegmentationFuzzyVoronoi ITKCommon ITKNumerics VXLNumerics ITKIO)

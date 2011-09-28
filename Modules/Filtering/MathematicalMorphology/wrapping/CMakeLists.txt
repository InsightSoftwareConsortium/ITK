WRAP_LIBRARY(ITKMathematicalMorphology)

set(WRAPPER_LIBRARY_DEPENDS ITKImageIntensity ITKImageGrid ITKConnectedComponents)

set(WRAPPER_LIBRARY_GROUPS
  itkFlatStructuringElement
  itkMovingHistogramImageFilterBase
)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()

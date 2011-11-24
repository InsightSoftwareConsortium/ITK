WRAP_LIBRARY(ITKFastMarching)

set(WRAPPER_LIBRARY_GROUPS
  itkLevelSetNode
  itkNodePair
  itkFastMarchingStoppingCriterionBase
  ITKFastMarchingBase
  itkFastMarchingImageFilterBase
)
AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()

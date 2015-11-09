itk_wrap_module(ITKFastMarching)

set(WRAPPER_SUBMODULE_ORDER
  itkLevelSetNode
  itkNodePair
  itkFastMarchingStoppingCriterionBase
  ITKFastMarchingBase
  itkFastMarchingImageFilterBase
)
itk_auto_load_submodules()
itk_end_wrap_module()

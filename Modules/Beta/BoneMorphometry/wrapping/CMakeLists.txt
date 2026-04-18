itk_wrap_module(BoneMorphometry)

set(WRAPPER_SUBMODULE_ORDER
   itkBoneCommon
   itkBoneMorphometryFeaturesFilter
   itkBoneMorphometryFeaturesImageFilter
   itkReplaceFeatureMapNanInfImageFilter)

itk_auto_load_submodules()
itk_end_wrap_module()

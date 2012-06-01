itk_wrap_module(ITKDenoising)

set(WRAPPER_LIBRARY_GROUPS
  itkPatchBasedDenoisingBaseImageFilter
  itkPatchBasedDenoisingImageFilter
)
itk_auto_load_submodules()

itk_end_wrap_module()

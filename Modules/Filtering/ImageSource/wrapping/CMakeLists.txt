itk_wrap_module(ITKImageSource)

set(WRAPPER_LIBRARY_GROUPS
  itkGaborImageSource
  itkGaussianImageSource
  itkGridImageSource
  itkGenerateImageSource
  itkParametricImageSource
  )
itk_auto_load_submodules()

itk_end_wrap_module()

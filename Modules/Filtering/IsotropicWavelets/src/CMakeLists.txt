set(IsotropicWavelets_SRCS
  itkRieszUtilities.cxx
  )
### generating libraries
itk_module_add_library( IsotropicWavelets ${IsotropicWavelets_SRCS})

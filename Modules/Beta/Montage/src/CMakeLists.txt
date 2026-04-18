set(Montage_SRCS
  itkPhaseCorrelationOptimizer.cxx
  itkPhaseCorrelationImageRegistrationMethod.cxx
  )
itk_module_add_library(Montage ${Montage_SRCS})

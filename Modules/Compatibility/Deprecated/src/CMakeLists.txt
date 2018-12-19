set(ITKDeprecated_SRCS
  itkConditionVariable.cxx
  itkFastMutexLock.cxx
  itkMutexLock.cxx
  itkSimpleFastMutexLock.cxx
  )

itk_module_add_library(ITKDeprecated ${ITKDeprecated_SRCS})

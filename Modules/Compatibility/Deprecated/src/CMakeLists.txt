set(ITKDeprecated_SRCS
  itkBarrier.cxx
  itkConditionVariable.cxx
  itkFastMutexLock.cxx
  itkMutexLock.cxx
  itkSimpleFastMutexLock.cxx
  )

itk_module_add_library(ITKDeprecated ${ITKDeprecated_SRCS})

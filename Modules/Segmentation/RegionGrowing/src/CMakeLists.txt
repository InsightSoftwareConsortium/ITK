set(ITKRegionGrowing_SRCS
    itkConnectedThresholdImageFilter.cxx
  )

itk_module_add_library(ITKRegionGrowing ${ITKRegionGrowing_SRCS})

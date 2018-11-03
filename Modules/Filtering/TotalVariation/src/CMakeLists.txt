set(TotalVariation_SRCS itkMyFilter.cxx)

itk_module_add_library(TotalVariation ${TotalVariation_SRCS})
itk_module_link_dependencies()
target_link_libraries(TotalVariation LINK_PUBLIC proxTV::proxTV)

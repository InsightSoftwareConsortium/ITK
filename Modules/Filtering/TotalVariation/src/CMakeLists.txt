set(TotalVariation_SRCS itkProxTV.cxx)

itk_module_add_library(TotalVariation ${TotalVariation_SRCS})
# add_dependencies(TotalVariation ${_proxTV_lib})
target_link_libraries(TotalVariation PUBLIC ${_proxTV_lib})

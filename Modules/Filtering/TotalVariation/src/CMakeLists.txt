set(TotalVariation_SRCS itkProxTVImageFilter.cxx)

itk_module_add_library(TotalVariation ${TotalVariation_SRCS})
# TODO: Remove debug messages
message(STATUS "proxTV_lib: ${_proxTV_lib}")
if(TARGET ${_proxTV_lib})
  message(STATUS "proxTV_lib: ${_proxTV_lib} TARGET EXISTS")
else()
  message(STATUS "proxTV_lib: ${_proxTV_lib} TARGET DOES NOT EXISTS")
endif()
target_link_libraries(TotalVariation PUBLIC ${_proxTV_lib})

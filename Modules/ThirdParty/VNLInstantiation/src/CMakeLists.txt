set(ITKVNLInstantiation_SRCS
  libraryplaceholder.cxx
)

itk_module_add_library(ITKVNLInstantiation ${ITKVNLInstantiation_SRCS})
target_link_libraries(ITKVNLInstantiation LINK_PUBLIC ${ITKVNL_LIBRARIES})

set(GrowCut_SRCS
  fibheap.cc
  HeapNode.cc
  )

itk_module_add_library(GrowCut ${GrowCut_SRCS})

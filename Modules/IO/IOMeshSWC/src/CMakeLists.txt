set(IOMeshSWC_SRCS
  itkSWCMeshIO.cxx
  itkSWCMeshIOFactory.cxx
  )

itk_module_add_library(IOMeshSWC ${IOMeshSWC_SRCS})

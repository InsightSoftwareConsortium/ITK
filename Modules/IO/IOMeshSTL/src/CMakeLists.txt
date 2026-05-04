set(IOMeshSTL_SRC
  itkSTLMeshIO.cxx
  itkSTLMeshIOFactory.cxx
)

itk_module_add_library(IOMeshSTL ${ITK_LIBRARY_BUILD_TYPE} ${IOMeshSTL_SRC})

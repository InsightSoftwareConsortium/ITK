set(
  ITKIOSTLMesh_SRC
  itkSTLMeshIO.cxx
  itkSTLMeshIOFactory.cxx
)

add_library(
  ITKIOSTLMesh
  ${ITK_LIBRARY_BUILD_TYPE}
  ${ITKIOSTLMesh_SRC}
)
target_link_libraries(
  ITKIOSTLMesh
  ${ITKCommon_LIBRARIES}
  ${ITKIOMesh_LIBRARIES}
)

itk_module_target(ITKIOSTLMesh)

set(
  IOMeshSTL_SRC
  itkSTLMeshIO.cxx
  itkSTLMeshIOFactory.cxx
)

add_library(
  IOMeshSTL
  ${ITK_LIBRARY_BUILD_TYPE}
  ${IOMeshSTL_SRC}
)
target_link_libraries(
  IOMeshSTL
  ${ITKCommon_LIBRARIES}
  ${ITKIOMesh_LIBRARIES}
)

itk_module_target(IOMeshSTL)

set(
  IOSTL_SRC
  itkSTLMeshIO.cxx
  itkSTLMeshIOFactory.cxx
)

add_library(
  IOSTL
  ${ITK_LIBRARY_BUILD_TYPE}
  ${IOSTL_SRC}
)
target_link_libraries(
  IOSTL
  ${ITKCommon_LIBRARIES}
  ${ITKIOMesh_LIBRARIES}
)

itk_module_target(IOSTL)

set(ITK-VTK_SRC
itkVTKImageExportBase.cxx
)

add_library(ITK-VTK ${ITK-VTK_SRC})
target_link_libraries(ITK-VTK  ${ITK-Common_LIBRARIES})
itk_module_target(ITK-VTK)

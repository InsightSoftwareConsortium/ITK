set(ITKVTK_SRC
itkVTKImageExportBase.cxx
)

add_library(ITKVTK ${ITKVTK_SRC})
itk_module_link_dependencies()
itk_module_target(ITKVTK)

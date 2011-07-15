set(ITKVTK_SRC
itkVTKImageExportBase.cxx
)

add_library(ITKVTK ${ITKVTK_SRC})
target_link_libraries(ITKVTK  ${ITKCommon_LIBRARIES})
itk_module_target(ITKVTK)

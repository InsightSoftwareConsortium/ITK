set(ITKVideoBridgeVXL_SRC
itkVXLVideoIO.cxx
itkVXLVideoIOFactory.cxx
)

add_library(ITKVideoBridgeVXL ${ITKVideoBridgeVXL_SRC})
itk_module_link_dependencies()
target_link_libraries(ITKVideoBridgeVXL LINK_PUBLIC vidl vul)
itk_module_target(ITKVideoBridgeVXL)

set(ITKIOMeta_SRC
itkMetaArrayWriter.cxx
itkMetaImageIO.cxx
itkMetaArrayReader.cxx
itkMetaImageIOFactory.cxx
)

add_library(ITKIOMeta ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOMeta_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOMeta)

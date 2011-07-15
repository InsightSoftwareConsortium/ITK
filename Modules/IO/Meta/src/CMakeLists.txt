set(ITKIOMeta_SRC
itkMetaArrayWriter.cxx
itkMetaImageIO.cxx
itkMetaArrayReader.cxx
itkMetaImageIOFactory.cxx
)

add_library(ITKIOMeta ${ITKIOMeta_SRC})
target_link_libraries(ITKIOMeta  ${ITKMetaIO_LIBRARIES} ${ITKIOBase_LIBRARIES})
itk_module_target(ITKIOMeta)

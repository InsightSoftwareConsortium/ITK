set(ITKIOMeta_SRC
itkMetaArrayWriter.cxx
itkMetaImageIO.cxx
itkMetaArrayReader.cxx
itkMetaImageIOFactory.cxx
)

add_library(ITKIOMeta ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOMeta_SRC})
target_link_libraries(ITKIOMeta  ${ITKMetaIO_LIBRARIES} ${ITKIOImageBase_LIBRARIES})
itk_module_target(ITKIOMeta)

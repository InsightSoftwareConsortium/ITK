set(ITK-IO-Meta_SRC
itkMetaArrayWriter.cxx
itkMetaImageIO.cxx
itkMetaArrayReader.cxx
itkMetaImageIOFactory.cxx
)

add_library(ITK-IO-Meta ${ITK-IO-Meta_SRC})
target_link_libraries(ITK-IO-Meta  ${ITK-MetaIO_LIBRARIES} ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-IO-Meta)

set(ITKIOTIFF_SRC
itkTIFFImageIO.cxx
itkTIFFImageIOFactory.cxx
)

add_library(ITKIOTIFF ${ITKIOTIFF_SRC})
target_link_libraries(ITKIOTIFF  ${ITKTIFF_LIBRARIES} ${ITKIOBase_LIBRARIES})
itk_module_target(ITKIOTIFF)

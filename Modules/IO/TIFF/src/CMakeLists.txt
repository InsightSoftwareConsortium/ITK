set(ITK-IO-TIFF_SRC
itkTIFFImageIO.cxx
itkTIFFImageIOFactory.cxx
)

add_library(ITK-IO-TIFF ${ITK-IO-TIFF_SRC})
target_link_libraries(ITK-IO-TIFF  ${ITK-TIFF_LIBRARIES} ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-IO-TIFF)

set(ITK-IO-JPEG_SRC
itkJPEGImageIOFactory.cxx
itkJPEGImageIO.cxx
)

add_library(ITK-IO-JPEG ${ITK-IO-JPEG_SRC})
target_link_libraries(ITK-IO-JPEG  ${ITK-JPEG_LIBRARIES} ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-IO-JPEG)

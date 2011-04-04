set(ITK-IO-BMP_SRC
itkBMPImageIO.cxx
itkBMPImageIOFactory.cxx
)

add_library(ITK-IO-BMP ${ITK-IO-BMP_SRC})
target_link_libraries(ITK-IO-BMP  ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-IO-BMP)

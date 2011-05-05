set(ITK-IO-PNG_SRC
itkPNGImageIOFactory.cxx
itkPNGImageIO.cxx
)

add_library(ITK-IO-PNG ${ITK-IO-PNG_SRC})
target_link_libraries(ITK-IO-PNG  ${ITK-PNG_LIBRARIES} ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-IO-PNG)

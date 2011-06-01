set(ITK-IO-GE_SRC
itkGE5ImageIOFactory.cxx
itkGEAdwImageIO.cxx
itkGE4ImageIO.cxx
itkGEAdwImageIOFactory.cxx
itkGE5ImageIO.cxx
itkGE4ImageIOFactory.cxx
)

add_library(ITK-IO-GE ${ITK-IO-GE_SRC})
target_link_libraries(ITK-IO-GE  ${ITK-IO-IPL_LIBRARIES})
itk_module_target(ITK-IO-GE)

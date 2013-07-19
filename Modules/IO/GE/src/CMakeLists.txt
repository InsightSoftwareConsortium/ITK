set(ITKIOGE_SRC
itkGE5ImageIOFactory.cxx
itkGEAdwImageIO.cxx
itkGE4ImageIO.cxx
itkGEAdwImageIOFactory.cxx
itkGE5ImageIO.cxx
itkGE4ImageIOFactory.cxx
)

add_library(ITKIOGE ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOGE_SRC})
target_link_libraries(ITKIOGE  ${ITKIOIPL_LIBRARIES})
itk_module_target(ITKIOGE)

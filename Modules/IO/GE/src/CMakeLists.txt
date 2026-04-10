set(
  ITKIOGE_SRCS
  itkGE4ImageIO.cxx
  itkGE4ImageIOFactory.cxx
  itkGE5ImageIO.cxx
  itkGE5ImageIOFactory.cxx
  itkGEAdwImageIO.cxx
  itkGEAdwImageIOFactory.cxx
)

itk_module_add_library(ITKIOGE ${ITKIOGE_SRCS})

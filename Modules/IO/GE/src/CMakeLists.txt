set(ITKIOGE_SRCS
  itkGE5ImageIOFactory.cxx
  itkGEAdwImageIO.cxx
  itkGE4ImageIO.cxx
  itkGEAdwImageIOFactory.cxx
  itkGE5ImageIO.cxx
  itkGE4ImageIOFactory.cxx
  )

itk_module_add_library(ITKIOGE ${ITKIOGE_SRCS})

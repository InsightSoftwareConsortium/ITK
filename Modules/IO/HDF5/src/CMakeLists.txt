set(
  ITKIOHDF5_SRCS
  itkHDF5ImageIO.cxx
  itkHDF5ImageIOFactory.cxx
)

itk_module_add_library(ITKIOHDF5 ${ITKIOHDF5_SRCS})

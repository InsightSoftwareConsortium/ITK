set(ITKIOHDF5_SRCS
  itkHDF5ImageIOFactory.cxx
  itkHDF5ImageIO.cxx
  )

itk_module_add_library(ITKIOHDF5 ${ITKIOHDF5_SRCS})

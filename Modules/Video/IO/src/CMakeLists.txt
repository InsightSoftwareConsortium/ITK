set(ITKVideoIO_SRCS
  itkFileListVideoIO.cxx
  itkFileListVideoIOFactory.cxx
  itkVideoIOBase.cxx
  itkVideoIOFactory.cxx
  )

itk_module_add_library(ITKVideoIO ${ITKVideoIO_SRCS})

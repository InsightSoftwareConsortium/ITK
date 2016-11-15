set(ITKIOMeta_SRCS
  itkMetaArrayWriter.cxx
  itkMetaImageIO.cxx
  itkMetaArrayReader.cxx
  itkMetaImageIOFactory.cxx
  )

itk_module_add_library(ITKIOMeta ${ITKIOMeta_SRCS})

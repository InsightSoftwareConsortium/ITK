set(
  ITKIOMeta_SRCS
  itkMetaArrayReader.cxx
  itkMetaArrayWriter.cxx
  itkMetaImageIO.cxx
  itkMetaImageIOFactory.cxx
)

itk_module_add_library(ITKIOMeta ${ITKIOMeta_SRCS})

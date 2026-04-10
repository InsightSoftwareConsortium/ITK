set(
  ITKIOBruker2dseq_SRCS
  itkBruker2dseqImageIO.cxx
  itkBruker2dseqImageIOFactory.cxx
)

itk_module_add_library(ITKIOBruker ${ITKIOBruker2dseq_SRCS})

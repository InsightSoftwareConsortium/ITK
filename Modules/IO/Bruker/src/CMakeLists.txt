set(ITKIOBruker2dseq_SRCS
    itkBruker2dseqImageIOFactory.cxx
    itkBruker2dseqImageIO.cxx
)

itk_module_add_library(ITKIOBruker ${ITKIOBruker2dseq_SRCS} )

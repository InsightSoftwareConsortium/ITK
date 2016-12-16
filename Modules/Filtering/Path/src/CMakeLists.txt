set(ITKPath_SRCS
  itkChainCodePath2D.cxx
  itkOrthogonallyCorrected2DParametricPath.cxx
  )

itk_module_add_library(ITKPath ${ITKPath_SRCS})

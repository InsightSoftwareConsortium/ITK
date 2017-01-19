set(ITKIOTransformInsightLegacy_SRCS
  itkTxtTransformIO.cxx
  itkTxtTransformIOFactory.cxx
  )

itk_module_add_library(ITKIOTransformInsightLegacy ${ITKIOTransformInsightLegacy_SRCS})

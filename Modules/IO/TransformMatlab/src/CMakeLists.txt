set(ITKIOTransformMatlab_SRCS
  itkMatlabTransformIO.cxx
  itkMatlabTransformIOFactory.cxx
  )

itk_module_add_library(ITKIOTransformMatlab ${ITKIOTransformMatlab_SRCS})

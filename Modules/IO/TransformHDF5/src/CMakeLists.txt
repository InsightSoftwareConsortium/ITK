set(ITKIOTransformHDF5_SRCS
  itkHDF5TransformIO.cxx
  itkHDF5TransformIOFactory.cxx
  )

itk_module_add_library(ITKIOTransformHDF5 ${ITKIOTransformHDF5_SRCS})

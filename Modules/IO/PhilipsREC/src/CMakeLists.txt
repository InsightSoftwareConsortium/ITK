set(
  ITKIOPhilipsREC_SRC
  itkPhilipsPAR.cxx
  itkPhilipsRECImageIO.cxx
  itkPhilipsRECImageIOFactory.cxx
)

itk_module_add_library(ITKIOPhilipsREC ${ITKIOPhilipsREC_SRC})

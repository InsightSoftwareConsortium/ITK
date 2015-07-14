set(ITKIOPhilipsREC_SRC
itkPhilipsPAR.cxx
itkPhilipsRECImageIO.cxx
itkPhilipsRECImageIOFactory.cxx
)

add_library(ITKIOPhilipsREC ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOPhilipsREC_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOPhilipsREC)

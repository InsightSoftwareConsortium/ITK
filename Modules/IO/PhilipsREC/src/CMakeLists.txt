set(ITKIOPhilipsREC_SRC
itkPhilipsPAR.cxx
itkPhilipsRECImageIO.cxx
itkPhilipsRECImageIOFactory.cxx
)

add_library(ITKIOPhilipsREC ${ITKIOPhilipsREC_SRC})
target_link_libraries(ITKIOPhilipsREC ${ITKIOBase_LIBRARIES} ${ITKZLIB_LIBRARIES})
itk_module_target(ITKIOPhilipsREC)

set(ITKIOPhilipsREC_SRC
itkPhilipsPAR.cxx
itkPhilipsRECImageIO.cxx
itkPhilipsRECImageIOFactory.cxx
)

add_library(ITKIOPhilipsREC ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOPhilipsREC_SRC})
target_link_libraries(ITKIOPhilipsREC ${ITKIOImageBase_LIBRARIES} ${ITKZLIB_LIBRARIES})
itk_module_target(ITKIOPhilipsREC)

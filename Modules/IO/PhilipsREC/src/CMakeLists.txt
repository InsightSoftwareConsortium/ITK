set(ITK-IO-PhilipsREC_SRC
itkPhilipsPAR.cxx
itkPhilipsRECImageIO.cxx
itkPhilipsRECImageIOFactory.cxx
)

add_library(ITK-IO-PhilipsREC ${ITK-IO-PhilipsREC_SRC})
target_link_libraries(ITK-IO-PhilipsREC ${ITK-IO-Base_LIBRARIES} ${ITK-ZLIB_LIBRARIES})
itk_module_target(ITK-IO-PhilipsREC)

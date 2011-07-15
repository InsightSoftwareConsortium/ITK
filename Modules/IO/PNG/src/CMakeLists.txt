set(ITKIOPNG_SRC
itkPNGImageIOFactory.cxx
itkPNGImageIO.cxx
)

add_library(ITKIOPNG ${ITKIOPNG_SRC})
target_link_libraries(ITKIOPNG  ${ITKPNG_LIBRARIES} ${ITKIOBase_LIBRARIES})
itk_module_target(ITKIOPNG)

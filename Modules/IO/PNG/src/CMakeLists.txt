set(ITKIOPNG_SRC
itkPNGImageIOFactory.cxx
itkPNGImageIO.cxx
)

add_library(ITKIOPNG ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOPNG_SRC})
target_link_libraries(ITKIOPNG  ${ITKPNG_LIBRARIES} ${ITKIOImageBase_LIBRARIES})
itk_module_target(ITKIOPNG)

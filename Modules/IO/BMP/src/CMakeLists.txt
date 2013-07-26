set(ITKIOBMP_SRC
itkBMPImageIO.cxx
itkBMPImageIOFactory.cxx
)

add_library(ITKIOBMP ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOBMP_SRC})
target_link_libraries(ITKIOBMP  ${ITKIOImageBase_LIBRARIES})
itk_module_target(ITKIOBMP)

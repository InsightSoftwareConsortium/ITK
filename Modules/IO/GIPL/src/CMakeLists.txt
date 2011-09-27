set(ITKIOGIPL_SRC
itkGiplImageIOFactory.cxx
itkGiplImageIO.cxx
)

add_library(ITKIOGIPL ${ITKIOGIPL_SRC})
target_link_libraries(ITKIOGIPL  ${ITKIOBase_LIBRARIES} ${ITKZLIB_LIBRARIES})
itk_module_target(ITKIOGIPL)

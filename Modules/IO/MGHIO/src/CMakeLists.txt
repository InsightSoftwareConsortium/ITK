set(ITKIOMGH_SRC
itkMGHImageIOFactory.cxx
itkMGHImageIO.cxx
)

add_library(ITKIOMGH ${ITKIOMGH_SRC})
target_link_libraries(ITKIOMGH ${ITKIOImageBase_LIBRARIES} ${ITKZLIB_LIBRARIES})
itk_module_target(ITKIOMGH)

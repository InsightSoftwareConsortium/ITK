set(ITKIOMGH_SRC
itkMGHImageIOFactory.cxx
itkMGHImageIO.cxx
)

add_library(ITKIOMGH ${ITKIOMGH_SRC})
target_link_libraries(ITKIOMGH  ${ITKMGH_LIBRARIES} ${ITKIOImageBase_LIBRARIES} ${ITKTransform_LIBRARIES})
itk_module_target(ITKIOMGH)

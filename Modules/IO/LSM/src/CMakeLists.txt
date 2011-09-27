set(ITKIOLSM_SRC
itkLSMImageIOFactory.cxx
itkLSMImageIO.cxx
)

add_library(ITKIOLSM ${ITKIOLSM_SRC})
target_link_libraries(ITKIOLSM  ${ITKIOTIFF_LIBRARIES})
itk_module_target(ITKIOLSM)

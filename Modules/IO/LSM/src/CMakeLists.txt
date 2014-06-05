set(ITKIOLSM_SRC
itkLSMImageIOFactory.cxx
itkLSMImageIO.cxx
)

add_library(ITKIOLSM ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOLSM_SRC})
target_link_libraries(ITKIOLSM  ${ITKIOTIFF_LIBRARIES})
itk_module_target(ITKIOLSM)

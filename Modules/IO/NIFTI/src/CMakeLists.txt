set(ITKIONIFTI_SRC
itkNiftiImageIOFactory.cxx
itkNiftiImageIO.cxx
)

add_library(ITKIONIFTI ${ITKIONIFTI_SRC})
target_link_libraries(ITKIONIFTI  ${ITKNIFTI_LIBRARIES} ${ITKIOBase_LIBRARIES} ${ITKTransform_LIBRARIES})
itk_module_target(ITKIONIFTI)

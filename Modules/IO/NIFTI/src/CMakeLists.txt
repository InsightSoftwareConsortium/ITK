set(ITKIONIFTI_SRC
itkNiftiImageIOFactory.cxx
itkNiftiImageIO.cxx
)

add_library(ITKIONIFTI ${ITK_LIBRARY_BUILD_TYPE} ${ITKIONIFTI_SRC})
target_link_libraries(ITKIONIFTI  ${ITKNIFTI_LIBRARIES} ${ITKIOImageBase_LIBRARIES} ${ITKTransform_LIBRARIES})
itk_module_target(ITKIONIFTI)

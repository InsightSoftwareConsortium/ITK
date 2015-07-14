set(ITKIONIFTI_SRC
itkNiftiImageIOFactory.cxx
itkNiftiImageIO.cxx
)

add_library(ITKIONIFTI ${ITK_LIBRARY_BUILD_TYPE} ${ITKIONIFTI_SRC})
itk_module_link_dependencies()
target_link_libraries(ITKIONIFTI LINK_PUBLIC ${ITKTransform_LIBRARIES})
itk_module_target(ITKIONIFTI)

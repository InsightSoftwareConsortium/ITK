set(ITK-IO-NIFTI_SRC
itkNiftiImageIOFactory.cxx
itkNiftiImageIO.cxx
)

add_library(ITK-IO-NIFTI ${ITK-IO-NIFTI_SRC})
target_link_libraries(ITK-IO-NIFTI  ${ITK-NIFTI_LIBRARIES} ${ITK-IO-Base_LIBRARIES} ${ITK-Transform_LIBRARIES})
itk_module_target(ITK-IO-NIFTI)

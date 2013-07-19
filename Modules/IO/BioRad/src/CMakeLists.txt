set(ITKIOBioRad_SRC
itkBioRadImageIOFactory.cxx
itkBioRadImageIO.cxx
)

add_library(ITKIOBioRad ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOBioRad_SRC})
target_link_libraries(ITKIOBioRad  ${ITKIOImageBase_LIBRARIES})
itk_module_target(ITKIOBioRad)

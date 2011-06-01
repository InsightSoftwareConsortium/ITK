set(ITK-IO-BioRad_SRC
itkBioRadImageIOFactory.cxx
itkBioRadImageIO.cxx
)

add_library(ITK-IO-BioRad ${ITK-IO-BioRad_SRC})
target_link_libraries(ITK-IO-BioRad  ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-IO-BioRad)

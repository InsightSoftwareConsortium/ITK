set(ITK-IO-NRRD_SRC
itkNrrdImageIOFactory.cxx
itkNrrdImageIO.cxx
)

add_library(ITK-IO-NRRD ${ITK-IO-NRRD_SRC})
target_link_libraries(ITK-IO-NRRD  ${ITK-NrrdIO_LIBRARIES} ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-IO-NRRD)

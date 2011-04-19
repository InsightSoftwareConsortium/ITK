set(ITK-IO-GIPL_SRC
itkGiplImageIOFactory.cxx
itkGiplImageIO.cxx
)

add_library(ITK-IO-GIPL ${ITK-IO-GIPL_SRC})
target_link_libraries(ITK-IO-GIPL  ${ITK-IO-Base_LIBRARIES} ${ITK-ZLIB_LIBRARIES})
itk_module_target(ITK-IO-GIPL)

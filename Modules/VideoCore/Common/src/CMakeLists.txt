set(Video-Common_SRC
itkTemporalRegion.cxx
itkTemporalDataObject.cxx
itkTemporalProcessObject.cxx
)

add_library(Video-Common ${Video-Common_SRC})
target_link_libraries(Video-Common ${ITK-Common_LIBRARIES})
itk_module_target(Video-Common)

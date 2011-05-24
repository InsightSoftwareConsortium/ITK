set(Video-Core-Common_SRC
itkTemporalRegion.cxx
itkTemporalDataObject.cxx
itkTemporalProcessObject.cxx
)

add_library(Video-Core-Common ${Video-Core-Common_SRC})
target_link_libraries(Video-Core-Common ${ITK-Common_LIBRARIES})
itk_module_target(Video-Core-Common)

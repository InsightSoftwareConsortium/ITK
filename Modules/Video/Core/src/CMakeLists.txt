set(ITKVideoCore_SRC
itkTemporalRegion.cxx
itkTemporalDataObject.cxx
itkTemporalProcessObject.cxx
)

add_library(ITKVideoCore ${ITKVideoCore_SRC})
target_link_libraries(ITKVideoCore ${ITKCommon_LIBRARIES})
itk_module_target(ITKVideoCore)

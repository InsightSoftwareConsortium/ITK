set(ITKVideoCore_SRC
itkTemporalRegion.cxx
itkTemporalDataObject.cxx
itkTemporalProcessObject.cxx
)

add_library(ITKVideoCore ${ITKVideoCore_SRC})
itk_module_link_dependencies()
itk_module_target(ITKVideoCore)

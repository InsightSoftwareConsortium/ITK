set(
  ITKVideoCore_SRCS
  itkTemporalDataObject.cxx
  itkTemporalProcessObject.cxx
  itkTemporalRegion.cxx
)

itk_module_add_library(ITKVideoCore ${ITKVideoCore_SRCS})

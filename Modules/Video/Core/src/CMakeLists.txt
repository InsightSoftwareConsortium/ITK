set(ITKVideoCore_SRCS
  itkTemporalRegion.cxx
  itkTemporalDataObject.cxx
  itkTemporalProcessObject.cxx
  )

itk_module_add_library(ITKVideoCore ${ITKVideoCore_SRCS})

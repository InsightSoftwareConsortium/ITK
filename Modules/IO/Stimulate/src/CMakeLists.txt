set(ITKIOStimulate_SRCS
  itkStimulateImageIO.cxx
  itkStimulateImageIOFactory.cxx
  )

itk_module_add_library(ITKIOStimulate ${ITKIOStimulate_SRCS})

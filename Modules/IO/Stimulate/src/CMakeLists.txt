set(ITKIOStimulate_SRC
itkStimulateImageIO.cxx
itkStimulateImageIOFactory.cxx
)

add_library(ITKIOStimulate ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOStimulate_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOStimulate)

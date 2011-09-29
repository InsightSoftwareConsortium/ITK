set(ITKIOStimulate_SRC
itkStimulateImageIO.cxx
itkStimulateImageIOFactory.cxx
)

add_library(ITKIOStimulate ${ITKIOStimulate_SRC})
target_link_libraries(ITKIOStimulate  ${ITKIOImageBase_LIBRARIES})
itk_module_target(ITKIOStimulate)

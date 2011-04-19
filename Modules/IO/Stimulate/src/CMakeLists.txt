set(ITK-IO-Stimulate_SRC
itkStimulateImageIO.cxx
itkStimulateImageIOFactory.cxx
)

add_library(ITK-IO-Stimulate ${ITK-IO-Stimulate_SRC})
target_link_libraries(ITK-IO-Stimulate  ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-IO-Stimulate)

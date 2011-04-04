set(ITK-Blox_SRC
itkBloxItem.cxx
)

add_library(ITK-Blox ${ITK-Blox_SRC})
target_link_libraries(ITK-Blox  ${ITK-ImageIntensity_LIBRARIES} ${ITK-ImageGradient_LIBRARIES} ${ITK-Smoothing_LIBRARIES} ${ITK-Optimizers_LIBRARIES} ${ITK-IO-RAW_LIBRARIES})
itk_module_target(ITK-Blox)

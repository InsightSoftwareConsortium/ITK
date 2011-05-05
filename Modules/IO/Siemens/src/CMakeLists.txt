set(ITK-IO-Siemens_SRC
itkSiemensVisionImageIOFactory.cxx
itkSiemensVisionImageIO.cxx
)

add_library(ITK-IO-Siemens ${ITK-IO-Siemens_SRC})
target_link_libraries(ITK-IO-Siemens  ${ITK-IO-IPL_LIBRARIES} ${ITK-IO-GE_LIBRARIES})
itk_module_target(ITK-IO-Siemens)

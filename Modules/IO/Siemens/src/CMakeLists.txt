set(ITKIOSiemens_SRC
itkSiemensVisionImageIOFactory.cxx
itkSiemensVisionImageIO.cxx
)

add_library(ITKIOSiemens ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOSiemens_SRC})
target_link_libraries(ITKIOSiemens  ${ITKIOIPL_LIBRARIES} ${ITKIOGE_LIBRARIES})
itk_module_target(ITKIOSiemens)

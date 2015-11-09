set(ITKIOSiemens_SRC
itkSiemensVisionImageIOFactory.cxx
itkSiemensVisionImageIO.cxx
)

add_library(ITKIOSiemens ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOSiemens_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOSiemens)

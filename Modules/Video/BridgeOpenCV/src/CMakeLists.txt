set(ITKVideoBridgeOpenCV_SRC
itkOpenCVVideoIO.cxx
itkOpenCVVideoIOFactory.cxx
)

add_library(ITKVideoBridgeOpenCV ${ITKVideoBridgeOpenCV_SRC})
itk_module_link_dependencies()
target_link_libraries(ITKVideoBridgeOpenCV LINK_PUBLIC ${OpenCV_LIBS})
itk_module_target(ITKVideoBridgeOpenCV)

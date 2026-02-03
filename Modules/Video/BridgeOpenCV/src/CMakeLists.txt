set(
  ITKVideoBridgeOpenCV_SRC
  itkOpenCVVideoIO.cxx
  itkOpenCVVideoIOFactory.cxx
)

itk_module_add_library(ITKVideoBridgeOpenCV ${ITKVideoBridgeOpenCV_SRC})
target_link_libraries(ITKVideoBridgeOpenCV PUBLIC ${OpenCV_LIBS})

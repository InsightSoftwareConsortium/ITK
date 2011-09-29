set(ITKVideoBridgeOpenCV_SRC
itkOpenCVVideoIO.cxx
itkOpenCVVideoIOFactory.cxx
)

add_library(ITKVideoBridgeOpenCV ${ITKVideoBridgeOpenCV_SRC})
target_link_libraries(ITKVideoBridgeOpenCV
  ITKVideoCore
  ITKVideoIO
  ${OpenCV_LIBS}
)
itk_module_target(ITKVideoBridgeOpenCV)

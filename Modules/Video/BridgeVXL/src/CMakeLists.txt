set(ITKVideoBridgeVXL_SRC
itkVXLVideoIO.cxx
itkVXLVideoIOFactory.cxx
)

include( ${VXL_CMAKE_DIR}/UseVXL.cmake )
add_library(ITKVideoBridgeVXL ${ITKVideoBridgeVXL_SRC})
target_link_libraries(ITKVideoBridgeVXL
  ${ITKVideoIO_LIBRARIES}
  vidl
  vul
  )
itk_module_target(ITKVideoBridgeVXL)

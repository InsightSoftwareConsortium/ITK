set(ITKVideoIO_SRC
itkFileListVideoIO.cxx
itkFileListVideoIOFactory.cxx
itkVideoIOBase.cxx
itkVideoIOFactory.cxx
)

add_library(ITKVideoIO ${ITKVideoIO_SRC})
target_link_libraries(ITKVideoIO ${ITKVideoCore_LIBRARIES} ${ITKIOImageBase_LIBRARIES})
itk_module_target(ITKVideoIO)

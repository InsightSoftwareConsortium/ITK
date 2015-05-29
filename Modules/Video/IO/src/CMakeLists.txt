set(ITKVideoIO_SRC
itkFileListVideoIO.cxx
itkFileListVideoIOFactory.cxx
itkVideoIOBase.cxx
itkVideoIOFactory.cxx
)

add_library(ITKVideoIO ${ITKVideoIO_SRC})
itk_module_link_dependencies()
itk_module_target(ITKVideoIO)

set(ITKIOCSV_SRC
itkCSVFileReaderBase.cxx
)

add_library(ITKIOCSV ${ITKIOCSV_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOCSV)

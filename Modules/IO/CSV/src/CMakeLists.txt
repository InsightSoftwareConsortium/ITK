set(ITKIOCSV_SRC
itkCSVFileReaderBase.cxx
)

add_library(ITKIOCSV ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOCSV_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOCSV)

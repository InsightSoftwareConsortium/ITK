set(ITKIOCSV_SRC
itkCSVFileReaderBase.cxx
)

add_library(ITKIOCSV ${ITKIOCSV_SRC})
target_link_libraries(ITKIOCSV  ${ITKIOBase_LIBRARIES})
itk_module_target(ITKIOCSV)

set(ITKIOIPL_SRC
itkIPLCommonImageIO.cxx
itkIPLFileNameList.cxx
)

add_library(ITKIOIPL ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOIPL_SRC})
target_link_libraries(ITKIOIPL  ${ITKIOImageBase_LIBRARIES})
itk_module_target(ITKIOIPL)

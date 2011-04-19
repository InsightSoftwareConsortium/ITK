set(ITK-IO-IPL_SRC
itkIPLCommonImageIO.cxx
itkIPLFileNameList.cxx
)

add_library(ITK-IO-IPL ${ITK-IO-IPL_SRC})
target_link_libraries(ITK-IO-IPL  ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-IO-IPL)

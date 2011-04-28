set(ITK-IO-GDCM_SRC
itkGDCMImageIO.cxx
itkGDCMImageIOFactory.cxx
itkGDCMSeriesFileNames.cxx
)

add_library(ITK-IO-GDCM ${ITK-IO-GDCM_SRC})
target_link_libraries(ITK-IO-GDCM  ${ITK-GDCM_LIBRARIES} ${ITK-IO-Base_LIBRARIES} ${ITK-ImageIntensity_LIBRARIES})
itk_module_target(ITK-IO-GDCM)

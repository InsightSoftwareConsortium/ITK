set(ITK-IO-Base_SRC
itkImageIORegion.cxx
itkArchetypeSeriesFileNames.cxx
itkImageIOFactory.cxx
itkIOCommon.cxx
itkNumericSeriesFileNames.cxx
itkImageIOBase.cxx
itkRegularExpressionSeriesFileNames.cxx
itkStreamingImageIOBase.cxx
)

add_library(ITK-IO-Base ${ITK-IO-Base_SRC})
target_link_libraries(ITK-IO-Base  ${ITK-Common_LIBRARIES})
itk_module_target(ITK-IO-Base)

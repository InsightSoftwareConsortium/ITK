set(ITKIOBase_SRC
itkImageIORegion.cxx
itkArchetypeSeriesFileNames.cxx
itkImageIOFactory.cxx
itkIOCommon.cxx
itkNumericSeriesFileNames.cxx
itkImageIOBase.cxx
itkRegularExpressionSeriesFileNames.cxx
itkStreamingImageIOBase.cxx
)

add_library(ITKIOBase ${ITKIOBase_SRC})
target_link_libraries(ITKIOBase  ${ITKCommon_LIBRARIES})
itk_module_target(ITKIOBase)

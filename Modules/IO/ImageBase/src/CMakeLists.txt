set(ITKIOImageBase_SRC
itkImageSeriesWriter.cxx
itkImageFileReaderException.cxx
itkImageFileWriter.cxx
itkArchetypeSeriesFileNames.cxx
itkImageIOFactory.cxx
itkIOCommon.cxx
itkNumericSeriesFileNames.cxx
itkImageIOBase.cxx
itkRegularExpressionSeriesFileNames.cxx
itkStreamingImageIOBase.cxx
)

add_library(ITKIOImageBase ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOImageBase_SRC})
target_link_libraries(ITKIOImageBase  ${ITKCommon_LIBRARIES})
itk_module_target(ITKIOImageBase)

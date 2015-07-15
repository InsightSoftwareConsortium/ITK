set(ITKDeprecated_SRC
itkAnalyzeImageIO.cxx
itkAnalyzeImageIOFactory.cxx
itkDICOMImageIO2.cxx
itkDICOMImageIO2Factory.cxx
itkDicomImageIOFactory.cxx
itkDICOMSeriesFileNames.cxx
itkSemaphore.cxx
)

add_library(ITKDeprecated ${ITKDeprecated_SRC})
itk_module_link_dependencies()
itk_module_target(ITKDeprecated)

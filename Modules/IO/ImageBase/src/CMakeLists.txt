set(ITKIOImageBase_SRCS
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

itk_module_add_library(ITKIOImageBase ${ITKIOImageBase_SRCS})

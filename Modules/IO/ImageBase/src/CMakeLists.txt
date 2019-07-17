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
  # Two non-templated utility functions that are needed by templated RAWImageIO
  itkRawImageIOUtilities.cxx
  )

itk_module_add_library(ITKIOImageBase ${ITKIOImageBase_SRCS})

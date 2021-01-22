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

if(BUILD_TESTING AND ITK_EXTERN_TEMPLATE_AND_EXPLICIT_INSTANTIATION)
  set(ExternTemplate_SRCS
    itkImageFileReaderExternTemplate.cxx
    )
  list(TRANSFORM ExternTemplate_SRCS PREPEND "../test/ExternTemplate/")
  list(APPEND ITKIOImageBase_SRCS ${ExternTemplate_SRCS})
endif()

itk_module_add_library(ITKIOImageBase ${ITKIOImageBase_SRCS})

if(BUILD_TESTING AND ITK_EXTERN_TEMPLATE_AND_EXPLICIT_INSTANTIATION)
  # Do not use these files in the install tree
  target_include_directories(ITKCommon PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../test/ExternTemplate>)
  target_compile_definitions(ITKCommon PUBLIC
    $<BUILD_INTERFACE:ITK_EXTERN_TEMPLATE_AND_EXPLICIT_INSTANTIATION>)
endif()

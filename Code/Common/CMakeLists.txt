# Sources of non-templated classes.
SOURCE_FILES(ITKCommon_SRCS
itkCommand 
itkDataObject 
itkDirectory 
itkDynamicLoader 
itkExceptionObject 
itkFastMutexLock 
itkIndent 
itkLightObject 
itkMeshRegion 
itkMultiThreader 
itkMutexLock 
itkNumericTraits 
itkObject 
itkObjectFactoryBase 
itkOutputWindow 
itkProcessObject 
itkRegion 
itkTimeStamp 
itkVersion 
itkImageIOCommon
itkImageIO
itkLightProcessObject
)

IF (WIN32)
  SOURCE_FILES(ITKCommon_SRCS itkWin32OutputWindow)
ELSE (WIN32)

ADD_LIBRARY(ITKCommon ITKCommon_SRCS)

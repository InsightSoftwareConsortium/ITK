# Sources of non-templated classes.
SOURCE_FILES(ITKCommon_SRCS
itkCommand 
itkDataObject 
itkDirectory 
itkDynamicLoader 
itkExceptionObject 
itkFastMutexLock 
itkFileOutputWindow
itkImageIOCommon
itkImageIO
itkIndent 
itkLightObject 
itkLightProcessObject
itkMeshRegion 
itkMultiThreader 
itkMutexLock 
itkNumericTraits 
itkObject 
itkObjectFactoryBase 
itkOutputWindow 
itkProcessObject 
itkRegion 
itkSimpleFastMutexLock
itkTimeStamp 
itkVersion 
itkVector
itkXMLFileOutputWindow
)

LINK_LIBRARIES (
VXLNumerics
)

IF (WIN32)
  SOURCE_FILES(ITKCommon_SRCS itkWin32OutputWindow)
ENDIF (WIN32)

ADD_LIBRARY(ITKCommon ITKCommon_SRCS)

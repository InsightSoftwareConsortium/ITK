# Sources of non-templated classes.
SOURCE_FILES(ITKCommon_SRCS
itkBloxItem
itkCommand 
itkDataObject 
itkDirectory 
itkDynamicLoader 
itkEventObject 
itkExceptionObject 
itkFastMutexLock 
itkFileOutputWindow
itkGaussianKernelFunction
itkImageIOCommon
itkImageIO
itkIndent 
itkLightObject 
itkLightProcessObject
itkKLMSegmentationBorder
itkKLMSegmentationRegion
itkMeshRegion 
itkMultiThreader 
itkMutexLock 
itkNumericTraits 
itkObject 
itkObjectFactoryBase 
itkOutputWindow 
itkProcessObject 
itkRegion
itkSegmentationBorder
itkSegmentationRegion 
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

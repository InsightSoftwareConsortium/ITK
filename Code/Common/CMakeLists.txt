LIBRARY(ITKCommon)

SOURCE_FILES(
itkCommand 
itkDataObject 
itkDirectory 
itkDynamicLoader 
itkExceptionObject 
itkFastMutexLock 
itkImageBase 
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
itkWriter
)


SOURCE_FILES_REQUIRE(WIN32 SOURCES_BEGIN itkWin32OutputWindow)

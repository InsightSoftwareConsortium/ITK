INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/IO)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/zlib)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/png)

LINK_LIBRARIES(ITKIO ITKCommon png zlib)

LINK_DIRECTORIES(${ITK_BINARY_DIR}/Utilities/png)
LINK_DIRECTORIES(${ITK_BINARY_DIR}/Utilities/zlib)
LINK_DIRECTORIES(${ITK_BINARY_DIR}/Code/IO)

ADD_EXECUTABLE(itkTestConvertBuffer itkTestConvertBuffer)
ADD_EXECUTABLE(itkTestLoadPNG itkTestLoadPNG)
#ADD_EXECUTABLE(itkTestRawImageIO itkTestRawImageIO)


# Add VTK code if we found VTK
INCLUDE (${CMAKE_ROOT}/Modules/FindVTK.cmake)
IF (USE_VTK_FILE)
  ADD_DEFINITIONS(-DITK_HAS_VTK)
  INCLUDE(${USE_VTK_FILE})
  LINK_LIBRARIES( 
      vtkRendering
      vtkGraphics	  
      vtkImaging
      vtkIO
      vtkFiltering
      vtkCommon
  )
ENDIF (USE_VTK_FILE)


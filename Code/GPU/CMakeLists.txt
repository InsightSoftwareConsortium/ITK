message("GPU acceleration using OpenCL is ON.")

FIND_PACKAGE ( OpenCL REQUIRED )
IF ( NOT ${OPENCL_FOUND} )
MESSAGE ( FATAL "Could not find OpenCL" )
ENDIF ()

set( itkCxxSRC itkGPUDataManager.cxx itkGPUContextManager.cxx itkGPUKernelManager.cxx itkOclUtil.cxx )

FILE(GLOB itkGPUSRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/ *.cl)
FILE(GLOB itkGPUHDR RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/ *.h)
FILE(GLOB itkTxxSRC  RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/ *.txx)

SOURCE_GROUP("OpenCL Source Files" FILES ${itkGPUSRC})

FILE(COPY ${itkGPUSRC} DESTINATION ${ITK_BINARY_DIR}/Code/GPU)
MESSAGE( "OpenCL files to copy " ${itkGPUSRC} )

configure_file(
  ${ITK_SOURCE_DIR}/Code/GPU/pathToOpenCLSourceCode.h.in
  ${ITK_BINARY_DIR}/Code/GPU/pathToOpenCLSourceCode.h)

# Include path
INCLUDE_DIRECTORIES( ${OPENCL_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR} ${ITK_BINARY_DIR}/Code/GPU )

# Create library
ADD_LIBRARY( ITKGPU ${itkCxxSRC} ${itkTxxSRC} ${itkGPUHDR} )
TARGET_LINK_LIBRARIES( ITKGPU ITKCommon ${OPENCL_LIBRARIES} )

# Test Executable
ADD_EXECUTABLE( test_itkGPUImage test_itkGPUImage.cxx )
TARGET_LINK_LIBRARIES( test_itkGPUImage ITKCommon ITKGPU ${OPENCL_LIBRARIES} )

ADD_EXECUTABLE( test_itkGPUImageFilter test_itkGPUImageFilter.cxx )
TARGET_LINK_LIBRARIES( test_itkGPUImageFilter ITKIO ITKCommon ITKGPU ${OPENCL_LIBRARIES} )

# ADD Test Executable to Testing
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)
ADD_TEST(gpuImageFilterTest ${EXECUTABLE_OUTPUT_PATH}/test_itkGPUImageFilter
   ${ITK_DATA_ROOT}/../../Examples/Data/BrainProtonDensitySlice.png
   ${TEMP}/gpuImageFilterTest.png)

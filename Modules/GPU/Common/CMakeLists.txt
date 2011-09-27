project(ITKGPUCommon)


IF (ITK_USE_GPU)

  FILE(GLOB itkGPUSRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/src *.cl)
  FILE(GLOB itkGPUHDR RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/include *.h)
  FILE(GLOB itkTxxSRC  RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/include *.txx)

  SOURCE_GROUP("OpenCL Source Files" FILES ${itkGPUSRC})

  FILE(COPY ${itkGPUSRC} DESTINATION ${ITK_BINARY_DIR}/GPU/Common)
  MESSAGE( "OpenCL files to copy " ${itkGPUSRC} )

  configure_file(
    src/pathToOpenCLSourceCode.h.in
    pathToOpenCLSourceCode.h)

   itk_module_impl()

ENDIF(ITK_USE_GPU)

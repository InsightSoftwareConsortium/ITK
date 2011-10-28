project(ITKGPUCommon)


if(ITK_USE_GPU)

  file(GLOB itkGPUSRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/src *.cl)
  file(GLOB itkGPUHDR RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/include *.h)
  file(GLOB itkTxxSRC  RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/include *.txx)

  source_group("OpenCL Source Files" FILES ${itkGPUSRC})

  file(COPY ${itkGPUSRC} DESTINATION ${ITK_BINARY_DIR}/GPU/Common)
  message( "OpenCL files to copy " ${itkGPUSRC} )

  configure_file(
    src/pathToOpenCLSourceCode.h.in
    pathToOpenCLSourceCode.h)

   itk_module_impl()

endif(ITK_USE_GPU)

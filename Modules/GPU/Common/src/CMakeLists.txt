set(ITKGPUCommon_SRC
itkGPUContextManager.cxx
itkGPUDataManager.cxx
itkGPUKernelManager.cxx
itkOclUtil.cxx
)

add_library(ITKGPUCommon ${ITKGPUCommon_SRC})
target_link_libraries(ITKGPUCommon ${ITKCommon_LIBRARIES} ${OPENCL_LIBRARIES} )
itk_module_target(ITKGPUCommon)

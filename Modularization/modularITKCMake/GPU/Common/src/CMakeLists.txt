set(ITK-GPUCommon_SRC
itkGPUContextManager.cxx
itkGPUDataManager.cxx
itkGPUKernelManager.cxx
itkOclUtil.cxx
)

add_library(ITK-GPUCommon ${ITK-GPUCommon_SRC})
target_link_libraries(ITK-GPUCommon ${ITK-Common_LIBRARIES} ${OPENCL_LIBRARIES} )
itk_module_target(ITK-GPUCommon)

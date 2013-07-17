if (ITK_USE_GPU)

   if(GPU_HANDLES_DOUBLE)
       add_definitions(-DWITH_DOUBLE)
   endif()

    set(GPU_SRC
    )

    set(GPU_Kernels
    GPUSmoothingRecursiveYvvGaussianImageFilter.cl
    )

    #essentially a #define GPU.
    add_definitions(-DGPU)

    if(NVIDIA_GPU)
        add_definitions(-DNVIDIA)
    endif()

    write_gpu_kernels("${GPU_Kernels}" GPU_SRC)

    add_library(${itk-module} ${GPU_SRC})
    target_link_libraries(${itk-module} ${INRIA_ITK_LIBRARIES} ${OPENCL_LIBRARIES} )

    #itk_module_target(${itk-module})

endif()

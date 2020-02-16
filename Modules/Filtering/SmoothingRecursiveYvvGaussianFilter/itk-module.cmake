# the top-level README is used for describing this module, just
# re-used it for documentation here
get_filename_component(MY_CURRENT_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
file(READ "${MY_CURRENT_DIR}/README.rst" DOCUMENTATION)

# itk_module() defines the module dependencies in
# SmoothingRecursiveYvvGaussianFilter
# The testing module in SmoothingRecursiveYvvGaussianFilter depends on
# ITKTestKernel
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
set(ModuleName "SmoothingRecursiveYvvGaussianFilter")
if(ITK_USE_GPU)
    itk_module(${ModuleName}
     DEPENDS
        ITKCommon
        ITKIOImageBase
        ITKImageFilterBase
        ITKSmoothing
        ITKGPUSmoothing
        ITKGPUCommon
     TEST_DEPENDS
        ITKTestKernel #to handle IO in src
        ITKGPUCommon
        ITKSmoothing
     EXCLUDE_FROM_DEFAULT
     DESCRIPTION
         "${DOCUMENTATION}"
    )
else()
    itk_module(${ModuleName}
     DEPENDS
        ITKCommon
        ITKIOImageBase
        ITKImageFilterBase
        ITKSmoothing
     TEST_DEPENDS
        ITKTestKernel #to handle IO in src
        ITKSmoothing
     EXCLUDE_FROM_DEFAULT
     DESCRIPTION
         "${DOCUMENTATION}"
    )
endif()

set(DOCUMENTATION "This module contains a collection of classes for performing
recursive gaussian filtering (Young Van Vliet implementation).")


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
     EXCLUDE_FROM_ALL
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
     EXCLUDE_FROM_DEFAULT
     DESCRIPTION
         "${DOCUMENTATION}"
    )
endif()

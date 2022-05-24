set(ITKImageIntensity_SRCS
        itkSymmetricEigenAnalysisImageFilter.cxx
        )
### generating libraries
itk_module_add_library(ITKImageIntensity ${ITKImageIntensity_SRCS})

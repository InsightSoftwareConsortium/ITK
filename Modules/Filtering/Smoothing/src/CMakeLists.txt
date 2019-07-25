set(ITKSmoothing_SRCS
        itkRecursiveGaussianImageFilter.cxx
        )
itk_module_add_library(ITKSmoothing ${ITKSmoothing_SRCS})

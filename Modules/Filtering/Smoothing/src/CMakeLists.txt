set(ITKSmoothing_SRCS
        itkFFTDiscreteGaussianImageFilter.cxx
        itkRecursiveGaussianImageFilter.cxx
        )
itk_module_add_library(ITKSmoothing ${ITKSmoothing_SRCS})

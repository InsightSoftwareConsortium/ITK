set(ITKDenoising_SRCS
        itkPatchBasedDenoisingBaseImageFilter.cxx
        )

itk_module_add_library(ITKDenoising ${ITKDenoising_SRCS})

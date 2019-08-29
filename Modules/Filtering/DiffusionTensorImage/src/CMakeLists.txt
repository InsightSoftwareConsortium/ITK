set(ITKDiffusionTensorImage_SRCS
    itkDiffusionTensor3DReconstructionImageFilter.cxx
        )

itk_module_add_library(ITKDiffusionTensorImage ${ITKDiffusionTensorImage_SRCS})

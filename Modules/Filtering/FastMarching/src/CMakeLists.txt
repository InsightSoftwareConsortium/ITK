set(ITKFastMarching_SRCS
        itkFastMarchingBase.cxx
        itkFastMarchingReachedTargetNodesStoppingCriterion.cxx
        itkFastMarchingImageFilter.cxx
        )

itk_module_add_library(ITKFastMarching ${ITKFastMarching_SRCS})

set(ITKImageFeature_SRCS
        itkMultiScaleHessianBasedMeasureImageFilter.cxx
        )

itk_module_add_library(ITKImageFeature ${ITKImageFeature_SRCS})

set(ITKColormap_SRCS
        itkScalarToRGBColormapImageFilter.cxx
        )

itk_module_add_library(ITKColormap ${ITKColormap_SRCS})

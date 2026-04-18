set(SimpleITKFilters_SRCS
        itkDICOMOrientation.cxx
        )

itk_module_add_library(SimpleITKFilters ${SimpleITKFilters_SRCS})
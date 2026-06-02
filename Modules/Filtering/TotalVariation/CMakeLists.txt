project(TotalVariation)

# Header-only module; the proxTV library is supplied by the ITKproxTV
# third-party module declared as a dependency in itk-module.cmake.
set(TotalVariation_LIBRARIES ${ITKproxTV_LIBRARIES})

itk_module_impl()

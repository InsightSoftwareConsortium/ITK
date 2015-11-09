set(ITKPath_SRC
itkChainCodePath2D.cxx
itkOrthogonallyCorrected2DParametricPath.cxx
itkParametricPath.cxx
)

add_library(ITKPath ${ITKPath_SRC})
itk_module_link_dependencies()
itk_module_target(ITKPath)

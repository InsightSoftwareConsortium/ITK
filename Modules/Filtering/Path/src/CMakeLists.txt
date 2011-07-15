set(ITKPath_SRC
itkChainCodePath2D.cxx
itkOrthogonallyCorrected2DParametricPath.cxx
itkParametricPath.cxx
)

add_library(ITKPath ${ITKPath_SRC})
target_link_libraries(ITKPath  ${ITKImageFilterBase_LIBRARIES} ${ITKImageFunction_LIBRARIES})
itk_module_target(ITKPath)

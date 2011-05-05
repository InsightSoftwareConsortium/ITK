set(ITK-Path_SRC
itkChainCodePath2D.cxx
itkOrthogonallyCorrected2DParametricPath.cxx
itkParametricPath.cxx
)

add_library(ITK-Path ${ITK-Path_SRC})
target_link_libraries(ITK-Path  ${ITK-ImageFilterBase_LIBRARIES} ${ITK-ImageFunction_LIBRARIES})
itk_module_target(ITK-Path)

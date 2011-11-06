set(ITKImageSource_SRC
  itkGaborKernelFunction.cxx
)

add_library( ITKImageSource ${ITKImageSource_SRC} )
target_link_libraries( ITKImageSource ${ITKCommon_LIBRARIES} )
itk_module_target( ITKImageSource )

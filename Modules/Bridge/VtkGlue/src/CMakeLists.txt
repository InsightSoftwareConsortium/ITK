set(ITKVtkGlue_SRCS
QuickView.cxx
)

# VTK 5.8 and greater defines VTK_LIBRARIES
if(NOT VTK_LIBRARIES)
  set(VTK_LIBRARIES vtkRendering)
endif(NOT VTK_LIBRARIES)

### generating libraries
add_library( ITKVtkGlue ${ITKVtkGlue_SRCS})
target_link_libraries(ITKVtkGlue
    ${ITKCommon_LIBRARIES}
    ${ITKVTK_LIBRARIES}
    ${VTK_LIBRARIES}
)

itk_module_target(ITKVtkGlue)

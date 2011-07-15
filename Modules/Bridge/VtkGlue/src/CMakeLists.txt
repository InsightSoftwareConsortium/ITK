set(ITKVtkGlue_SRCS
QuickView.cxx
)

### generating libraries
add_library( ITKVtkGlue ${ITKVtkGlue_SRCS})
target_link_libraries(ITKVtkGlue
    ${ITKCommon_LIBRARIES}
    ${ITKVTK_LIBRARIES}
    ${VTK_LIBRARIES}
)

itk_module_target(ITKVtkGlue)

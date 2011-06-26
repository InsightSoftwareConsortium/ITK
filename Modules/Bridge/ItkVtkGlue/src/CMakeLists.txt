set(ITK-ItkVtkGlue_SRCS
QuickView.cxx
)

### generating libraries
add_library( ITK-ItkVtkGlue ${ITK-ItkVtkGlue_SRCS})
target_link_libraries(ITK-ItkVtkGlue
    ${ITK-Common_LIBRARIES}
    ${ITK-VTK_LIBRARIES}
    ${VTK_LIBRARIES}
)

itk_module_target(ITK-ItkVtkGlue)

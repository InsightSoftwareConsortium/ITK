if(NOT VTK_RENDERING_BACKEND STREQUAL "None")
  set(ITKVtkGlue_SRCS QuickView.cxx)

  itk_module_add_library(ITKVtkGlue ${ITKVtkGlue_SRCS})
  target_link_libraries(ITKVtkGlue PUBLIC ${ITKVtkGlue_VTK_LIBRARIES})
endif()

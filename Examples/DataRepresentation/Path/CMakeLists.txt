project(PathExamples)

add_executable(PolyLineParametricPath1 PolyLineParametricPath1.cxx)
target_link_libraries(
  PolyLineParametricPath1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKPathModule
)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()

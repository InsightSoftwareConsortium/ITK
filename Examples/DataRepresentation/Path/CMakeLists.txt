project(PathExamples)
include_regular_expression("^.*$")


add_executable(PolyLineParametricPath1 PolyLineParametricPath1.cxx )
target_link_libraries(PolyLineParametricPath1 ${ITK_LIBRARIES})

if(BUILD_TESTING)

itk_add_test(NAME PolyLineParametricPath1 COMMAND PolyLineParametricPath1
    ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
)

endif(BUILD_TESTING)

set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)
include_regular_expression("^.*$")

add_executable(TreeContainer TreeContainer.cxx )
target_link_libraries(TreeContainer ITKCommon)



SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(TreeContainer TreeContainer.cxx )
TARGET_LINK_LIBRARIES(TreeContainer ITKCommon)



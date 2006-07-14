PROJECT(PathExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

IF(NOT BORLAND)
ADD_EXECUTABLE(PolyLineParametricPath1 PolyLineParametricPath1.cxx )
TARGET_LINK_LIBRARIES(PolyLineParametricPath1 ITKCommon ITKIO)

ADD_TEST(PolyLineParametricPath1 ${CXX_TEST_PATH}/PolyLineParametricPath1
    ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
)
ENDIF(NOT BORLAND)

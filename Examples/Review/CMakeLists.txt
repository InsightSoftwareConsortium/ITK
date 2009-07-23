PROJECT(ReviewExamples)

ADD_EXECUTABLE(ScalarSinglePhaseDense2D itkScalarSinglePhaseDense2DTest.cxx )
TARGET_LINK_LIBRARIES(ScalarSinglePhaseDense2D ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics )

ADD_EXECUTABLE(ScalarSinglePhaseSparse2D itkScalarSinglePhaseSparse2DTest.cxx )
TARGET_LINK_LIBRARIES(ScalarSinglePhaseSparse2D ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics )

#SET(REVIEW_EXAMPLES ${CXX_TEST_PATH}/ReviewExamples)
#SET(BASELINE ${ITK_DATA_ROOT}/myDataDirectory)
#SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)
#
#IF( NOT ITK_DISABLE_CXX_TESTING )
#  IF(BUILD_TESTING)
#
#    ADD_TEST(MyFilter ${REVIEW_EXAMPLES}
#      --compare ${BASELINE}/myTestOutputReference.png
#                ${TEMP}/myTestOutput.png
#      MyFilter
#            ${ITK_SOURCE_DIR}/Examples/Data/myTestInput.png
#            ${TEMP}/myTestOutput.png
#            myTestParameters 
#    )
#
#  ENDIF(BUILD_TESTING)
#ENDIF( NOT ITK_DISABLE_CXX_TESTING )


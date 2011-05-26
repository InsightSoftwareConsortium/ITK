project(ReviewExamples)

add_executable(ScalarSinglePhaseDense2D itkScalarSinglePhaseDense2DTest.cxx )
target_link_libraries(ScalarSinglePhaseDense2D ${ITK_LIBRARIES})

add_executable(ScalarSinglePhaseSparse2D itkScalarSinglePhaseSparse2DTest.cxx )
target_link_libraries(ScalarSinglePhaseSparse2D ${ITK_LIBRARIES})

#set(REVIEW_EXAMPLES ReviewExamples)
#set(BASELINE ${ITK_DATA_ROOT}/myDataDirectory)
#set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)
#
#if( NOT ITK_DISABLE_CXX_TESTING )
#  if(BUILD_TESTING)
#
#    add_test(MyFilter ${REVIEW_EXAMPLES}
#      --compare ${BASELINE}/myTestOutputReference.png
#                ${TEMP}/myTestOutput.png
#      MyFilter
#            ${ITK_SOURCE_DIR}/Examples/Data/myTestInput.png
#            ${TEMP}/myTestOutput.png
#            myTestParameters
#    )
#
#  endif(BUILD_TESTING)
#endif( NOT ITK_DISABLE_CXX_TESTING )


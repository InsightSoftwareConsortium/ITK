
add_executable(StitchingTest StitchingTest.cxx)
target_link_libraries(StitchingTest ${ITK_LIBRARIES})

add_executable(ThresholdingTest ThresholdingTest.cxx)
target_link_libraries(ThresholdingTest ${ITK_LIBRARIES})

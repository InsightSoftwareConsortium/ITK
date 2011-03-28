add_executable(itkTestDriver itkTestDriver.cxx)
target_link_libraries(itkTestDriver ${ITK-TestKernel_LIBRARIES})
install(TARGETS itkTestDriver DESTINATION bin)

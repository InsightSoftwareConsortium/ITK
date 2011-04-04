add_executable(itkTestDriver itkTestDriver.cxx)
target_link_libraries(itkTestDriver ${ITK-TestKernel_LIBRARIES})
itk_module_target_install(itkTestDriver)

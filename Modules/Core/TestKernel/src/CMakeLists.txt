if(NOT DO_NOT_BUILD_ITK_TEST_DRIVER)
  add_executable(itkTestDriver itkTestDriver.cxx)
  target_link_libraries(itkTestDriver
    LINK_PRIVATE ${ITK_MODULE_ITKTestKernel_PRIVATE_DEPENDS}
    LINK_PUBLIC ${ITK_MODULE_ITKTestKernel_PUBLIC_DEPENDS} ${ITKTestKernel_LIBRARIES})
  itk_module_target_label(itkTestDriver)
  itk_module_target_export(itkTestDriver)

  if(NOT DO_NOT_INSTALL_ITK_TEST_DRIVER) # used only by vcpkg
    itk_module_target_install(itkTestDriver)
  endif()
endif()

set(ITKTestKernel_SRCS
  itkTestDriverIncludeRequiredFactories.cxx
  itkTestDriverInclude.cxx
  itkTestingExtractSliceImageFilter.cxx
  itkTestingHashImageFilter.cxx)

itk_module_add_library(ITKTestKernel ${ITKTestKernel_SRCS})

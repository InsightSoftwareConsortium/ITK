set(
  ITKTestKernel_SRCS
  itkTestDriverInclude.cxx
  itkTestDriverIncludeRequiredFactories.cxx
  itkTestingExtractSliceImageFilter.cxx
  itkTestingHashImageFilter.cxx
)

itk_module_add_library(ITKTestKernel ${ITKTestKernel_SRCS})

if(NOT DO_NOT_BUILD_ITK_TEST_DRIVER)
  add_executable(itkTestDriver itkTestDriver.cxx)
  target_link_libraries(itkTestDriver PRIVATE ${ITKTestKernel_LIBRARIES})

  itk_target_attach_windows_utf8_manifest(itkTestDriver)
  itk_module_target_label(itkTestDriver)
  itk_module_target_export(itkTestDriver)
  target_link_options(
    itkTestDriver
    PRIVATE
      "$<$<AND:$<C_COMPILER_ID:AppleClang>,$<VERSION_GREATER_EQUAL:$<C_COMPILER_VERSION>,15.0>>:LINKER:-no_warn_duplicate_libraries>"
  )

  if(NOT DO_NOT_INSTALL_ITK_TEST_DRIVER) # used only by vcpkg
    if(
      IS_ABSOLUTE
        "${ITK_INSTALL_RUNTIME_DIR}"
      OR
        IS_ABSOLUTE
          "${ITK_INSTALL_LIBRARY_DIR}"
    )
      # ITKModuleMacros assumes relative too, so the install tree is already
      # wrong by this point (issue #6896).
      message(
        FATAL_ERROR
        "ITK_INSTALL_RUNTIME_DIR ('${ITK_INSTALL_RUNTIME_DIR}') and ITK_INSTALL_LIBRARY_DIR ('${ITK_INSTALL_LIBRARY_DIR}') must be relative to CMAKE_INSTALL_PREFIX."
      )
    else()
      file(
        RELATIVE_PATH
        _itk_test_driver_rpath
        "${CMAKE_INSTALL_PREFIX}/${ITK_INSTALL_RUNTIME_DIR}"
        "${CMAKE_INSTALL_PREFIX}/${ITK_INSTALL_LIBRARY_DIR}"
      )
      if(APPLE)
        set(_itk_test_driver_rpath "@loader_path/${_itk_test_driver_rpath}")
      else()
        set(_itk_test_driver_rpath "$ORIGIN/${_itk_test_driver_rpath}")
      endif()
      set_property(
        TARGET
          itkTestDriver
        APPEND
        PROPERTY
          INSTALL_RPATH
            "${_itk_test_driver_rpath}"
      )
      unset(_itk_test_driver_rpath)
    endif()
    itk_module_target_install(itkTestDriver)
  endif()
endif()

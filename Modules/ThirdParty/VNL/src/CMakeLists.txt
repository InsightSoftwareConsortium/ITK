set(VXL_INSTALL_EXPORT_NAME "${ITK-VNL-targets}")
set(VXL_INSTALL_INCLUDE_DIR "${ITK-VNL_INSTALL_INCLUDE_DIR}")
set(VXL_INSTALL_RUNTIME_DIR "${ITK-VNL_INSTALL_RUNTIME_DIR}")
set(VXL_INSTALL_LIBRARY_DIR "${ITK-VNL_INSTALL_LIBRARY_DIR}")
set(VXL_INSTALL_ARCHIVE_DIR "${ITK-VNL_INSTALL_ARCHIVE_DIR}")
set(VXL_INSTALL_NO_DEVELOPMENT ${ITK_INSTALL_NO_DEVELOPMENT})
set(VXL_INSTALL_NO_LIBRARIES ${ITK_INSTALL_NO_LIBRARIES})

# vxl uses EXECUTABLE_OUTPUT_PATH to refer to test executable locations
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
set(LIBRARY_OUTPUT_PATH ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

add_subdirectory(vxl)
foreach(lib itkvcl itkv3p_netlib itkv3p_lsqr itktestlib itkvnl itkvnl_algo)
  itk_module_target(${lib} NO_INSTALL)
endforeach()

foreach(exe
    netlib_integral_test
    netlib_lbfgs_example
    netlib_lbfgsb_example
    netlib_slamch_test
    testlib_test_all
    testlib_test_include
    testlib_test_link
    vcl_test_all
    vcl_test_include
    vnl_algo_test_all
    vnl_algo_test_include
    vnl_basic_operation_timings
    vnl_test_all
    vnl_test_include
    )
  if(TARGET ${exe}) # some vxl targets are conditional
    itk_module_target_label(${exe})
  endif()
endforeach()

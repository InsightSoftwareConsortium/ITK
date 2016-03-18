set(VXL_INSTALL_EXPORT_NAME "${ITKVNL-targets}")
set(VXL_INSTALL_INCLUDE_DIR "${ITKVNL_INSTALL_INCLUDE_DIR}")
set(VXL_INSTALL_RUNTIME_DIR "${ITKVNL_INSTALL_RUNTIME_DIR}")
set(VXL_INSTALL_LIBRARY_DIR "${ITKVNL_INSTALL_LIBRARY_DIR}")
set(VXL_INSTALL_ARCHIVE_DIR "${ITKVNL_INSTALL_ARCHIVE_DIR}")
set(VXL_INSTALL_NO_DEVELOPMENT ${ITK_INSTALL_NO_DEVELOPMENT})
set(VXL_INSTALL_NO_LIBRARIES ${ITK_INSTALL_NO_LIBRARIES})

# vxl uses EXECUTABLE_OUTPUT_PATH to refer to test executable locations
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
set(LIBRARY_OUTPUT_PATH ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

set(BUILD_DOCUMENTATION OFF)

add_subdirectory(vxl)

# The variable type of BUILD_EXAMPLES changed to INTERNAL type in VXL.
# Retrive the variable type to CACHE.
set(BUILD_EXAMPLES ${BUILD_EXAMPLES} CACHE BOOL "Build the examples from the ITK Software Guide." FORCE)

foreach(lib itkvcl itkv3p_netlib itktestlib itkvnl itkvnl_algo itknetlib)
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
    vnl_algo_test_template_include
    vnl_basic_operation_timings
    vnl_test_all
    vnl_test_include
    vnl_test_template_include
    )
  if(TARGET ${exe}) # some vxl targets are conditional
    itk_module_target_label(${exe})
  endif()
endforeach()

try_run(_libcxx_run_result _using_libcxx
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../CMake/itkLibCxxVersion.cxx
  RUN_OUTPUT_VARIABLE _libcxx_version
  )
if( ${_using_libcxx} )
  # This test is know to fail on libc++.  Do not run on versions it is known
  # to fail until fixed by upstream to avoid dashboard clutter.
  # See http://llvm.org/bugs/show_bug.cgi?id=17782
  # http://stackoverflow.com/questions/19725070/discrepancy-between-istreams-operator-double-val-between-libc-and-libstd/19727956#19727956
  # http://stackoverflow.com/questions/22233868/another-istream-discrepancy-between-libstdc-and-libc
  if( ${_libcxx_version} VERSION_LESS 1102 )
    itk_tests_ignore(vnl_test_na)
  endif()
endif()

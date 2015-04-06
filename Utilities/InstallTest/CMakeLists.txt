# Test "make install"
if("x${CMAKE_INSTALL_PREFIX}" STREQUAL "x${ITK_BINARY_DIR}/InstallTest")
  itk_add_test(NAME Install
    COMMAND ${CMAKE_COMMAND} -DCONFIGURATION=$<CONFIGURATION>
                             -DITK_BINARY_DIR=${ITK_BINARY_DIR}
                             -P ${CMAKE_CURRENT_SOURCE_DIR}/InstallTest.cmake
    )
endif()

set(NIFTI_NIFTILIB_NAME ${NIFTI_PACKAGE_PREFIX}niftiio)
add_library(${NIFTI_NIFTILIB_NAME} nifti1_io.c )
target_link_libraries( ${NIFTI_NIFTILIB_NAME} PUBLIC ${NIFTI_PACKAGE_PREFIX}znz)
target_include_directories(${NIFTI_NIFTILIB_NAME} PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<INSTALL_INTERFACE:${NIFTI_INSTALL_INCLUDE_DIR}>
        )

if(NIFTI_BUILD_APPLICATIONS)
  add_executable(${NIFTI_PACKAGE_PREFIX}nifti1_tool nifti1_tool.c)
  target_link_libraries(${NIFTI_PACKAGE_PREFIX}nifti1_tool PUBLIC ${NIFTI_NIFTILIB_NAME})
  set(NIFTI1_APPS ${NIFTI_PACKAGE_PREFIX}nifti1_tool)
endif()

# Set library version when building shared libs.
if(BUILD_SHARED_LIBS)
  set_target_properties(${NIFTI_NIFTILIB_NAME} PROPERTIES ${NIFTI_LIBRARY_PROPERTIES})
endif()

if(UNIX)
  target_link_libraries(${NIFTI_NIFTILIB_NAME} PUBLIC m)
endif()

if(NOT NIFTI_INSTALL_NO_LIBRARIES)
  install(TARGETS ${NIFTI_NIFTILIB_NAME} ${NIFTI1_APPS}
          EXPORT ${NIFTI_INSTALL_EXPORT_NAME}
          RUNTIME DESTINATION ${NIFTI_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
          LIBRARY DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
          ARCHIVE DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR} COMPONENT Development
          INCLUDES DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
          )
endif()

if(NOT NIFTI_INSTALL_NO_DEVELOPMENT)
  file(GLOB __files "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  install(FILES ${__files}
    DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
    COMPONENT Development)
endif()


if(NIFTI_BUILD_TESTING AND NIFTI_BUILD_APPLICATIONS)
  add_executable(${NIFTI_PACKAGE_PREFIX}nifti1_test nifti1_test.c)
  target_link_libraries(${NIFTI_PACKAGE_PREFIX}nifti1_test PUBLIC ${NIFTI_NIFTILIB_NAME})

  add_executable(nifti_first_test_program nifti_tester001.c)
  target_link_libraries(nifti_first_test_program ${NIFTI_PACKAGE_PREFIX}niftiio )

  add_executable(nifti_second_test_program nifti_tester002.c)
  target_link_libraries(nifti_second_test_program ${NIFTI_PACKAGE_PREFIX}niftiio )

  add_executable( ${NIFTI_PACKAGE_PREFIX}nifti_tester001 nifti_tester001.c)
  target_link_libraries(${NIFTI_PACKAGE_PREFIX}nifti_tester001 ${NIFTI_PACKAGE_PREFIX}niftiio )

  add_executable( ${NIFTI_PACKAGE_PREFIX}nifti_tester002 nifti_tester002.c)
  target_link_libraries(${NIFTI_PACKAGE_PREFIX}nifti_tester002 ${NIFTI_PACKAGE_PREFIX}niftiio )

  add_test( NAME nifti_first_test COMMAND $<TARGET_FILE:nifti_first_test_program> )
  add_test( NAME nifti_second_test COMMAND $<TARGET_FILE:nifti_second_test_program> )

  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_test_help COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>)
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_test_n1  COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>  -n1  ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${NIFTI_BINARY_DIR}/niftilib/n1 )
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_test_n2  COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>  -n2  ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${NIFTI_BINARY_DIR}/niftilib/n2 )
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_test_a2  COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>  -a2  ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${NIFTI_BINARY_DIR}/niftilib/a2 )
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_test_zn1 COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test> -zn1 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${NIFTI_BINARY_DIR}/niftilib/zn1 )
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_test_zn2 COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test> -zn2 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${NIFTI_BINARY_DIR}/niftilib/zn2 )
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_test_za2 COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test> -za2 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${NIFTI_BINARY_DIR}/niftilib/za2 )

  #The help screens always return 1; add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool1_help COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool>)
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_n1 COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool>  -disp_hdr -infile ${NIFTI_BINARY_DIR}/niftilib/n1 )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_n1 PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_n1)
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_n2 COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool>  -disp_hdr -infile ${NIFTI_BINARY_DIR}/niftilib/n2 )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_n2 PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_n2)
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_a2 COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool>  -disp_hdr -infile ${NIFTI_BINARY_DIR}/niftilib/a2 )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_a2 PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_a2)
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_zn1 COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -disp_hdr -infile ${NIFTI_BINARY_DIR}/niftilib/zn1 )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_zn1 PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_zn1)
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_zn2 COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -disp_hdr -infile ${NIFTI_BINARY_DIR}/niftilib/zn2 )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_zn2 PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_zn2)
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_za2 COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -disp_hdr -infile ${NIFTI_BINARY_DIR}/niftilib/za2 )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_za2 PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_za2)

  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_diff_hdr COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -diff_hdr -infile ${NIFTI_BINARY_DIR}/niftilib/zn1 ${NIFTI_BINARY_DIR}/niftilib/zn1 )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_diff_hdr PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_zn1)
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_diff_nims COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -diff_nim -infile ${NIFTI_BINARY_DIR}/niftilib/zn1 ${NIFTI_BINARY_DIR}/niftilib/zn1 )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_diff_nims PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_zn1)

  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_copy_brick_list COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -cbl -prefix cbl_zn1.nii -infile ${NIFTI_BINARY_DIR}/niftilib/zn1.nii.gz[0,1])
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_copy_brick_list PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_zn1)
  #add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_copy_collapsed_image /bin/sh ${NIFTI_SOURCE_DIR}/niftilib/rmthenrun cci_zn1.nii COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -cci 2 2 2 -1 -1 -1 -1 -prefix cci_zn1.nii -infile ${NIFTI_BINARY_DIR}/niftilib/zn1.nii.gz )
  #set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_copy_collapsed_image PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_zn1)
  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_disp_ci COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -disp_ci 2 2 2 -1 0 0 0  -infile ${NIFTI_BINARY_DIR}/niftilib/zn1.nii.gz )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_disp_ci PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_zn1)

  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_disp_ts COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -disp_ts 2 2 2 -infile ${NIFTI_BINARY_DIR}/niftilib/zn1.nii.gz )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_disp_ts PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_zn1)

  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_strip_extras COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -strip_extras -infile ${NIFTI_BINARY_DIR}/niftilib/zn1.nii.gz )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_strip_extras PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_zn1)

  # This test needs a file that has extensions to remove
  #add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_rm_ext /bin/sh ${NIFTI_SOURCE_DIR}/niftilib/rmthenrun rm_ext.nii COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -rm_ext ALL -prefix rm_ext.nii -infile ${NIFTI_BINARY_DIR}/niftilib/zn1.nii.gz )

  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_check_hdr COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -check_hdr -infile ${NIFTI_BINARY_DIR}/niftilib/za2 )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_check_hdr PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_za2)

  add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool_check_nim COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_tool> -check_nim -infile ${NIFTI_BINARY_DIR}/niftilib/za2 )
  set_tests_properties( ${NIFTI_PACKAGE_PREFIX}nifti1_tool_check_nim PROPERTIES DEPENDS ${NIFTI_PACKAGE_PREFIX}nifti1_test_za2)

endif()

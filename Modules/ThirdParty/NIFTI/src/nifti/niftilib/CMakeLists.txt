set(NIFTI_NIFTILIB_NAME ${NIFTI_PACKAGE_PREFIX}niftiio)

add_nifti_library(${NIFTI_NIFTILIB_NAME} nifti1_io.c )
target_link_libraries( ${NIFTI_NIFTILIB_NAME} PUBLIC ${NIFTI_PACKAGE_PREFIX}znz ${NIFTI_SYSTEM_MATH_LIB})
set_target_properties(
  ${NIFTI_NIFTILIB_NAME}
  PROPERTIES
    PUBLIC_HEADER
      "${CMAKE_CURRENT_LIST_DIR}/nifti1_io.h;${CMAKE_CURRENT_LIST_DIR}/nifti1.h"
  )
# Set library version when building shared libs.
if(BUILD_SHARED_LIBS)
    get_lib_version_vars("nifti1_io_version.h" NIFTI_VERSION NIFTI_MAJOR_VERSION)
    set_target_properties(${NIFTI_NIFTILIB_NAME}
      PROPERTIES
        ${NIFTI_LIBRARY_PROPERTIES}
        VERSION ${NIFTI_VERSION}
        SOVERSION ${NIFTI_MAJOR_VERSION}
        )
endif()
install_nifti_target(${NIFTI_NIFTILIB_NAME})

if(NIFTI_BUILD_APPLICATIONS)
  add_nifti_executable(${NIFTI_PACKAGE_PREFIX}nifti1_tool nifti1_tool.c)
  target_link_libraries(${NIFTI_PACKAGE_PREFIX}nifti1_tool PUBLIC ${NIFTI_NIFTILIB_NAME})
  install_nifti_target(${NIFTI_PACKAGE_PREFIX}nifti1_tool)
  install_man_page(
    ${NIFTI_PACKAGE_PREFIX}nifti1_tool
    OPTS "--help-option=-help;--version-option=-ver_man;--no-info"
    )
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

  # setup data to be used for other tests
  add_test( NAME nifti_first_test  COMMAND $<TARGET_FILE:nifti_first_test_program> )
  add_test( NAME nifti_second_test COMMAND $<TARGET_FILE:nifti_second_test_program> )

  # Do all regression tests
  set(TOOL_SUFFIX "1") # The suffxi used for this tool "1" for nifti1, and "" for nifti2
  set(TEST_PREFIX "${NIFTI_PACKAGE_PREFIX}nifti${TOOL_SUFFIX}")
  set(TOOL_NAME "${TEST_PREFIX}_tool")

  add_test( NAME ${TEST_PREFIX}_test_help COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>)
  add_test( NAME ${TEST_PREFIX}_test_n1   COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>  -n1 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/n1 )
  add_test( NAME ${TEST_PREFIX}_test_n2   COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>  -n2 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/n2 )
  add_test( NAME ${TEST_PREFIX}_test_a2   COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>  -a2 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/a2 )
  add_test( NAME ${TEST_PREFIX}_test_zn1  COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test> -zn1 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/zn1 )
  add_test( NAME ${TEST_PREFIX}_test_zn2  COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test> -zn2 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/zn2 )
  add_test( NAME ${TEST_PREFIX}_test_za2  COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test> -za2 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/za2 )

  add_test( NAME ${TEST_PREFIX}_ver          COMMAND $<TARGET_FILE:${TOOL_NAME}> -nifti_ver)
  add_test( NAME ${TEST_PREFIX}_tool_ver     COMMAND $<TARGET_FILE:${TOOL_NAME}> -ver)
  add_test( NAME ${TEST_PREFIX}_help         COMMAND $<TARGET_FILE:${TOOL_NAME}> -help)
  add_test( NAME ${TEST_PREFIX}_nifti_hist   COMMAND $<TARGET_FILE:${TOOL_NAME}> -nifti_hist)
  add_test( NAME ${TEST_PREFIX}_hist         COMMAND $<TARGET_FILE:${TOOL_NAME}> -hist)

  add_test( NAME ${TEST_PREFIX}_disp_hdr     COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr  -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
  add_test( NAME ${TEST_PREFIX}_disp_nim     COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_nim  -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
  add_test( NAME ${TEST_PREFIX}_disp_ext     COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_ext  -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
  add_test( NAME ${TEST_PREFIX}_header_check COMMAND $<TARGET_FILE:${TOOL_NAME}> -check_hdr -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
  add_test( NAME ${TEST_PREFIX}_nim_check    COMMAND $<TARGET_FILE:${TOOL_NAME}> -check_nim -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )

  set_tests_properties(${TEST_PREFIX}_disp_hdr PROPERTIES LABELS NEEDS_DATA)
  set_tests_properties(${TEST_PREFIX}_disp_nim PROPERTIES LABELS NEEDS_DATA)
  set_tests_properties(${TEST_PREFIX}_disp_ext PROPERTIES LABELS NEEDS_DATA)
  set_tests_properties(${TEST_PREFIX}_header_check PROPERTIES LABELS NEEDS_DATA)
  set_tests_properties(${TEST_PREFIX}_nim_check PROPERTIES LABELS NEEDS_DATA)

  #The help screens always return 1; add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool1_help COMMAND $<TARGET_FILE:${TOOL_NAME}>)
  add_test( NAME ${TEST_PREFIX}_tool_n1  COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/n1 )
  add_test( NAME ${TEST_PREFIX}_tool_n2  COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/n2 )
  add_test( NAME ${TEST_PREFIX}_tool_a2  COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/a2 )
  add_test( NAME ${TEST_PREFIX}_tool_zn1 COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1 )
  add_test( NAME ${TEST_PREFIX}_tool_zn2 COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/zn2 )
  add_test( NAME ${TEST_PREFIX}_tool_za2 COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/za2 )

  add_test( NAME ${TEST_PREFIX}_tool_diff_hdr         COMMAND $<TARGET_FILE:${TOOL_NAME}> -diff_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1 ${CMAKE_CURRENT_BINARY_DIR}/zn1 )
  add_test( NAME ${TEST_PREFIX}_tool_diff_nims        COMMAND $<TARGET_FILE:${TOOL_NAME}> -diff_nim -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1 ${CMAKE_CURRENT_BINARY_DIR}/zn1 )

  add_test( NAME ${TEST_PREFIX}_tool_copy_brick_list COMMAND $<TARGET_FILE:${TOOL_NAME}> -cbl -prefix cbl_zn1.nii -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz[0,1])
  add_test( NAME ${TEST_PREFIX}_tool_disp_ci         COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_ci 2 2 2 -1 0 0 0  -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz )

  add_test( NAME ${TEST_PREFIX}_tool_disp_ts         COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_ts 2 2 2 -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz )
  add_test( NAME ${TEST_PREFIX}_tool_strip_extras    COMMAND $<TARGET_FILE:${TOOL_NAME}> -strip_extras -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz )

  # This test needs a file that has extensions to remove
  #add_test( NAME ${TEST_PREFIX}_tool_rm_ext /bin/sh ${NIFTI_SOURCE_DIR}/niftilib/rmthenrun rm_ext.nii COMMAND $<TARGET_FILE:${TOOL_NAME}> -rm_ext ALL -prefix rm_ext.nii -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz )
  add_test( NAME ${TEST_PREFIX}_tool_check_hdr COMMAND $<TARGET_FILE:${TOOL_NAME}> -check_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/za2 )
  add_test( NAME ${TEST_PREFIX}_tool_check_nim COMMAND $<TARGET_FILE:${TOOL_NAME}> -check_nim -infile ${CMAKE_CURRENT_BINARY_DIR}/za2 )

  #add_test( NAME ${TEST_PREFIX}_tool_copy_collapsed_image /bin/sh ${NIFTI_SOURCE_DIR}/niftilib/rmthenrun cci_zn1.nii COMMAND $<TARGET_FILE:${TOOL_NAME}> -cci 2 2 2 -1 -1 -1 -1 -prefix cci_zn1.nii -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz )

  add_test( NAME cleanup_n1  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/n1.nii.gz  ${CMAKE_CURRENT_BINARY_DIR}/n1.nii )
  add_test( NAME cleanup_n2  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/n2.nii     ${CMAKE_CURRENT_BINARY_DIR}/n2.hdr ${CMAKE_CURRENT_BINARY_DIR}/n2.img)
  add_test( NAME cleanup_a2  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/a2.img     ${CMAKE_CURRENT_BINARY_DIR}/a2.hdr )
  add_test( NAME cleanup_zn1 COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/cbl_zn1.nii )
  add_test( NAME cleanup_zn2 COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/zn2.img.gz ${CMAKE_CURRENT_BINARY_DIR}/zn2.hdr.gz )
  add_test( NAME cleanup_za2 COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/za2.img.gz ${CMAKE_CURRENT_BINARY_DIR}/za2.hdr.gz )

  # https://crascit.com/2016/10/18/test-fixtures-with-cmake-ctest/ as of cmake 3.7 fixtures are supported
  foreach( testsuffix "n1" "n2" "a2" "zn1" "zn2" "za2")
    #message(STATUS "Setting up ${testsuffix}")
    set_tests_properties( ${TEST_PREFIX}_tool_${testsuffix}
            PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_${testsuffix} )
    set_tests_properties( ${TEST_PREFIX}_test_${testsuffix}
            PROPERTIES FIXTURES_SETUP NiftiTestGeneratedFiles_${testsuffix})
      message(STATUS "NiftiTestGeneratedFiles_${testsuffix}" )
    set_tests_properties( ${TEST_PREFIX}_tool_${testsuffix} ${TEST_PREFIX}_test_${testsuffix}
            PROPERTIES RESOURCE_LOCK Serial_${testsuffix}
            LABELS NEEDS_DATA
            )
    set_tests_properties(cleanup_${testsuffix} PROPERTIES FIXTURES_CLEANUP  NiftiTestGeneratedFiles_${testsuffix} )
  endforeach()

  set_tests_properties( ${TEST_PREFIX}_tool_diff_hdr         PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_diff_nims        PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_copy_brick_list  PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_disp_ci          PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_disp_ts          PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_strip_extras     PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_check_hdr        PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_za2  LABELS NEEDS_DATA)
  set_tests_properties( ${TEST_PREFIX}_tool_check_nim        PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_za2  LABELS NEEDS_DATA)
  #set_tests_properties( ${TEST_PREFIX}_tool_copy_collapsed_image PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFileszn1)

  set_tests_properties( ${TEST_PREFIX}_tool_diff_hdr ${TEST_PREFIX}_tool_diff_nims
          ${TEST_PREFIX}_tool_copy_brick_list
          ${TEST_PREFIX}_tool_disp_ci ${TEST_PREFIX}_tool_disp_ts ${TEST_PREFIX}_tool_strip_extras
          PROPERTIES RESOURCE_LOCK Serial_zn1
          LABELS NEEDS_DATA
          )
  #==END NIFTI1 and NIFTI2 common tests ============================================

  unset(TEST_SUFFIX)
  unset(TEST_PREFIX)
  unset(TOOL_NAME)
endif()

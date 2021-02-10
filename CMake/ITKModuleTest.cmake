# This file contains CMake functions and macros used when testing ITK modules.

#-----------------------------------------------------------------------------
# Create source code, compile and link a test driver
# Two variables must be defined before including this file.
#   KIT should define a unique name for the test driver.
#   KitTests should contain a list of test file names.
# Arguments - Input
#   KIT - the name of the test directory
#   KIT_LIBS - a list of libraries needed to link the test driver
#   KitTests - a list of tests to be included in the test driver
#   ADDITIONAL_SRC (optional) - additional source files, which don't contain tests

macro(CreateTestDriver KIT KIT_LIBS KitTests)
  set(ADDITIONAL_SRC ${ARGN})
  if(EMSCRIPTEN)
    set(emscripten_before "
EM_ASM(
  var cmake_source_dir = '${CMAKE_SOURCE_DIR}'.split('/');
  // This is intentionally global so it can be unmounted at the end.
  source_mount_dir = null;
  if(cmake_source_dir[1] === 'home') {
    source_mount_dir = cmake_source_dir.slice(0, 3).join('/');
    }
  else {
    source_mount_dir = cmake_source_dir.slice(0, 2).join('/');
    }
  FS.mkdir(source_mount_dir);
  FS.mount(NODEFS, { root: source_mount_dir }, source_mount_dir);

  var cmake_binary_dir = '${CMAKE_BINARY_DIR}'.split('/');
  // This is intentionally global so it can be unmounted at the end.
  binary_mount_dir = null;
  if(cmake_binary_dir[1] === 'home') {
    binary_mount_dir = cmake_binary_dir.slice(0, 3).join('/');
    }
  else {
    binary_mount_dir = cmake_binary_dir.slice(0, 2).join('/');
    }
  if(source_mount_dir != binary_mount_dir) {
    FS.mkdir(binary_mount_dir);
    FS.mount(NODEFS, { root: binary_mount_dir }, binary_mount_dir);
    }
  );
")
    set(emscripten_after "
EM_ASM(
  FS.unmount(source_mount_dir);
  if(source_mount_dir != binary_mount_dir) {
    FS.unmount(binary_mount_dir);
    }
  );
")
  endif()
  set(CMAKE_TESTDRIVER_BEFORE_TESTMAIN "${emscripten_before}#include \"itkTestDriverBeforeTest.inc\"")
  set(CMAKE_TESTDRIVER_AFTER_TESTMAIN "#include \"itkTestDriverAfterTest.inc\"${emscripten_after}")
  create_test_sourcelist(Tests ${KIT}TestDriver.cxx
    ${KitTests}
    EXTRA_INCLUDE itkTestDriverIncludeRequiredIOFactories.h
    FUNCTION  ProcessArgumentsAndRegisterRequiredFactories
    )
  add_executable(${KIT}TestDriver ${KIT}TestDriver.cxx ${Tests} ${ADDITIONAL_SRC})
  target_link_libraries(${KIT}TestDriver LINK_PUBLIC ${KIT_LIBS} ${ITKTestKernel_LIBRARIES})
  itk_module_target_label(${KIT}TestDriver)
endmacro()


macro(CreateTestDriver_SupportBuildInIOFactories KIT KIT_LIBS KitTests)
  set(ADDITIONAL_SRC ${ARGN} )
  if(EMSCRIPTEN)
    set(emscripten_before "
EM_ASM(
  var cmake_source_dir = '${CMAKE_SOURCE_DIR}'.split('/');
  // This is intentionally global so it can be unmounted at the end.
  source_mount_dir = null;
  if(cmake_source_dir[1] === 'home') {
    source_mount_dir = cmake_source_dir.slice(0, 3).join('/');
    }
  else {
    source_mount_dir = cmake_source_dir.slice(0, 2).join('/');
    }
  FS.mkdir(source_mount_dir);
  FS.mount(NODEFS, { root: source_mount_dir }, source_mount_dir);

  // This is intentionally global so it can be unmounted at the end.
  binary_mount_dir = null;
  var cmake_binary_dir = '${CMAKE_BINARY_DIR}'.split('/');
  if(cmake_binary_dir[1] === 'home') {
    binary_mount_dir = cmake_binary_dir.slice(0, 3).join('/');
    }
  else {
    binary_mount_dir = cmake_binary_dir.slice(0, 2).join('/');
    }
  if(source_mount_dir != binary_mount_dir) {
    FS.mkdir(binary_mount_dir);
    FS.mount(NODEFS, { root: binary_mount_dir }, binary_mount_dir);
    }
  );
")
    set(emscripten_after "
EM_ASM(
  FS.unmount(source_mount_dir);
  if(source_mount_dir != binary_mount_dir) {
    FS.unmount(binary_mount_dir);
    }
  );
")
  endif()
  set(CMAKE_TESTDRIVER_BEFORE_TESTMAIN "${emscripten_before}#include \"itkTestDriverBeforeTest.inc\"")
  set(CMAKE_TESTDRIVER_AFTER_TESTMAIN "#include \"itkTestDriverAfterTest.inc\"${emscripten_after}")
  create_test_sourcelist(Tests ${KIT}TestDriver.cxx
    ${KitTests}
    EXTRA_INCLUDE  itkTestDriverIncludeBuiltInIOFactories.h
    FUNCTION  ProcessArgumentsAndRegisterBuiltInFactories
    )
  add_executable(${KIT}TestDriver ${KIT}TestDriver.cxx ${Tests} ${ADDITIONAL_SRC})
  target_link_libraries(${KIT}TestDriver LINK_PUBLIC ${KIT_LIBS} ${ITKTestKernel_LIBRARIES})
  itk_module_target_label(${KIT}TestDriver)
endmacro()

#-----------------------------------------------------------------------------
# ITK wrapper for add_test that automatically sets the test's LABELS property
# to the value of its containing module.
#
function(itk_add_test)
  # Add tests with data in the ITKData group.
  ExternalData_add_test(ITKData ${ARGN})

  if("NAME" STREQUAL "${ARGV0}")
    set(_iat_testname ${ARGV1})
  else()
    set(_iat_testname ${ARGV0})
  endif()

  if(itk-module)
    set(_label ${itk-module})
    if(TARGET ${itk-module}-all AND "${ARGN}" MATCHES "DATA{")
      add_dependencies(${itk-module}-all ITKData)
    endif()
  else()
    set(_label ${main_project_name})
  endif()

  set_property(TEST ${_iat_testname} PROPERTY LABELS ${_label})
endfunction()

#-----------------------------------------------------------------------------
# ITK wrapper for add_test that runs the given Python script with a Python
# executable exposed to ITK's build tree Python wrapping
#
# Usage:
#
#  itk_python_add_test(NAME testName
#    TEST_DRIVER_ARGS --compare testOutput.mha testBaseline.mha
#    SCRIPT testPythonScript.py argv1 argv2 argv3
#    )
#
# where the named arguments are:
#
# NAME             - test name
# TEST_DRIVER_ARGS - additional arguments to the itkTestDriver executable
# COMMAND          - Python test script and its arguments
#
function(itk_python_add_test)
  # No-op if wrapping is not available
  if(NOT ITK_WRAP_PYTHON)
    if(DEFINED ITK_SOURCE_DIR)
      message(FATAL_ERROR "`itk_python_add_test` should never be called if `ITK_WRAP_PYTHON` if OFF")
    else()
      return()
    endif()
  endif()

  set(options )
  set(oneValueArgs NAME)
  set(multiValueArgs TEST_DRIVER_ARGS COMMAND)
  cmake_parse_arguments(PYTHON_ADD_TEST "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  set(command "${Python3_EXECUTABLE}")
  # add extra command which may be needed on some systems
  if(CMAKE_OSX_ARCHITECTURES)
    list(GET CMAKE_OSX_ARCHITECTURES 0 test_arch)
    set(command arch -${test_arch} ${command})
  endif()

  if(ITK_DIR)
    set(itk_wrap_python_binary_dir "${ITK_DIR}/Wrapping/Generators/Python")
  else()
    set(itk_wrap_python_binary_dir "${ITK_BINARY_DIR}/Wrapping/Generators/Python")
  endif()
  # itk_wrap_python_binary_dir *MUST* contain the WrapITK.pth file
  # Final installed version of ITK will leverage the WrapITK.pth paths, so
  # the test environment should also use those same paths.
  set(WrapITK_PTH_FILE "${itk_wrap_python_binary_dir}/WrapITK.pth")
  if(NOT EXISTS ${WrapITK_PTH_FILE})
    message(FATAL_ERROR "${WrapITK_PTH_FILE} must exist.")
  endif()
  unset(WrapITK_PTH_FILE)

  itk_add_test(NAME ${PYTHON_ADD_TEST_NAME}
    COMMAND itkTestDriver
      --add-before-env PYTHONPATH "${itk_wrap_python_binary_dir}"  # parent directory of the itk package
      --add-before-env PYTHONPATH "${ITK_PYTHON_PACKAGE_DIR}"      # package directory and shared libraries + swig artifacts
      --add-before-libpath "${ITK_PYTHON_PACKAGE_DIR}"             # itk non-wrapping shared libs
      ${PYTHON_ADD_TEST_TEST_DRIVER_ARGS}
      ${command}
      ${PYTHON_ADD_TEST_COMMAND}
    WORKING_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}"
  )
  set_property(TEST ${PYTHON_ADD_TEST_NAME} APPEND PROPERTY LABELS Python)
endfunction()

#-----------------------------------------------------------------------------
# ITK wrapper for add_test that runs the given Python expression to test the
# instantiation of a wrapped ITK Python class
#
# Usage:
#
#  itk_python_expression_add_test(NAME testName
#    EXPRESSION "image = itk.Image.New()"
#    )
#
# where the named arguments are:
#
# NAME       - test name
# EXPRESSION - expression to instantiate the object
#
function(itk_python_expression_add_test)
  # No-op if wrapping is not available
  if(NOT ITK_WRAP_PYTHON)
    if(DEFINED ITK_SOURCE_DIR)
      message(FATAL_ERROR "`itk_python_expression_add_test` should never be called if `ITK_WRAP_PYTHON` if OFF")
    else()
      return()
    endif()
  endif()

  set(options )
  set(oneValueArgs NAME EXPRESSION)
  set(multiValueArgs )
  cmake_parse_arguments(PYTHON_EXPRESSION_ADD_TEST "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  itk_python_add_test(NAME ${PYTHON_EXPRESSION_ADD_TEST_NAME}
    COMMAND -c "import itk$<SEMICOLON> itk.auto_progress(2)$<SEMICOLON> ${PYTHON_EXPRESSION_ADD_TEST_EXPRESSION}"
    )
endfunction()

function(CreateGoogleTestDriver KIT KIT_LIBS KitTests)
  set(exe "${KIT}GTestDriver")
  add_executable(${exe} ${KitTests} )
  target_link_libraries(${exe} ${KIT_LIBS} GTest::GTest GTest::Main)
  itk_module_target_label(${exe})

  include(GoogleTest)

  # CMake 3.10 added this method, to avoid configure time introspection.
  # Verion 3.10.3 is needed for the DISCOVERY_TIMEOUT method
  if(NOT CMAKE_CROSSCOMPILING AND NOT ${CMAKE_VERSION} VERSION_LESS "3.10.3" )
    gtest_discover_tests( ${exe} DISCOVERY_TIMEOUT 15 )
  else()
    set(_skip_dependency)
    if( ITK_SKIP_GTEST_DEPENDANCY_AUTO_CHECK )
      # This advanced behavior is only available through the
      # command line.  It is intended to be used only when writing GoogleTests,
      # to require the developer to explicitly ask for introspection of
      # gtest instrumented files by the time-consumeing re-running of cmake
      # for the entire project
      #
      # use "cmake -DITK_SKIP_GTEST_DEPENDANCY_AUTO_CHECK:BOOL=ON ."  to
      #           Disable the slow introspection (i.e. while writing gtests)
      # use "cmake -DITK_SKIP_GTEST_DEPENDANCY_AUTO_CHECK:BOOL=OFF ." to
      #           enable the slow introspection (during all other development)
      set(_skip_dependency "SKIP_DEPENDENCY")
    endif()
    gtest_add_tests(TARGET ${exe}
      ${_skip_dependency}
      )
  endif()
  # TODO need to label the produced ctests
endfunction()

#-----------------------------------------------------------------------------
# ITK function to ignore a test
#
function(itk_tests_ignore)
  set_property(GLOBAL APPEND PROPERTY CTEST_CUSTOM_TESTS_IGNORE ${ARGN})
endfunction()

#-----------------------------------------------------------------------------
# ITK function to ignore a test during MemCheck
#
function(itk_memcheck_ignore)
  set_property(GLOBAL APPEND PROPERTY CTEST_CUSTOM_MEMCHECK_IGNORE ${ARGN})
endfunction()

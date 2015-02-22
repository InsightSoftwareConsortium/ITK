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
  target_link_libraries(${KIT}TestDriver ${KIT_LIBS} ${ITKTestKernel_LIBRARIES})
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
  target_link_libraries(${KIT}TestDriver ${KIT_LIBS} ${ITKTestKernel_LIBRARIES})
  itk_module_target_label(${KIT}TestDriver)
endmacro()

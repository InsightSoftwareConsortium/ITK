#
# Create source code, compile and link a test driver
# Two variables must be defined before including this file.
#   KIT should define a unique name for the test driver.
#   KitTests should contain a list of test file names without a .cxx suffix.
# Arguments - Input
#   KIT - the name of the test directory
#   KIT_LIBS - a list of libraries needed to link the test driver
#   KitTests - a list of tests to be included in the test driver
# Arguments - Output
#   TestDriver - the name of the test driver.

macro(CreateTestDriver KIT KIT_LIBS KitTests TestDriver)
  set(CMAKE_TESTDRIVER_BEFORE_TESTMAIN "#include \"itkTestDriverBeforeTest.inc\"")
  set(CMAKE_TESTDRIVER_AFTER_TESTMAIN "#include \"itkTestDriverAfterTest.inc\"")
  create_test_sourcelist(Tests ${KIT}CxxTests.cxx
    ${KitTests}
    EXTRA_INCLUDE itkTestDriverInclude.h
    FUNCTION ProcessArguments
    )
  add_executable(${KIT}CxxTests ${KIT}CxxTests.cxx ${Tests})
  target_link_libraries(${KIT}CxxTests ${KIT_LIBS})
  set(${TestDriver} ${EXECUTABLE_OUTPUT_PATH}/${KIT}CxxTests)
endmacro(CreateTestDriver)

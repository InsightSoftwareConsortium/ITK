# This is a stub of the itk_python_add_test function for gersemi code formatting
#
# Usage:
#  itk_python_add_test(NAME testName
#    TEST_DRIVER_ARGS --compare testOutput.mha testBaseline.mha
#    COMMAND testPythonScript.py argv1 argv2 argv3
#    WORKING_DIRECTORY dir
#    )
function(itk_python_add_test)
  set(options)
  set(
    oneValueArgs
    NAME
    WORKING_DIRECTORY
  )
  set(
    multiValueArgs
    TEST_DRIVER_ARGS
    COMMAND
  )
  cmake_parse_arguments(
    PYTHON_ADD_TEST
    "${options}"
    "${oneValueArgs}"
    "${multiValueArgs}"
    ${ARGN}
  )

  # This is a stub function to assist code formatting
  # The real function is defined in ITKModuleTest.cmake
endfunction()

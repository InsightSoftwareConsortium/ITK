# This is a stub of the itk_python_expression_add_test function for gersemi code formatting
#
# Usage:
#  itk_python_expression_add_test(NAME testName
#    EXPRESSION "image = itk.Image.New()"
#    )
function(itk_python_expression_add_test)
  set(options)
  set(
    oneValueArgs
    NAME
    EXPRESSION
  )
  set(multiValueArgs)
  cmake_parse_arguments(
    PYTHON_EXPRESSION_ADD_TEST
    "${options}"
    "${oneValueArgs}"
    "${multiValueArgs}"
    ${ARGN}
  )

  # This is a stub function to assist code formatting
  # The real function is defined in ITKModuleTest.cmake
endfunction()

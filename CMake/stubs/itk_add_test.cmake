# This is a stub of the itk_add_test function for gersemi code formatting
#
# The function signature matches CMake's add_test with the same argument handling:
# add_test(NAME <name> COMMAND <command> [<arg>...]
#         [CONFIGURATIONS <config>...]
#         [WORKING_DIRECTORY <dir>]
#         [COMMAND_EXPAND_LISTS])
function(itk_add_test)
  cmake_parse_arguments(
    PARSE_ARGV
    1
    ARG
    "COMMAND_EXPAND_LISTS"
    "NAME;WORKING_DIRECTORY"
    "COMMAND;CONFIGURATIONS"
  )

  # This is a stub function to assist code formatting
  # The real function is defined in ITKModuleTest.cmake
endfunction()

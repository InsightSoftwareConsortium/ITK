# This is a stub of the itk_module macro for gersemi code formatting
#
# Usage:
#   itk_module(<name>
#     [DEPENDS module1 [module2...]]
#     [PRIVATE_DEPENDS module1 [module2...]]
#     [COMPILE_DEPENDS module1 [module2...]]
#     [TEST_DEPENDS module1 [module2...]]
#     [DESCRIPTION "text"]
#     [FACTORY_NAMES <factory>::<format> [...]]
#     [EXCLUDE_FROM_DEFAULT]
#     [ENABLE_SHARED]
#   )
#
# Description:
# Main function for declaring an ITK module, usually in an itk-module.cmake file
# in the module search path. The module name is the only required argument.
#
# Dependencies:
#  DEPENDS            = Modules that will be publicly linked to this module
#  PRIVATE_DEPENDS    = Modules that will be privately linked to this module
#  COMPILE_DEPENDS    = Modules needed at compile time by this module
#  TEST_DEPENDS       = Modules needed by this modules testing executables
#
# Other Arguments:
#  DESCRIPTION        = Free text description of the module
#  FACTORY_NAMES      = List of <factories>::<formats> to register
#  EXCLUDE_FROM_DEFAULT = Exclude this module from the build default modules flag
#  ENABLE_SHARED      = Build as shared library if build shared libraries flag is set
macro(itk_module _name)
  # Define the supported set of keywords
  set(
    options
    EXCLUDE_FROM_DEFAULT
    ENABLE_SHARED
  )
  set(oneValueArgs DESCRIPTION)
  set(
    multiValueArgs
    DEPENDS
    PRIVATE_DEPENDS
    COMPILE_DEPENDS
    TEST_DEPENDS
    FACTORY_NAMES
  )

  # Parse the arguments
  cmake_parse_arguments(
    MODULE
    "${options}"
    "${oneValueArgs}"
    "${multiValueArgs}"
    ${ARGN}
  )

  # This is a stub macro to assist code formatting
  # The real macro is defined in ITKModuleMacros.cmake
endmacro()

# This is a stub of the itk_fetch_module function for gersemi code formatting
#
# Usage:
#   itk_fetch_module(<name> <description>
#     [MODULE_COMPLIANCE_LEVEL <level>]  # level must be 0-5
#     [GIT_REPOSITORY <url>]             # URL of git repo
#     [GIT_TAG <tag>]                    # Git branch name, commit id or tag
#   )
#
# Description:
# Download and turn on a remote module.
# The module CMake option is created: Module_${module_name}, which defaults to OFF.
# Once set to ON, the module is downloaded into the Remote module group.
#
# Compliance Levels:
#   5: ITK main modules, or remote modules that could become core modules
#   4: Very high-quality code, perhaps small community dependance
#   3: Quality beta code
#   2: Alpha code feature API development or niche community/execution environment dependance
#   1: Pre-alpha features under development and code of unknown quality
#   0: Code/Feature of known poor-quality or deprecated status
function(itk_fetch_module _name _description)
  include(CMakeParseArguments)
  cmake_parse_arguments(
    _fetch_options
    ""
    "MODULE_COMPLIANCE_LEVEL;GIT_REPOSITORY;GIT_TAG"
    ""
    ${ARGN}
  )

  # A module name and description are required
  if(NOT _name OR NOT _description)
    message(FATAL_ERROR "Module name and description are required.")
  endif()

  # This is a stub function to assist code formatting
  # The real function is defined in ITKModuleRemote.cmake
endfunction()

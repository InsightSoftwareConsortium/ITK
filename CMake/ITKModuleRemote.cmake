# Function to fetch remote modules.

# Helper to perform the initial git clone and checkout.
function(_git_clone git_executable git_repository git_tag module_dir)
  execute_process(
    COMMAND "${git_executable}" clone "${git_repository}" "${module_dir}"
    RESULT_VARIABLE error_code
    OUTPUT_QUIET
    ERROR_QUIET
    )
  if(error_code)
    message(FATAL_ERROR "Failed to clone repository: '${git_repository}'")
  endif()

  execute_process(
    COMMAND "${git_executable}" checkout ${git_tag}
    WORKING_DIRECTORY "${module_dir}"
    RESULT_VARIABLE error_code
    OUTPUT_QUIET
    ERROR_QUIET
    )
  if(error_code)
    message(FATAL_ERROR "Failed to checkout tag: '${git_tag}'")
  endif()

  execute_process(
    COMMAND "${git_executable}" submodule init
    WORKING_DIRECTORY "${module_dir}"
    RESULT_VARIABLE error_code
    )
  if(error_code)
    message(FATAL_ERROR "Failed to init submodules in: '${module_dir}'")
  endif()

  execute_process(
    COMMAND "${git_executable}" submodule update --recursive
    WORKING_DIRECTORY "${module_dir}"
    RESULT_VARIABLE error_code
    )
  if(error_code)
    message(FATAL_ERROR "Failed to update submodules in: '${module_dir}'")
  endif()
endfunction()

# Helper to perform a git update.  Checks the current Git revision against the
# desired revision and only performs a fetch and checkout if needed.
function(_git_update git_executable git_repository git_tag module_dir)
  # Verify that remote url are the same
  execute_process(
    COMMAND "${git_executable}" config --get remote.origin.url
    WORKING_DIRECTORY "${module_dir}"
    RESULT_VARIABLE error_code
    OUTPUT_VARIABLE remote_origin_url
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  if ( NOT "${remote_origin_url}" STREQUAL "${git_repository}")
    message(WARNING "Remote URL changed from ${git_repository} to ${remote_origin_url}")
    execute_process(
      COMMAND "${git_executable}" remote rename origin old_origin
      WORKING_DIRECTORY "${module_dir}"
      RESULT_VARIABLE error_code
      OUTPUT_VARIABLE ignored
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    execute_process(
      COMMAND "${git_executable}" remote add origin "${git_repository}"
      WORKING_DIRECTORY "${module_dir}"
      RESULT_VARIABLE error_code
      OUTPUT_VARIABLE ignored
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  endif()
  execute_process(
    COMMAND "${git_executable}" rev-parse --verify "${git_tag}"
    WORKING_DIRECTORY "${module_dir}"
    RESULT_VARIABLE error_code
    OUTPUT_VARIABLE tag_hash
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  if(error_code)
    message(FATAL_ERROR "Failed to get the hash for tag '${module_dir}'")
  endif()
  execute_process(
    COMMAND "${git_executable}" rev-parse --verify HEAD
    WORKING_DIRECTORY "${module_dir}"
    RESULT_VARIABLE error_code
    OUTPUT_VARIABLE head_hash
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  if(error_code)
    message(FATAL_ERROR "Failed to get the hash for ${git_repository} HEAD")
  endif()

  # Is the hash checkout out that we want?
  if(NOT ("${tag_hash}" STREQUAL "${head_hash}"))
    execute_process(
      COMMAND "${git_executable}" fetch "${git_repository}"
      WORKING_DIRECTORY "${module_dir}"
      RESULT_VARIABLE error_code
      )
    if(error_code)
      message(FATAL_ERROR "Failed to fetch repository '${git_repository}'")
    endif()

    execute_process(
      COMMAND "${git_executable}" checkout ${git_tag}
      WORKING_DIRECTORY "${module_dir}"
      RESULT_VARIABLE error_code
      )
    if(error_code)
      message(FATAL_ERROR "Failed to checkout tag: '${git_tag}'")
    endif()

    execute_process(
      COMMAND "${git_executable}" submodule update --recursive
      WORKING_DIRECTORY "${module_dir}"
      RESULT_VARIABLE error_code
      )
    if(error_code)
      message(FATAL_ERROR "Failed to update submodules in: '${module_dir}'")
    endif()
  endif()
endfunction()

# Helper function to fetch a module stored in a Git repository.
# Git fetches are only performed when required.
function(_fetch_with_git git_executable git_repository git_tag module_dir)
  if("${git_tag}" STREQUAL "" OR "${git_repository}" STREQUAL "")
    message(FATAL_ERROR "Tag or repository for git checkout should not be empty.")
  endif()

  # If we don't have a clone yet.
  if(NOT EXISTS "${module_dir}")
    _git_clone("${git_executable}" "${git_repository}" "${git_tag}" "${module_dir}")
    message(STATUS " The remote module: ${git_repository} is cloned into the directory ${module_dir}")
  else() # We already have a clone, but we need to check that it has the right revision.
    _git_update("${git_executable}" "${git_repository}" "${git_tag}" "${module_dir}")
  endif()
endfunction()

# Download and turn on a remote module.
#
# The module CMake option is created: Module_${module_name}, which defaults to OFF.
# Once set to ON, the module is downloaded into the Remote module group.
#
# A module name and description are required.  The description will show up in
# the CMake user interface.
#
# The following options are currently supported:
#    [MODULE_COMPLIANCE_LEVEL [5|4|3|2|1|0] ] # The compliance level of the module, used for filtering
#            Compliance level 5 star (AKA ITK main modules, or remote modules that could become core modules)
#            Compliance Level 4 star (Very high-quality code, perhaps small community dependance)
#            Compliance Level 3 star (Quality beta code)
#            Compliance Level 2 star (Alpha code feature API development or niche community/exectution environment dependance )
#            Compliance Level 1 star (Pre-alpha features under development and code of unkown quality)
#            Compliance Level 0 star ( Code/Feature of known poor-quality or deprecated status )
#    [GIT_REPOSITORY url]            # URL of git repo
#    [GIT_TAG tag]                   # Git branch name, commit id or tag
#
# An CMake variable Module_${name}_GIT_TAG can be set
# in to override the value in the remote module configuration file.
# The intent of the Module_${name}_GIT_TAG variable override is to
# facilitate testing of remote module branch behaviors without
# requiring changes to the ITK code base. If Module_${name}_GIT_TAG is
# "" then no git fetch or update will be performed.
function(itk_fetch_module _name _description)
  include(CMakeParseArguments)
  cmake_parse_arguments(_fetch_options "" "MODULE_COMPLIANCE_LEVEL;GIT_REPOSITORY;GIT_TAG" "" ${ARGN})
  set(MODULE_COMPLIANCE_LEVEL "${_fetch_options_MODULE_COMPLIANCE_LEVEL}")
  if(NOT MODULE_COMPLIANCE_LEVEL)
    set(DEFAULT_MODULE_COMPLIANCE_LEVEL 1)
    message(STATUS "Implicitly setting unspecified compliance level for module Module_${_name} to ${DEFAULT_MODULE_COMPLIANCE_LEVEL}")
    set(MODULE_COMPLIANCE_LEVEL ${DEFAULT_MODULE_COMPLIANCE_LEVEL})
  endif()

  set(Module_${_name}_REMOTE_COMPLIANCE_LEVEL ${MODULE_COMPLIANCE_LEVEL} CACHE INTERNAL "Variable to indicate the Module_${_name} compliance level")

  if(NOT DEFINED Module_${_name} )
    option(Module_${_name} "(Remote-${MODULE_COMPLIANCE_LEVEL}) ${_description}" OFF)
  else()
    # If Module_${_name} is set manually, put it's value in the CACHE
    option(Module_${_name} "(Remote-${MODULE_COMPLIANCE_LEVEL}) ${_description}" ${Module_${_name}})
  endif()

  if(${MODULE_COMPLIANCE_LEVEL} GREATER_EQUAL ${ITK_MINIMUM_COMPLIANCE_LEVEL})
    set(Module_${_name}_VALID ON)
    mark_as_advanced(CLEAR Module_${_name})
  else()
    set(Module_${_name}_VALID OFF)
    mark_as_advanced(FORCE Module_${_name})
  endif()
  # message(INFO " MODULE_VALID Module_${_name}:${Module_${_name}_VALID}:${MODULE_COMPLIANCE_LEVEL}>=${ITK_MINIMUM_COMPLIANCE_LEVEL}")

  # Fetch_$_remote_module} is deprecated. To maintain backward compatibility:
  if(Fetch_${_name})
    message(WARNING "Fetch_${_name} is deprecated, please use Module_${_name} to download and enable the remote module.")
    set(Module_${_name} ON CACHE FORCE "(Remote-${MODULE_COMPLIANCE_LEVEL}) ${_description}")
  endif()

  if(Module_${_name})
    itk_download_attempt_check(Module_${_name})
    find_package(Git)
    if(NOT GIT_EXECUTABLE)
      message(FATAL_ERROR "error: could not find git for clone of ${_name}")
    endif()
    execute_process(
      COMMAND "${GIT_EXECUTABLE}" --version
      OUTPUT_VARIABLE ov
      OUTPUT_STRIP_TRAILING_WHITESPACE
      )
    string(REGEX REPLACE "^git version (.+)$" "\\1" _version "${ov}")
    if("${_version}" VERSION_LESS 1.6.6)
      message(FATAL_ERROR "Git version 1.6.6 or later is required.")
    endif()

    set(REMOTE_GIT_TAG "${_fetch_options_GIT_TAG}")

    if( DEFINED Module_${_name}_GIT_TAG AND NOT "${Module_${_name}_GIT_TAG}" STREQUAL "${_fetch_options_GIT_TAG}")
      set(REMOTE_GIT_TAG "${Module_${_name}_GIT_TAG}")
      message(STATUS "NOTE: Using override 'Module_${_name}_GIT_TAG=${REMOTE_GIT_TAG}'\n"
                     "      instead of value 'GIT_TAG=${_fetch_options_GIT_TAG}'\n"
                     "      specified in file ${ITK_SOURCE_DIR}/Modules/Remote/${_name}.remote.cmake'")
    endif()
    set(Module_${_name}_GIT_TAG "${REMOTE_GIT_TAG}" CACHE STRING "Override default GIT_TAG value for remote module ${_name}")
    mark_as_advanced(Module_${_name}_GIT_TAG)
    # Show remote module options if building.
    set_property(CACHE Module_${_name}_GIT_TAG PROPERTY TYPE STRING)
    if(DEFINED Module_${_name}_BUILD_EXAMPLES)
      set_property(CACHE Module_${_name}_BUILD_EXAMPLES PROPERTY TYPE BOOL)
    endif()

    if (NOT REMOTE_GIT_TAG STREQUAL "")
      _fetch_with_git("${GIT_EXECUTABLE}"
        "${_fetch_options_GIT_REPOSITORY}"
        "${REMOTE_GIT_TAG}"
        "${ITK_SOURCE_DIR}/Modules/Remote/${_name}"
        )
    endif()
  else()
    # Hide remote module options if not building.
    if(DEFINED Module_${_name}_GIT_TAG)
      set_property(CACHE Module_${_name}_GIT_TAG PROPERTY TYPE INTERNAL)
    endif()
    if(DEFINED Module_${_name}_BUILD_EXAMPLES)
      set_property(CACHE Module_${_name}_BUILD_EXAMPLES PROPERTY TYPE INTERNAL)
    endif()
  endif()
endfunction()

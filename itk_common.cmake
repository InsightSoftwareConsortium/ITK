# ITK Common Dashboard Script
#
# This script contains basic dashboard driver code common to all
# clients.
#
# Put this script in a directory such as "~/Dashboards/Scripts" or
# "c:/Dashboards/Scripts".  Create a file next to this script, say
# 'my_dashboard.cmake', with code of the following form:
#
#   # Client maintainer: me@mydomain.net
#   set(CTEST_SITE "machine.site")
#   set(CTEST_BUILD_NAME "Platform-Compiler")
#   set(CTEST_BUILD_CONFIGURATION Debug)
#   set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
#   include(${CTEST_SCRIPT_DIRECTORY}/itk_common.cmake)
#
# Then run a scheduled task (cron job) with a command line such as
#
#   ctest -S ~/Dashboards/Scripts/my_dashboard.cmake -V
#
# By default the source and build trees will be placed in the path
# "../MyTests/" relative to your script location.
#
# The following variables may be set before including this script
# to configure it:
#
#   dashboard_model           = Nightly | Experimental | Continuous
#   dashboard_track           = Optional track to submit dashboard to
#   dashboard_loop            = Repeat until N seconds have elapsed
#   dashboard_root_name       = Change name of "MyTests" directory
#   dashboard_source_name     = Name of source directory (ITK)
#   dashboard_binary_name     = Name of binary directory (ITK-build)
#   dashboard_data_name       = Name of ExternalData store (ExternalData)
#   dashboard_cache           = Initial CMakeCache.txt file content
#   dashboard_do_cache        = Always write CMakeCache.txt
#   dashboard_do_coverage     = True to enable coverage (ex: gcov)
#   dashboard_do_memcheck     = True to enable memcheck (ex: valgrind)
#   dashboard_no_clean        = True to skip build tree wipeout
#   dashboard_no_update       = True to skip source tree update
#   CTEST_UPDATE_COMMAND      = path to git command-line client
#   CTEST_BUILD_FLAGS         = build tool arguments (ex: -j2)
#   CTEST_BUILD_TARGET        = A specific target to be built (instead of all)
#   CTEST_DASHBOARD_ROOT      = Where to put source and build trees
#   CTEST_TEST_CTEST          = Whether to run long CTestTest* tests
#   CTEST_TEST_TIMEOUT        = Per-test timeout length
#   CTEST_COVERAGE_ARGS       = ctest_coverage command args
#   CTEST_TEST_ARGS           = ctest_test args (ex: PARALLEL_LEVEL 4)
#   CTEST_MEMCHECK_ARGS       = ctest_memcheck args (defaults to CTEST_TEST_ARGS)
#   CMAKE_MAKE_PROGRAM        = Path to "make" tool to use
#
# Options to configure builds from experimental git repository:
#   dashboard_git_url      = Custom git clone url
#   dashboard_git_branch   = Custom remote branch to track
#   dashboard_git_crlf     = Value of core.autocrlf for repository
#
# The following macros will be invoked before the corresponding
# step if they are defined:
#
#   dashboard_hook_init       = End of initialization, before loop
#   dashboard_hook_start      = Start of loop body, before ctest_start
#   dashboard_hook_started    = After ctest_start
#   dashboard_hook_build      = Before ctest_build
#   dashboard_hook_test       = Before ctest_test
#   dashboard_hook_coverage   = Before ctest_coverage
#   dashboard_hook_memcheck   = Before ctest_memcheck
#   dashboard_hook_submit     = Before ctest_submit
#   dashboard_hook_end        = End of loop body, after ctest_submit
#
# For Makefile generators the script may be executed from an
# environment already configured to use the desired compilers.
# Alternatively the environment may be set at the top of the script:
#
#   set(ENV{CC}  /path/to/cc)   # C compiler
#   set(ENV{CXX} /path/to/cxx)  # C++ compiler
#   set(ENV{FC}  /path/to/fc)   # Fortran compiler (optional)
#   set(ENV{LD_LIBRARY_PATH} /path/to/vendor/lib) # (if necessary)

#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

cmake_minimum_required(VERSION 3.22.1 FATAL_ERROR)

set(dashboard_user_home "$ENV{HOME}")

get_filename_component(dashboard_self_dir ${CMAKE_CURRENT_LIST_FILE} PATH)

# Select the top dashboard directory.
if(NOT DEFINED dashboard_root_name)
  set(dashboard_root_name "MyTests")
endif()
if(NOT DEFINED CTEST_DASHBOARD_ROOT)
  get_filename_component(CTEST_DASHBOARD_ROOT "${CTEST_SCRIPT_DIRECTORY}/../${dashboard_root_name}" ABSOLUTE)
endif()

# Select the model (Nightly, Experimental, Continuous).
if(NOT DEFINED dashboard_model)
  set(dashboard_model Nightly)
endif()
if(NOT "${dashboard_model}" MATCHES "^(Nightly|Experimental|Continuous)$")
  message(FATAL_ERROR "dashboard_model must be Nightly, Experimental, or Continuous")
endif()

# Default to a Debug build.
if(NOT DEFINED CTEST_CONFIGURATION_TYPE AND DEFINED CTEST_BUILD_CONFIGURATION)
  set(CTEST_CONFIGURATION_TYPE ${CTEST_BUILD_CONFIGURATION})
endif()

if(NOT DEFINED CTEST_CONFIGURATION_TYPE)
  set(CTEST_CONFIGURATION_TYPE Debug)
endif()

# Choose CTest reporting mode.
if(NOT DEFINED CTEST_USE_LAUNCHERS)
  if(NOT "${CTEST_CMAKE_GENERATOR}" MATCHES "Make")
    # Launchers work only with Makefile generators.
    set(CTEST_USE_LAUNCHERS 0)
  elseif(NOT DEFINED CTEST_USE_LAUNCHERS)
    # The setting is ignored by CTest < 2.8 so we need no version test.
    set(CTEST_USE_LAUNCHERS 1)
  endif()
endif()

# Configure testing.
if(NOT DEFINED CTEST_TEST_CTEST)
  set(CTEST_TEST_CTEST 1)
endif()
if(NOT CTEST_TEST_TIMEOUT)
  set(CTEST_TEST_TIMEOUT 1500)
endif()


# Select Git source to use.
if(NOT DEFINED dashboard_git_url)
  set(dashboard_git_url "https://github.com/InsightSoftwareConsortium/ITK.git")
endif()
if(NOT DEFINED dashboard_git_branch)
   if("${dashboard_model}" STREQUAL "Nightly")
     set(dashboard_git_branch follow/main/nightly)
   else()
     set(dashboard_git_branch main)
   endif()
 else()
   # map values from outdated client scripts
   if(dashboard_git_branch STREQUAL "nightly-master")
     set(dashboard_git_branch follow/master/nightly)
   endif()
endif()
if(NOT DEFINED dashboard_git_crlf)
  if(UNIX)
    set(dashboard_git_crlf false)
  else(UNIX)
    set(dashboard_git_crlf true)
  endif(UNIX)
endif()

# Look for a GIT command-line client.
if(NOT DEFINED CTEST_GIT_COMMAND)
  find_program(CTEST_GIT_COMMAND NAMES git git.cmd)
endif()

if(NOT DEFINED CTEST_GIT_COMMAND)
  message(FATAL_ERROR "No Git Found.")
endif()

# Explicitly specify the remote as "origin". This ensure we are pulling from
# the correct remote and prevents command failures when the git tracking
# branch has not been configured.
set(CTEST_GIT_UPDATE_CUSTOM "${CTEST_GIT_COMMAND}" pull origin ${dashboard_git_branch})

# Select a source directory name.
if(NOT DEFINED CTEST_SOURCE_DIRECTORY)
  if(DEFINED dashboard_source_name)
    set(CTEST_SOURCE_DIRECTORY ${CTEST_DASHBOARD_ROOT}/${dashboard_source_name})
  else()
    set(CTEST_SOURCE_DIRECTORY ${CTEST_DASHBOARD_ROOT}/ITK)
  endif()
endif()

# Select a build directory name.
if(NOT DEFINED CTEST_BINARY_DIRECTORY)
  if(DEFINED dashboard_binary_name)
    set(CTEST_BINARY_DIRECTORY ${CTEST_DASHBOARD_ROOT}/${dashboard_binary_name})
  else()
    set(CTEST_BINARY_DIRECTORY ${CTEST_SOURCE_DIRECTORY}-build)
  endif()
endif()

# Select a data store.
if(NOT DEFINED ExternalData_OBJECT_STORES)
  if(DEFINED "ENV{ExternalData_OBJECT_STORES}")
    file(TO_CMAKE_PATH "$ENV{ExternalData_OBJECT_STORES}" ExternalData_OBJECT_STORES)
  else()
    if(DEFINED dashboard_data_name)
        set(ExternalData_OBJECT_STORES ${CTEST_DASHBOARD_ROOT}/${dashboard_data_name})
    else()
        set(ExternalData_OBJECT_STORES ${CTEST_DASHBOARD_ROOT}/ExternalData)
    endif()
  endif()
endif()

if(NOT DEFINED CTEST_MEMCHECK_ARGS)
  set(CTEST_MEMCHECK_ARGS ${CTEST_TEST_ARGS})
endif()

# Delete source tree if it is incompatible with current VCS.
if(EXISTS ${CTEST_SOURCE_DIRECTORY})
  if(NOT EXISTS "${CTEST_SOURCE_DIRECTORY}/.git")
    set(vcs_refresh "because it is not managed by git.")
  endif()
  if(vcs_refresh AND "${CTEST_SOURCE_DIRECTORY}" MATCHES "/(ITK|Insight)[^/]*")
    message("Deleting source tree\n  ${CTEST_SOURCE_DIRECTORY}\n${vcs_refresh}")
    file(REMOVE_RECURSE "${CTEST_SOURCE_DIRECTORY}")
  endif()
endif()

# Upstream non-head refs to treat like branches.
# These are updated by kwrobot.
set(dashboard_git_extra_branches
  follow/main/nightly          # updated nightly to main
  )

# Support initial checkout if necessary.
if(NOT EXISTS "${CTEST_SOURCE_DIRECTORY}"
    AND NOT DEFINED CTEST_CHECKOUT_COMMAND)
  get_filename_component(_name "${CTEST_SOURCE_DIRECTORY}" NAME)
    # Generate an initial checkout script.
    set(ctest_checkout_script ${CTEST_DASHBOARD_ROOT}/${_name}-init.cmake)
    file(WRITE ${ctest_checkout_script} "# git repo init script for ${_name}
execute_process(
  COMMAND \"${CTEST_GIT_COMMAND}\" clone -n -- \"${dashboard_git_url}\"
          \"${CTEST_SOURCE_DIRECTORY}\"
  )
if(EXISTS \"${CTEST_SOURCE_DIRECTORY}/.git\")
  execute_process(
    COMMAND \"${CTEST_GIT_COMMAND}\" config core.autocrlf ${dashboard_git_crlf}
    WORKING_DIRECTORY \"${CTEST_SOURCE_DIRECTORY}\"
    )
  foreach(b ${dashboard_git_extra_branches})
     execute_process(
       COMMAND \"${CTEST_GIT_COMMAND}\" config --add remote.origin.fetch +refs/\${b}:refs/remotes/origin/\${b}
       WORKING_DIRECTORY \"${CTEST_SOURCE_DIRECTORY}\"
       )
  endforeach()
  execute_process(
    COMMAND \"${CTEST_GIT_COMMAND}\" fetch
    WORKING_DIRECTORY \"${CTEST_SOURCE_DIRECTORY}\"
    )
  foreach(b ${dashboard_git_extra_branches})
    execute_process(
       COMMAND \"${CTEST_GIT_COMMAND}\" branch \${b} origin/\${b}
       WORKING_DIRECTORY \"${CTEST_SOURCE_DIRECTORY}\"
       )
     execute_process(
       COMMAND \"${CTEST_GIT_COMMAND}\" config branch.\${b}.remote origin
       WORKING_DIRECTORY \"${CTEST_SOURCE_DIRECTORY}\"
     )
     execute_process(
       COMMAND \"${CTEST_GIT_COMMAND}\" config branch.\${b}.merge refs/\${b}
       WORKING_DIRECTORY \"${CTEST_SOURCE_DIRECTORY}\"
       )
  endforeach()
  execute_process(
     COMMAND \"${CTEST_GIT_COMMAND}\" checkout ${dashboard_git_branch}
     WORKING_DIRECTORY \"${CTEST_SOURCE_DIRECTORY}\"
     )
endif()
")
  set(CTEST_CHECKOUT_COMMAND "\"${CMAKE_COMMAND}\" -P \"${ctest_checkout_script}\"")
  # CTest delayed initialization is broken, so we put the
  # CTestConfig.cmake info here.
  set(CTEST_NIGHTLY_START_TIME "01:00:00 UTC")
  set(CTEST_DROP_METHOD "http")
  set(CTEST_DROP_SITE "open.cdash.org")
  set(CTEST_DROP_LOCATION "/submit.php?project=Insight")
  set(CTEST_DROP_SITE_CDASH TRUE)
endif()

#-----------------------------------------------------------------------------

# Send the main script as a note.
list(APPEND CTEST_NOTES_FILES
  "${CTEST_SCRIPT_DIRECTORY}/${CTEST_SCRIPT_NAME}"
  "${CMAKE_CURRENT_LIST_FILE}"
  )

# Check for required variables.
foreach(req
    CTEST_CMAKE_GENERATOR
    CTEST_SITE
    CTEST_BUILD_NAME
    )
  if(NOT DEFINED ${req})
    message(FATAL_ERROR "The containing script must set ${req}")
  endif()
endforeach(req)

# Print summary information.
foreach(v
    CTEST_SITE
    CTEST_BUILD_NAME
    CTEST_SOURCE_DIRECTORY
    CTEST_BINARY_DIRECTORY
    ExternalData_OBJECT_STORES
    CTEST_CMAKE_GENERATOR
    CTEST_BUILD_CONFIGURATION
    CTEST_BUILD_FLAGS
    CTEST_GIT_COMMAND
    CTEST_CHECKOUT_COMMAND
    CTEST_SCRIPT_DIRECTORY
    CTEST_USE_LAUNCHERS
    CTEST_TEST_TIMEOUT
    CTEST_COVERAGE_ARGS
    CTEST_TEST_ARGS
    CTEST_MEMCHECK_ARGS
    )
  set(vars "${vars}  ${v}=[${${v}}]\n")
endforeach(v)
message("Dashboard script configuration:\n${vars}\n")

# Avoid non-ascii characters in tool output.
set(ENV{LC_ALL} C)

# Helper macro to write the initial cache.
macro(write_cache)
  set(cache_build_type "")
  set(cache_make_program "")
  if(CTEST_CMAKE_GENERATOR MATCHES "Make")
    set(cache_build_type CMAKE_BUILD_TYPE:STRING=${CTEST_BUILD_CONFIGURATION})
    if(CMAKE_MAKE_PROGRAM)
      set(cache_make_program CMAKE_MAKE_PROGRAM:FILEPATH=${CMAKE_MAKE_PROGRAM})
    endif()
  endif()
  file(WRITE ${CTEST_BINARY_DIRECTORY}/CMakeCache.txt "
SITE:STRING=${CTEST_SITE}
BUILDNAME:STRING=${CTEST_BUILD_NAME}
CTEST_USE_LAUNCHERS:BOOL=${CTEST_USE_LAUNCHERS}
DART_TESTING_TIMEOUT:STRING=${CTEST_TEST_TIMEOUT}
ExternalData_OBJECT_STORES:STRING=${ExternalData_OBJECT_STORES}
MAXIMUM_NUMBER_OF_HEADERS:STRING=35
BUILD_EXAMPLES:BOOL=ON
ITK_USE_EIGEN_MPL2_ONLY:BOOL=ON
${cache_build_type}
${cache_make_program}
${dashboard_cache}
")
  file(REMOVE_RECURSE "${CTEST_BINARY_DIRECTORY}/CMakeFiles")
endmacro()

# Start with a fresh build tree.
if(NOT EXISTS "${CTEST_BINARY_DIRECTORY}")
  file(MAKE_DIRECTORY "${CTEST_BINARY_DIRECTORY}")
elseif(NOT "${CTEST_SOURCE_DIRECTORY}" STREQUAL "${CTEST_BINARY_DIRECTORY}"
    AND NOT dashboard_no_clean)
  message("Clearing build tree...")
  ctest_empty_binary_directory(${CTEST_BINARY_DIRECTORY})
endif()

set(dashboard_continuous 0)
if("${dashboard_model}" STREQUAL "Continuous")
  set(dashboard_continuous 1)
endif()
if(NOT DEFINED dashboard_loop)
  if(dashboard_continuous)
    set(dashboard_loop 43200)
  else()
    set(dashboard_loop 0)
  endif()
endif()

# CI log section helpers — emit collapsible group markers for
# GitHub Actions, Azure DevOps, and GitLab CI.
# GitLab CI requires unique section IDs within a job log, so callers
# in the loop below include _dashboard_iteration in the ID.
string(ASCII 27 _CI_ESC)

function(ci_section_start section_id title)
  if(DEFINED ENV{GITLAB_CI})
    string(TIMESTAMP _epoch "%s" UTC)
    message("${_CI_ESC}[0Ksection_start:${_epoch}:${section_id}[collapsed=true]\r${_CI_ESC}[0K${title}")
  elseif(DEFINED ENV{GITHUB_ACTIONS})
    message("::group::${title}")
  elseif(DEFINED ENV{TF_BUILD})
    message("##[group]${title}")
  else()
    message("--- ${title} ---")
  endif()
endfunction()

function(ci_section_end section_id)
  if(DEFINED ENV{GITLAB_CI})
    string(TIMESTAMP _epoch "%s" UTC)
    message("${_CI_ESC}[0Ksection_end:${_epoch}:${section_id}\r${_CI_ESC}[0K")
  elseif(DEFINED ENV{GITHUB_ACTIONS})
    message("::endgroup::")
  elseif(DEFINED ENV{TF_BUILD})
    message("##[endgroup]")
  else()
    message("--- end ${section_id} ---")
  endif()
endfunction()

# Extract and print build warnings/errors from Build.xml so they
# appear directly in the CI log.  ctest_build() only exposes counts
# (NUMBER_WARNINGS / NUMBER_ERRORS) — the actual compiler messages
# live inside the XML that CTest writes for CDash submission.
function(ci_report_build_diagnostics binary_dir num_warnings num_errors)
  if(num_warnings EQUAL 0 AND num_errors EQUAL 0)
    return()
  endif()

  # Locate the Build.xml via the TAG file that CTest maintains.
  set(_tag_file "${binary_dir}/Testing/TAG")
  if(NOT EXISTS "${_tag_file}")
    return()
  endif()
  file(STRINGS "${_tag_file}" _tag_lines)
  list(GET _tag_lines 0 _tag_dir)
  set(_build_xml "${binary_dir}/Testing/${_tag_dir}/Build.xml")
  if(NOT EXISTS "${_build_xml}")
    return()
  endif()

  # Read Build.xml — escape semicolons so CMake list operations
  # don't mangle lines that happen to contain them.
  file(READ "${_build_xml}" _xml)
  string(REPLACE ";" "\\;" _xml "${_xml}")
  string(REPLACE "\n" ";" _xml_lines "${_xml}")

  # Walk the XML line-by-line, tracking whether we are inside a
  # <Warning> or <Error> block, and collect the <Text> content.
  set(_in_warning FALSE)
  set(_in_error FALSE)
  set(_warning_texts "")
  set(_error_texts "")
  foreach(_line IN LISTS _xml_lines)
    if("${_line}" MATCHES "<Warning>")
      set(_in_warning TRUE)
    elseif("${_line}" MATCHES "</Warning>")
      set(_in_warning FALSE)
    elseif("${_line}" MATCHES "<Error>")
      set(_in_error TRUE)
    elseif("${_line}" MATCHES "</Error>")
      set(_in_error FALSE)
    endif()
    if("${_line}" MATCHES "<Text>(.*)</Text>")
      if(_in_warning)
        list(APPEND _warning_texts "${CMAKE_MATCH_1}")
      elseif(_in_error)
        list(APPEND _error_texts "${CMAKE_MATCH_1}")
      endif()
    endif()
  endforeach()

  # Print collected diagnostics so they are visible in CI output.
  if(num_errors GREATER 0)
    message("========== BUILD ERRORS (${num_errors}) ==========")
    foreach(_t IN LISTS _error_texts)
      message("  ${_t}")
    endforeach()
  endif()
  if(num_warnings GREATER 0)
    message("========== BUILD WARNINGS (${num_warnings}) ==========")
    foreach(_t IN LISTS _warning_texts)
      message("  ${_t}")
    endforeach()
  endif()
  message("====================================================")
endfunction()

if(COMMAND dashboard_hook_init)
  dashboard_hook_init()
endif()

set(dashboard_done 0)
set(_dashboard_iteration 0)
while(NOT dashboard_done)
  math(EXPR _dashboard_iteration "${_dashboard_iteration} + 1")
  if(dashboard_loop)
    set(START_TIME ${CTEST_ELAPSED_TIME})
  endif()
  set(ENV{HOME} "${dashboard_user_home}")

  # Start a new submission.
  if(COMMAND dashboard_hook_start)
    dashboard_hook_start()
  endif()
  if(dashboard_track)
    ctest_start(${dashboard_model} TRACK ${dashboard_track})
  else()
    ctest_start(${dashboard_model})
  endif()
  if(COMMAND dashboard_hook_started)
    dashboard_hook_started()
  endif()

  # Always build if the tree is fresh.
  set(dashboard_fresh 0)
  if(NOT EXISTS "${CTEST_BINARY_DIRECTORY}/CMakeCache.txt"
     OR "${dashboard_do_cache}")
    set(dashboard_fresh 1)
    message("Writing initial dashboard cache...")
    write_cache()
  endif()

  # Look for updates.
  if(NOT dashboard_no_update)
    ctest_update(RETURN_VALUE count)
  endif()
  set(CTEST_CHECKOUT_COMMAND) # checkout on first iteration only
  message("Found ${count} changed files")

  if(dashboard_fresh OR NOT dashboard_continuous OR count GREATER 0)
    ci_section_start("configure_${_dashboard_iteration}" "Configure")
    ctest_configure(RETURN_VALUE configure_return)
    ctest_read_custom_files(${CTEST_BINARY_DIRECTORY})
    ci_section_end("configure_${_dashboard_iteration}")

    ci_section_start("build_${_dashboard_iteration}" "Build")
    if(COMMAND dashboard_hook_build)
      dashboard_hook_build()
    endif()
    ctest_build(RETURN_VALUE build_return
                NUMBER_ERRORS build_errors
                NUMBER_WARNINGS build_warnings)
    ci_section_end("build_${_dashboard_iteration}")

    # Intentionally placed OUTSIDE the collapsible build section so
    # that warnings and errors are always visible in the CI log
    # without having to expand the build section.
    ci_report_build_diagnostics(
      "${CTEST_BINARY_DIRECTORY}" "${build_warnings}" "${build_errors}")

    ci_section_start("test_${_dashboard_iteration}" "Test")
    if(COMMAND dashboard_hook_test)
      dashboard_hook_test()
    endif()
    ctest_test(${CTEST_TEST_ARGS} RETURN_VALUE test_return)
    ci_section_end("test_${_dashboard_iteration}")

    if(dashboard_do_coverage)
      ci_section_start("coverage_${_dashboard_iteration}" "Coverage")
      if(COMMAND dashboard_hook_coverage)
        dashboard_hook_coverage()
      endif()
      ctest_coverage(${CTEST_COVERAGE_ARGS})
      ci_section_end("coverage_${_dashboard_iteration}")
    endif()
    if(dashboard_do_memcheck)
      ci_section_start("memcheck_${_dashboard_iteration}" "MemCheck")
      if(COMMAND dashboard_hook_memcheck)
        dashboard_hook_memcheck()
      endif()
      ctest_memcheck(${CTEST_MEMCHECK_ARGS})
      ci_section_end("memcheck_${_dashboard_iteration}")
    endif()
    ci_section_start("submit_${_dashboard_iteration}" "Submit to CDash")
    if(COMMAND dashboard_hook_submit)
      dashboard_hook_submit()
    endif()
    if(NOT dashboard_no_submit)
      ctest_submit()
    endif()
    ci_section_end("submit_${_dashboard_iteration}")
    if(COMMAND dashboard_hook_end)
      dashboard_hook_end()
    endif()
  endif()

  if(dashboard_loop)
    # Delay until at least 5 minutes past START_TIME
    ctest_sleep(${START_TIME} 300 ${CTEST_ELAPSED_TIME})
    if(${CTEST_ELAPSED_TIME} GREATER ${dashboard_loop})
      set(dashboard_done 1)
    endif()
  else()
    # Not continuous, so we are done.
    set(dashboard_done 1)
  endif()
endwhile()

set(ci_completed_successfully 0)
if(NOT ${configure_return} EQUAL 0)
  message(WARNING
	  "configure_return did not complete without warnings, errors, or failures.")
  set(ci_completed_successfully 1)
endif()

if(NOT ${build_return} EQUAL 0)
  message(WARNING
    "build_return did not complete without warnings, errors, or failures.")
  set(ci_completed_successfully 1)
endif()

if(NOT ${build_errors} EQUAL 0)
  message(WARNING
    "build_errors did not complete without warnings, errors, or failures.")
  set(ci_completed_successfully 1)
endif()

if(NOT ${build_warnings} EQUAL 0)
  message(WARNING
    "build_warnings did not complete without warnings, errors, or failures.")
  set(ci_completed_successfully 1)
endif()

if(NOT ${test_return} EQUAL 0)
  message(WARNING
    "test_return did not complete without warnings, errors, or failures.")
  set(ci_completed_successfully 1)
endif()

if(NOT ${ci_completed_successfully} EQUAL 0)
  message(FATAL_ERROR
    "ci_completed_successfully did not complete without warnings, errors, or failures.")
endif()

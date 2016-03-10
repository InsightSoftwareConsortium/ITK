set(CTEST_BUILD_NAME "${platform}-${compiler}-$ENV{GERRIT_TOPIC}-$ENV{GERRIT_CHANGE_NUMBER}-$ENV{GERRIT_PATCHSET_NUMBER}")
set(dashboard_git_branch "$ENV{GERRIT_TOPIC}-$ENV{GERRIT_CHANGE_NUMBER}-$ENV{GERRIT_PATCHSET_NUMBER}")
set(dashboard_no_update 1)
set(dashboard_model "Experimental")
set(dashboard_track "Gerrit")
set(dashboard_no_clean 1)

find_package(Git)
if(GIT_EXECUTABLE)
  execute_process(COMMAND ${GIT_EXECUTABLE} diff-tree --no-commit-id --name-only --diff-filter=ACMRT -r HEAD
    WORKING_DIRECTORY "${CTEST_DASHBOARD_ROOT}/${dashboard_source_name}"
    RESULT_VARIABLE result
    OUTPUT_VARIABLE output)
  if(${result} EQUAL 0 AND "${output}" MATCHES "Modules/Remote")
    string(REGEX MATCHALL "Modules/Remote/.*[.]remote[.]cmake" remote_paths "${output}")
    string(REGEX REPLACE "\n" ";" remote_paths ${remote_paths})
    foreach(remote ${remote_paths})
      string(REGEX REPLACE "Modules/Remote/(.*)[.]remote[.]cmake" "Module_\\1:BOOL=ON" module_enable "${remote}")
      message(STATUS "Remote module change detected. Adding: ${module_enable}")
      set(dashboard_cache "${dashboard_cache}\n${module_enable}")
    endforeach()
  endif()
endif()

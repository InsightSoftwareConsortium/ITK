cmake_minimum_required(VERSION 3.22.1)
project(ExternalModuleSettingsCheck LANGUAGES NONE)

set(case_dir "${CMAKE_CURRENT_BINARY_DIR}/case")
set(python "${CMAKE_CURRENT_BINARY_DIR}/python")
file(WRITE "${python}" "")
set(failures "")

# Configure the case project, expecting PASS or FAIL and output matching regex.
function(run name expected regex)
  execute_process(
    COMMAND
      "${CMAKE_COMMAND}" -S "${CMAKE_CURRENT_SOURCE_DIR}/Case" -B "${case_dir}"
      ${ARGN}
    RESULT_VARIABLE result
    OUTPUT_VARIABLE output
    ERROR_VARIABLE output
  )
  if(result)
    set(outcome FAIL)
  else()
    set(outcome PASS)
  endif()
  if(NOT outcome STREQUAL expected OR NOT output MATCHES "${regex}")
    set(failures "${failures}\n${name}:\n${output}" PARENT_SCOPE)
  endif()
endfunction()

file(REMOVE_RECURSE "${case_dir}")
run(seed PASS "BUILD_SHARED_LIBS=ON" -DITK_BUILD_SHARED=ON)
run(equivalent_bool PASS "BUILD_SHARED_LIBS=ON" -DITK_BUILD_SHARED=1)
run(mismatch FAIL "-DBUILD_SHARED_LIBS=OFF [(]currently ON[)]"
  -DITK_BUILD_SHARED=OFF
)

file(REMOVE_RECURSE "${case_dir}")
run(normal_variable FAIL "-DBUILD_SHARED_LIBS=ON [(]currently OFF[)]"
  -DITK_BUILD_SHARED=ON -DCASE_NORMAL_SHARED=OFF
)

file(REMOVE_RECURSE "${case_dir}")
run(python_seed PASS ""
  -DITK_WRAP_PYTHON=ON "-DITK_WRAP_PYTHON_EXECUTABLE=${python}"
  -DITK_WRAP_PYTHON_LIMITED_API=ON
)
run(equivalent_path PASS ""
  -DITK_WRAP_PYTHON=ON
  "-DITK_WRAP_PYTHON_EXECUTABLE=${CMAKE_CURRENT_BINARY_DIR}/case/../python"
  -DITK_WRAP_PYTHON_LIMITED_API=ON
)
run(combined_mismatch FAIL
  "-DPython3_EXECUTABLE=[^\n]*other .*-DITK_USE_PYTHON_LIMITED_API=OFF"
  -DITK_WRAP_PYTHON=ON
  "-DITK_WRAP_PYTHON_EXECUTABLE=${CMAKE_CURRENT_BINARY_DIR}/other"
  -DITK_WRAP_PYTHON_LIMITED_API=OFF
)

if(failures)
  message(
    FATAL_ERROR
    "Unexpected external module settings behavior:${failures}"
  )
endif()

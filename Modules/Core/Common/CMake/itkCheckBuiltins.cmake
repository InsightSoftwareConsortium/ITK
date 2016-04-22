# check for gcc/clang atomic builtins like __sync_add_and_fetch
if(NOT WIN32 OR MINGW)
  if(EMSCRIPTEN)
    set(ITK_HAVE_SYNC_BUILTINS 0 CACHE INTERNAL "For __sync atomic builtins.")
  elseif(NOT DEFINED ITK_HAVE_SYNC_BUILTINS)
    message(STATUS "Checking for builtin __sync_add_and_fetch")
    try_compile(ITK_TEST_SYNC_BUILTINS_COMPILED
      ${ITK_BINARY_DIR}
      ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckSyncBuiltins.cxx
      OUTPUT_VARIABLE OUTPUT)
    if(ITK_TEST_SYNC_BUILTINS_COMPILED)
      message(STATUS "Checking for builtin __sync_add_and_fetch -- success")
      set(ITK_HAVE_SYNC_BUILTINS 1 CACHE INTERNAL "For __sync atomic builtins.")
      file(APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeOutput.log
        "Determining if the C++ compiler supports __sync_add_and_fetch builtin "
        "passed with the following output:\n"
        "${OUTPUT}\n")
    else()
      message(STATUS "Checking for builtin __sync_add_and_fetch -- failed")
      set(ITK_HAVE_SYNC_BUILTINS 0 CACHE INTERNAL "For __sync atomic builtins.")
      file(APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeOutput.log
        "Determining if the C++ compiler supports __sync_add_and_fetch builtin "
        "failed with the following output:\n"
        "${OUTPUT}\n")
    endif()
  endif()
endif()
